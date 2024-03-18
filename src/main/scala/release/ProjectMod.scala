package release

import com.google.common.base.{Stopwatch, Strings}
import com.typesafe.scalalogging.LazyLogging
import release.PomMod.{abbreviate, selectFirstVersionFrom, selfDep, unmanaged}
import release.ProjectMod.{Dep, Gav3, PluginDep, UpdateCon, UpdatePrinter}
import release.Starter.{Opts, OptsDepUp, PreconditionsException}

import java.io.{File, PrintStream}
import java.time.{Duration, LocalDate, Period}
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, Seq}
import scala.collection.parallel.CollectionConverters._
import scala.util.{Failure, Success, Try}

object ProjectMod extends LazyLogging {

  def toDepChangeMap(in: Seq[(ProjectMod.GavWithRef, (Seq[String], Duration))]): Map[Gav3, (Seq[String], Duration)] = {
    in.map(k => (k._1.gav.simpleGav(), k._2)).to(ListMap)
  }

  def toDepChangeSeq(in: Map[Gav3, (Seq[String], Duration)]): Seq[(ProjectMod.GavWithRef, (Seq[String], Duration))] = {
    in.toSeq.map(e => (GavWithRef(SelfRef.ofGav3(e._1), e._1.toGav()), e._2)).sortBy(_.toString())
  }

  def removeOlderVersions(in: Seq[(ProjectMod.GavWithRef, (Seq[String], Duration))]): Seq[(ProjectMod.GavWithRef, (Seq[String], Duration))] = {
    toDepChangeSeq(removeOlderVersions(toDepChangeMap(in)))
  }

  def removeOlderVersions(in: Map[Gav3, (Seq[String], Duration)]): Map[Gav3, (Seq[String], Duration)] = {
    in.map(gavAndVersion => {
      val versionLiterals = gavAndVersion._2._1.map(Version.parseSloppy).sorted
      if (versionLiterals == Nil) {
        (gavAndVersion._1, (Nil, gavAndVersion._2._2)) // TODO remove this, because it is invalid
      } else {
        (gavAndVersion._1, {
          val current = Version.parseSloppy(gavAndVersion._1.version.get)
          (versionLiterals
            .filter(v => Version.ordering.gteq(v, current))
            .filter(_.rawInput != gavAndVersion._1.version.get)
            .map(_.rawInput), gavAndVersion._2._2)
        })
      }
    })
  }

  val knownScopes = Set("provided", "compile", "runtime", "test", "system", "import", "")

  def toUpdats(refs: Seq[(GavWithRef, (Seq[String], Duration))], fx: (Gav3, Seq[String]) => String): Seq[(Gav3, String)] = {
    refs.map(in => {
      (in._1.gav.simpleGav(), fx.apply(in._1.gav.simpleGav(), in._2._1))
    }).distinct
  }

  def rangeFnOf(in: String): (Gav3, Seq[String]) => String = {
    // TODO later
    (a, b) => b.last
  }

  case class GavWithRef(pomRef: SelfRef, gav: Gav)

  def read(workDirFile: File, sys: Term.Sys, opts: Opts, repo: RepoZ, showRead: Boolean = true): ProjectMod = {
    if (PomMod.rootPom(workDirFile).canRead) {
      if (showRead) {
        sys.out.print("I: Reading pom.xmls ..")
      }
      PomMod.withRepo(workDirFile, opts, repo, failureCollector = None)
    } else if (SbtMod.buildSbt(workDirFile).canRead) {
      if (showRead) {
        sys.out.print("I: Reading build.sbt ..")
      }
      SbtMod.withRepo(workDirFile, opts, repo)
    } else {
      throw new PreconditionsException(workDirFile.toString + " is no maven or sbt project")
    }
  }

  def groupSortReleases(o: Seq[String]): Seq[(String, Seq[String])] = {
    o.map(vs => Version.parseSloppy(vs)).groupBy(_.major).toSeq
      .sortBy(_._1)
      .map(e => e.copy(_1 = e._1.toString, _2 = e._2.sorted.map(_.rawInput)))
      .reverse
  }

  def relocatedDeps(relevant: Seq[Dep], repo: RepoZ): Seq[Dep] = {
    relevant.flatMap(dep => {
      val gav = dep.gav().simpleGav()
      val others = relocateGavs(Seq(gav), repo)(gav).filterNot(x => x == gav)
      if (others.nonEmpty) {
        others.map(gav3 => dep.copy(groupId = gav3.groupId, artifactId = gav3.artifactId, version = gav3.version))
      } else {
        None
      }
    })
  }

  def relocateGavs(gavs: Seq[Gav3], repo: RepoZ)(gav: Gav3): Seq[Gav3] = {
    // TODO plugins.sbt - name ?
    val maybeGav = repo.getRelocationOf(gav.groupId, gav.artifactId, gav.version.get)
    if (maybeGav.isDefined) {
      // TODO handle remote relocation
      Seq(gav)
    } else {
      val sGroupId = "org.scala-lang"
      val sArtifactId = "scala-library"
      val s3ArtifactId = "scala3-library_3"
      val sFind = gavs.find(gavk => gavk.groupId == sGroupId &&
        (gavk.artifactId == sArtifactId || gavk.artifactId == s3ArtifactId))
      if (sFind.isDefined) {
        val scalaLib = sFind.get
        val scalaMinor = scalaLib.version.get match {
          case o if o.startsWith("3") => o.replaceFirst("\\.[0-9]+\\.[0-9]+$", "")
          case o => o.replaceFirst("\\.[0-9]+$", "")
        }
        val scalaDepRegex = "^(.*_)[1-9][0-9]*\\.[1-9][0-9]*.*$".r
        if (scalaDepRegex.matches(gav.artifactId)) {
          val newA = scalaDepRegex.replaceAllIn(gav.artifactId, "$1" + scalaMinor)
          Seq(gav, gav.copy(artifactId = newA)).distinct
        } else {
          if (gav.artifactId == sArtifactId && gav.groupId == sGroupId) {
            Seq(gav, gav.copy(artifactId = s3ArtifactId, version = Some("-1")))
          } else {
            Seq(gav)
          }
        }
      } else {
        Seq(gav)
      }
    }

  }

  def isUnwanted(gav: Gav3): Boolean = {
    isUnwantedLiteral(gav.toGav2())(gav.version.get)
  }

  def isUnwantedLiteral(gav: Gav2)(versionLiteral: String): Boolean = {
    versionLiteral.endsWith("-SNAPSHOT") ||
      versionLiteral.contains("patch") ||
      versionLiteral.matches(".*[Mm][0-9]+$") ||
      versionLiteral.matches(".*-[Mm][0-9]+-.*") ||
      versionLiteral.matches(".*-ea-[0-9]+$") || // used by org.immutables
      versionLiteral.matches(".*-rc[0-9]+-.*") || // com.fasterxml.jackson.module:jackson-module-scala_2.13:2.13.0-rc3-preview2
      versionLiteral.matches(".*-rc-[0-9]+$") ||
      versionLiteral.matches(".*-rc\\.[0-9]+$") || // nosqlunit-redis 1.0.0-rc.4, 1.0.0-rc.5
      versionLiteral.matches(".*-rc$") || // org.mariadb.jdbc:mariadb-java-client:3.0.2-rc
      versionLiteral.matches(".*-[0-9a-f]{7}$") || // used by org.typelevel:cats-effect
      versionLiteral.matches(".*-g[0-9a-f]{9}$") || // used by coursier
      versionLiteral.matches(".*-dev$") || // used by commons-discovery:commons-discovery
      versionLiteral.matches(".*pr[0-9]+$") ||
      versionLiteral.contains("alpha") ||
      versionLiteral.contains("Alpha") ||
      versionLiteral.contains("ALPHA") ||
      versionLiteral.contains("BETA") ||
      versionLiteral.contains("Beta") ||
      versionLiteral.contains("beta") ||
      versionLiteral.contains("brew") ||
      versionLiteral.matches(".*b[0-9]+.*") ||
      versionLiteral.matches(".*\\-beta$") ||
      versionLiteral.matches(".*SP[0-9]+$") ||
      versionLiteral.matches(".*-SNAP[0-9]+$") ||
      versionLiteral.matches(".*[(sec|SEC)][0-9]+$") ||
      versionLiteral.endsWith("-incubating") ||
      versionLiteral.endsWith("SONATYPE") ||
      versionLiteral.contains("jbossorg") ||
      versionLiteral.contains("-atlassian-") ||
      versionLiteral.matches(".*jenkins-[0-9]+$") ||
      versionLiteral.contains("PFD") ||
      versionLiteral.matches(".*\\.CR[0-9]+$") || // hibernate-validator
      // .versionLiteral.contains("-cdh") || // Cloudera Distribution Including Apache Hadoop
      versionLiteral.contains("darft") ||
      versionLiteral.startsWith("2003") || // too old
      versionLiteral.startsWith("2004") || // too old
      versionLiteral.endsWith("0.11.0-sshd-314-1") || // org.apache.sshd:sshd-sftp
      versionLiteral.endsWith("-NIGHTLY") || // org.scala-lang:scala3-library_3
      versionLiteral.endsWith("does-not-exist") || // commons-logging:commons-logging:99.0-does-not-exist
      versionLiteral.endsWith("-PUBLISHED-BY-MISTAKE") ||
      versionLiteral.endsWith("230521-nf-execution") || // com.graphql-java:graphql-java
      versionLiteral.matches("^[1-9][0-9]{3}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}.*$") || // com.graphql-java:graphql-java
      (versionLiteral.contains("android") && gav == Gav2(groupId = "com.google.guava", artifactId = "guava"))

  }

  def normalizeUnwantedVersions(gav: Gav3, versionsToNormalize: Seq[String]): Seq[String] = {
    if (versionsToNormalize == null) {
      Nil
    } else {
      val out: Seq[String] = versionsToNormalize
        .filterNot(isUnwantedLiteral(gav.toGav2()))

      val result = if (versionsToNormalize.contains(gav.version.get)) {
        (Seq(gav.version.get) ++ out).distinct
      } else {
        out
      }
      if (result != versionsToNormalize) {
        logger.debug("filtered for " + gav.formatted + ": " + Util.symmetricDiff(result, versionsToNormalize))
      }
      result
    }
  }

  trait Formated {
    def formatted: String
  }

  case class Dep(pomRef: SelfRef, groupId: String, artifactId: String, version: Option[String], typeN: String,
                 scope: String, packaging: String, classifier: String, pomPath: Seq[String]) {
    val gavWithDetailsFormatted: String = Gav.format(Seq(groupId, artifactId, version.getOrElse(""), typeN, scope, packaging, classifier))

    def gav() = Gav(groupId, artifactId, version, packaging, classifier, scope)
  }

  case class PluginExec(id: String, goals: Seq[String], phase: String, config: Map[String, String])

  case class PluginDep(pomRef: SelfRef, groupId: String, artifactId: String, version: Option[String], execs: Seq[PluginExec], pomPath: Seq[String]) {

    def fakeDep() = Dep(pomRef, groupId, artifactId, version, "", "", "", "", pomPath)

    def simpleGav() = Gav3(groupId, artifactId, version)
  }

  case class Gav2(groupId: String, artifactId: String) extends Formated {
    def formatted: String = Gav.format(Seq(groupId, artifactId))
  }

  object Gav3 {
    def opt(groupId: Option[String], artifactId: Option[String], version: Option[String]): Gav3 = {
      Gav3(groupId.getOrElse(""), artifactId.getOrElse(""), version) // XXX blank feels strange
    }
  }

  case class Gav3(groupId: String, artifactId: String, version: Option[String]) extends Formated {
    def toDep(pomRef: SelfRef): Dep = Dep(pomRef, groupId, artifactId, version, "", "", "", "", Nil)

    def toGav(): Gav = Gav(groupId, artifactId, version)

    def toGav2(): Gav2 = Gav2(groupId, artifactId)

    def formatted: String = Gav.format(Seq(groupId, artifactId, version.getOrElse("")))

    def slashedMeta: String = (groupId + '/' + artifactId).replace('.', '/') + "/maven-metadata.xml"
  }

  case class Gav(groupId: String, artifactId: String, version: Option[String], packageing: String = "", classifier: String = "", scope: String = "")
    extends Formated {
    def formatted: String = Gav.format(Seq(groupId, artifactId, version.getOrElse(""), packageing, classifier, scope))

    def simpleGav() = Gav3(groupId, artifactId, version)

    def feelsUnusual(): Boolean = {
      Gav.isUnusualElementValue(groupId) || Gav.isUnusualElementValue(artifactId) ||
        (version.isDefined && (Gav.isUnusualElementValue(version.get) || version.get == "RELEASE" || version.get == "LATEST")) ||
        Gav.isUnusualElementValue(packageing) || Gav.isUnusualElementValue(classifier) || Gav.isUnknownScope(scope)
    }

    def isEmpty(): Boolean = this == Gav.empty

    def copyWithNonEmptyScope(): Gav = {
      if (scope.isBlank) {
        copy(scope = "compile")
      } else {
        this
      }
    }

  }

  object Gav {
    def format(parts: Seq[String]): String = parts.mkString(":").replaceAll("[:]{2,}", ":").replaceFirst(":$", "")

    val empty = Gav(groupId = "", artifactId = "", version = None)

    def isUnusualElementValue(in: String): Boolean = {
      val repl = in.replaceFirst("^[^\\p{Alpha}^\\p{Digit}]", "")
        .replaceFirst("[^\\p{Alpha}^\\p{Digit}]$", "")
      repl != in
    }

    def isUnknownScope(in: String): Boolean = {
      isUnusualElementValue(in) || !knownScopes.contains(in)
    }
  }

  case class SelfRef(id: String, gav3: Gav3) {
    override def toString: String = s"SelfRef(${gav3.toString})"
  }

  object SelfRef {
    val extensions = SelfRef(".mvn/extensions.xml", Gav3("maven", "extension", None))
    val undef = SelfRef("X", Gav3("X", "X", None))

    def ofGav3(gav3: Gav3): SelfRef = SelfRef(gav3.formatted, gav3)

    def ofGav(gav: Gav): SelfRef = SelfRef(gav.formatted, gav.simpleGav())

    @Deprecated
    def parse(id: String): SelfRef = {
      val splited = id.split(":").toSeq
      if (splited.size < 3) {
        throw new IllegalStateException(s"not valid id for ref splitting ${id}")
      }
      SelfRef(id, Gav3(splited.head, splited(1), Some(splited(2))))
    }
  }

  case class Version(pre: String, major: Int, minor: Int, patch: Int, low: String, rawInput: String) {

    lazy val primarys: (Int, Int, Int) = (major, minor, patch)
    lazy val primarysOpt: Option[(Int, Int, Int)] = Some(primarys)
    lazy val isOrdinal: Boolean = {
      val x = primarysOpt.getOrElse((-1, -1, -1))
      x._1 >= 0 && x._2 >= 0 && x._3 >= 0
    }
    lazy val isOrdinalOnly: Boolean = {
      isOrdinal && lowF.replaceFirst("_-SNAPSHOT", "") == "" && pre == ""
    }
    lazy val isSnapshot: Boolean = rawInput.endsWith("-SNAPSHOT")

    def same(major: Int): Boolean = {
      this.major == major
    }

    def same(major: Int, minor: Int): Boolean = {
      same(major) && this.minor == minor
    }

    def same(major: Int, minor: Int, patch: Int): Boolean = {
      same(major, minor) && this.patch == patch
    }

    def same(v: Seq[Int]): Boolean = {
      v.size match {
        case 3 => same(v(0), v(1), v(2))
        case 2 => same(v(0), v(1))
        case 1 => same(v(0))
        case _ => false
      }

    }

    private val lowF:String = if (Util.isNullOrEmpty(low)) {
      ""
    } else {
      "_" + low
    }

    val lowOrdinalPart:Int = {
      Strings.nullToEmpty(low).replaceAll("[^0-9]+", "").toIntOption.getOrElse(Int.MaxValue)
    }

    private val patchF: String = if (patch == 0) {
      ""
    } else {
      "." + patch
    }

    @tailrec
    final def nextIfKnown(known: Seq[Version], ref: Version = this): Version = {
      if (known.contains(ref)) {
        nextIfKnown(known, ref.copy(patch = ref.patch + 1, low = ""))
      } else {
        ref
      }
    }

    def plusWeek(): Version = {
      val addWeek = minor.toInt + 1
      val nextWeek = if (addWeek > 52) {
        1
      } else {
        addWeek
      }
      val nextYear = if (addWeek > 52) {
        major.toInt + 1
      } else {
        major
      }
      copy(minor = nextWeek, major = nextYear)
    }

    def formatShop(): String = {
      pre.toUpperCase() + major + "." + "%02d".format(minor) + patchF + lowF
    }

    def format(): String = {
      pre + major + "." + minor + "." + patch + lowF
    }

    def formatAsSnapshot(): String = {
      pre + major + "." + minor + "." + patch + lowF + "-SNAPSHOT"
    }

    def formatShopAsSnapshot(): String = {
      formatShop() + "-SNAPSHOT"
    }

    def isMajor(): Boolean = {
      minor == 0 && patch == 0
    }
  }

  object Version {
    private[release] val semverPattern = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)$".r
    private[release] val semverPatternNoBugfix = "^([0-9]+)\\.([0-9]+)$".r
    private[release] val semverPatternNoMinor = "^([0-9]+)$".r
    private[release] val semverPatternLowdash = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)_([0-9]+)$".r
    private[release] val semverPatternLowdashString = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)_(.+)$".r
    private[release] val semverPatternRCEnd = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)-((?:RC|M)[1-9][0-9]*)$".r
    private[release] val semverGitTagForDockerTagPattern = "^v[0-9]+\\.[0-9]+\\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$".r
    private[release] val semverPatternLetterEnd = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)-([0-9a-zA-Z\\.]+)$".r
    private[release] val stableShop = "^([0-9]+x)-stable.*$".r
    private[release] val shopPattern = "^(RC-)([1-9][0-9]{3})\\.([0-9][0-9])?(?:\\.([1-9]+[0-9]*))?(?:_([1-9]+[0-9]*))?$".r
    private[release] val betaTagPattern = "^(BETA-)(.+)$".r
    val shopBranchPattern = ("^release/" + shopPattern.regex.substring(1)).r
    private[release] val shopPatternSloppy = "^([Rr][Cc][-\\._])([0-9]{4})[_\\.-]([0-9][0-9]?)?(?:[_\\.-]([0-9]+[0-9]*))?(?:[-_\\.]([0-9]+[0-9]*))?$".r
    private[release] val number = "^([0-9]+)(.*)".r
    private[release] val number2 = "^([0-9]+)\\.([0-9]+)(.*)".r
    private[release] val number3 = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)(.*)".r

    private def ordering1 = Ordering.by[Version, (Int, Int, Int, Int)](e => (e.major, e.minor, e.patch, e.lowOrdinalPart))

    private def ordering2: Ordering[Version] = Ordering.by[Version, (String, String)](e => (e.low, e.pre)).reverse

    private def ordering3: Ordering[Version] = ordering1.orElse(ordering2)

    implicit def ordering: Ordering[Version] = ordering3

    val undef: Version = Version("n/a", -1, -1, -1, "", "")

    private def nullToZero(in: String) = if (in == null || in == "") {
      0
    } else if (in.forall(_.isDigit)) {
      in.toInt
    } else {
      -1
    }

    def fromStringOpt(pre: String, major: String, minor: String, patch: String, low: String, original: String): Option[Version] = {
      Some(fromString(pre, major, minor, patch, low, original))
    }

    def fromString(pre: String, major: String, minor: String, patch: String, low: String, original: String): Version = {
      Version(pre, nullToZero(major), nullToZero(minor), nullToZero(patch), Util.nullToEmpty(low), original)
    }

    def toVersion(m: String): Option[Version] = m match {
      case semverPattern(ma, mi, b) => Version.fromStringOpt("", ma, mi, b, "", m)
      case semverPatternLowdash(ma, mi, b, low) => Version.fromStringOpt("", ma, mi, b, low, m)
      case _ => None
    }

    def parseSloppy(versionText: String): Version = {

      try {
        versionText match {
          case semverPatternLowdash(major, minor, patch, low) => Version.fromString("", major, minor, patch, low, versionText)
          case semverPatternLowdashString(major, minor, patch, low) => Version.fromString("", major, minor, patch, low, versionText)
          case semverPattern(major, minor, patch) => Version.fromString("", major, minor, patch, "", versionText)
          case semverPatternNoBugfix(major, minor) => Version.fromString("", major, minor, "", "", versionText)
          case semverPatternNoMinor(major) => Version.fromString("", major, "", "", "", versionText)
          case shopPattern(pre, year, week, minor, low) => Version.fromString(pre, year, week, minor, low, versionText)
          case number3(major, minor, patch, low) => Version.fromString("", major, minor, patch, low, versionText)
          case number2(major, minor, low) => Version.fromString("", major, minor, "", low, versionText)
          case number(major, low) => Version.fromString("", major, "", "", low, versionText)

          case any => undef.copy(rawInput = any)
        }
      } catch {
        case e: Exception => e.printStackTrace(); undef.copy(rawInput = versionText)
      }
    }

    def parse(versionText: String): Version = {
      val snapped = Term.removeTrailingSnapshots(versionText)

      try {
        snapped match {
          case stableShop(pre) => undef
          case semverPatternLowdash(ma, mi, b, low) => Version.fromString("", ma, mi, b, low, "")
          case semverPatternLowdashString(ma, mi, b, low) => Version.fromString("", ma, mi, b, low, "")
          case semverPattern(ma, mi, b) => Version.fromString("", ma, mi, b, "", "")
          case semverPatternNoBugfix(ma, mi) => Version.fromString("", ma, mi, "", "", "")
          case semverPatternNoMinor(ma) => Version.fromString("", ma, "", "", "", "")
          case shopPattern(pre, year, week, minor, low) => Version.fromString(pre, year, week, minor, low, "")
          case any => undef
        }
      } catch {
        case e: Exception => e.printStackTrace(); undef
      }
    }
  }

  def checkForUpdates(in: Seq[Gav3], depUpOpts: OptsDepUp, repo: RepoZ, updatePrinter: UpdateCon): Map[Gav3, (Seq[String], Duration)] = {

    val statusLine = StatusLine(in.size, updatePrinter.shellWidth, new StatusPrinter() {
      override def print(string: String): Unit = updatePrinter.println(string)

      override def println(): Unit = updatePrinter.println()
    }, updatePrinter.printProgress)
    val updates: Map[Gav3, (Seq[String], Duration)] = in
      .par
      .map(dep => (dep, {
        statusLine.start()
        val newerVersions = if (depUpOpts.filter.isDefined && !depUpOpts.filter.get.matches(dep.formatted)) {
          repo.newerAndPrevVersionsOf(dep.groupId, dep.artifactId, dep.version.get).headOption.toSeq
        } else {
          repo.newerAndPrevVersionsOf(dep.groupId, dep.artifactId, dep.version.get)
        }

        val selectedVersions: Seq[String] = if (depUpOpts.filter.isDefined) {
          val value = newerVersions.map(v => dep.copy(version = Some(v)))
            .filter(gav => depUpOpts.filter.get.matches(gav.formatted))
            .map(_.version.get)
          value
        } else if (depUpOpts.hideStageVersions) {
          ProjectMod.normalizeUnwantedVersions(dep, newerVersions)
        } else {
          newerVersions
        }
        val d = if (depUpOpts.showLibYears && selectedVersions.nonEmpty) {
          val latest: String = selectedVersions.last // TODO version sort
          if (dep != dep.copy(version = Some(latest))) {
            val currentDate = repo.depDate(dep.groupId, dep.artifactId, dep.version.get)
            val latestDate = repo.depDate(dep.groupId, dep.artifactId, latest)
            if (currentDate.isDefined && latestDate.isDefined) {
              logger.trace(s"libyear - ${dep.groupId}:${dep.artifactId}:(${dep.version},${selectedVersions.last}) => ${currentDate.get} to ${latestDate.get}")
              Duration.between(currentDate.get, latestDate.get)
            } else {
              Duration.ofDays(-1)
            }
          } else {
            Duration.ZERO
          }

        } else {
          Duration.ofDays(-2)
        }
        statusLine.end()
        (selectedVersions, d)
      }))
      .seq
      .toMap

    statusLine.finish()
    updates
  }

  trait UpdateCon {
    val simpleChars: Boolean
    val printProgress: Boolean
    val shellWidth: Int

    def println(any: String): Unit

    def println(): Unit

    def printlnErr(any: String): Unit

    def printlnErr(any: Gav3): Unit

    def printlnErr(): Unit
  }

  class StaticPrinter extends UpdateCon {
    val result = new StringBuffer()

    override def println(any: String): Unit = {
      result.append(any)
      println()
    }

    override def println(): Unit = {
      result.append("\n")
    }

    override def printlnErr(any: String): Unit = {
      println(any)
    }

    override def printlnErr(any: Gav3): Unit = {
      println(any.toString)
    }

    override def printlnErr(): Unit = {
      println()
    }

    override val printProgress: Boolean = false
    override val shellWidth: Int = 120
    override val simpleChars: Boolean = false
  }

  class UpdatePrinter(val shellWidth: Int, val termOs: Term, sys: Term.Sys, val printProgress: Boolean) extends UpdateCon {
    override val simpleChars: Boolean = {
      if (!termOs.isCygwin || termOs.isMinGw) {
        if (termOs.simpleChars) {
          true
        } else {
          false
        }
      } else {
        false
      }
    }

    def println(any: String): Unit = {
      sys.out.println(any)
    }

    def println(): Unit = {
      sys.out.println()
    }

    def printlnErr(any: String): Unit = {
      sys.err.println(any)
    }

    def printlnErr(any: Gav3): Unit = {
      sys.err.println(any)
    }

    def printlnErr(): Unit = {
      sys.err.println()
    }
  }

  def collectDependencyUpdates(updatePrinter: UpdateCon, depUpOpts: OptsDepUp,
                               rootDeps: Seq[Dep], selfDepsMod: Seq[Dep], repoDelegator: RepoProxy,
                               checkOnline: Boolean): Seq[(GavWithRef, (Seq[String], Duration))] = {
    if (checkOnline) {
      repoDelegator.repos.foreach(repo => {
        val reachableResult = repo.isReachable(false)
        if (!reachableResult.online) {
          throw new PreconditionsException(repo.workNexusUrl() + " - repo feels offline - " + reachableResult.msg)
        }
      })

    }
    val now = LocalDate.now()
    val stopw = Stopwatch.createStarted()
    updatePrinter.println("I: checking dependencies against nexus - please wait")

    val selfSimple = selfDepsMod.map(_.gav().simpleGav()).distinct
    val relevant: Seq[Dep] = rootDeps
      .filterNot(_.version.get == "")
      .filterNot(in => selfSimple.contains(in.gav().simpleGav()))

    val relevantGav = relevant
      .map(_.gav())
      .distinct

    val emptyVersions = rootDeps
      .filter(_.version.get == "")
      .map(_.gav())
      .distinct

    val prepared = relevantGav.map(_.simpleGav())
      .map(in => {
        if (in.version.isEmpty || in.artifactId.isEmpty || in.groupId.isEmpty) {
          throw new IllegalStateException("gav has empty parts: " + in)
        } else {
          in
        }
      })

    val value: Seq[Gav3] = prepared
      .flatMap(ProjectMod.relocateGavs(prepared, repoDelegator))
    val updates = checkForUpdates(value, depUpOpts, repoDelegator, updatePrinter)

    updatePrinter.println(s"I: checked ${value.size} dependencies in ${stopw.elapsed(TimeUnit.MILLISECONDS)}ms (${now.toString})")

    val checkedUpdates: Map[Gav3, (Seq[String], Duration)] = if (depUpOpts.allowDependencyDowngrades) {
      updates
    } else {
      ProjectMod.removeOlderVersions(updates)
    }

    def change(inp:Map[Gav3, (Seq[String], Duration)]) = {
      (relevant ++ ProjectMod.relocatedDeps(relevant, repoDelegator)).distinct
        .map(in => {
          val ref = GavWithRef(in.pomRef, in.gav())
          val a: (Seq[String], Duration) = inp.getOrElse(in.gav().simpleGav(), (Nil, Duration.ZERO))
          (ref, a)
        })
        .filterNot((in: (GavWithRef, (Seq[String], Duration))) => depUpOpts.hideLatest && in._2._1.isEmpty)
    }

    val allWithUpdate: Seq[(GavWithRef, (Seq[String], Duration))] = change(checkedUpdates)
    val allWithUpdateForPrint: Seq[(GavWithRef, (Seq[String], Duration))] = change(ProjectMod.removeOlderVersions(updates))

    allWithUpdateForPrint.groupBy(_._1.pomRef).foreach(element => {
      val ref: SelfRef = element._1
      val mods: Seq[(GavWithRef, (Seq[String], Duration))] = element._2

      def ch(pretty: String, simple: String) = if (updatePrinter.simpleChars) {
        simple
      } else {
        pretty
      }

      updatePrinter.println(ch("║ ", "| ") + "Project GAV: " + ref.id)
      mods.sortBy(_._1.toString).foreach((subElement: (GavWithRef, (Seq[String], Duration))) => {

        val o: Seq[String] = subElement._2._1

        val majorVersions: Seq[(String, Seq[String])] = ProjectMod.groupSortReleases(o)
        val libyear = if (depUpOpts.showLibYears) {
          val period: Period = Period.between(now, now.plusDays(subElement._2._2.toDays))
          s" (libyears: ${period.getYears}.${period.getMonths} [${subElement._2._2.toDays} days])"
        } else {
          ""
        }
        if (majorVersions != Nil) {
          updatePrinter.println(ch("╠═╦═ ", "+-+- ") + subElement._1.gav.formatted)
        } else {
          updatePrinter.println(ch("╠═══ ", "+--- ") + subElement._1.gav.formatted)
        }

        if (majorVersions.size == 1) {
          updatePrinter.println(ch("║ ╚═══ ", "| +--- ") +
            abbreviate(depUpOpts.versionRangeLimit)(majorVersions.head._2).mkString(", ") + libyear)
        } else {
          if (majorVersions != Nil) {
            majorVersions.tail.reverse.foreach(el => {
              updatePrinter.println(ch("║ ╠═══ ", "| +--- ") + "(" + el._1 + ") " +
                abbreviate(depUpOpts.versionRangeLimit)(el._2).mkString(", "))
            })
            updatePrinter.println(ch("║ ╚═══ ", "| +--- ") + "(" + majorVersions.head._1 + ") " +
              abbreviate(depUpOpts.versionRangeLimit)(majorVersions.head._2).mkString(", ") + libyear)
          }
        }

      })
      updatePrinter.println(ch("║", "|"))
    })

    {
      // TODO check versions before
      val versionNotFound: Map[Gav3, (Seq[String], Duration)] = updates
        .filterNot(t => if (depUpOpts.filter.isEmpty) {
          false
        } else {
          !depUpOpts.filter.get.matches(t._1.formatted)
        })
        .filter(_._2._1 == Nil)
      if (versionNotFound.nonEmpty) {
        // TODO throw new PreconditionsException
        updatePrinter.printlnErr("Non existing dependencies for:\n" +
          versionNotFound.toList.map(in => in._1.formatted + "->" + (in._2._1 match {
            case Nil => "Nil"
            case e => e
          }) + "\n  " + repoDelegator.workNexusUrl() + in._1.slashedMeta).sorted.mkString("\n"))
        updatePrinter.printlnErr()
      }
    }

    {
      val unmangedVersions = unmanaged(emptyVersions, relevantGav)
      if (unmangedVersions != Nil) {
        updatePrinter.printlnErr("Empty or managed versions found:")
        unmangedVersions.map(_.simpleGav()).foreach(updatePrinter.printlnErr)
        updatePrinter.printlnErr()
      }
    }

    if (depUpOpts.showLibYears) {
      // https://libyear.com/
      // https://ericbouwers.github.io/papers/icse15.pdf
      updatePrinter.println()
      val durations = updates
        .filter(t => if (depUpOpts.filter.isEmpty) {
          true
        } else {
          depUpOpts.filter.get.matches(t._1.formatted)
        })
        .map(_._2._2)
      val sum = durations.foldLeft(Duration.ZERO)((a, b) => a.plus(b))
      val period: Period = Period.between(now, now.plusDays(sum.toDays))
      if (durations.exists(_.isNegative)) {
        updatePrinter.println("WARN: negative durations for:")
        updates
          .filter(_._2._2.isNegative)
          .foreach(e => println(s"${e._1} ${e._2._2.toString}"))
      }
      updatePrinter.println(s"libyears: ${period.getYears}.${period.getMonths} (${sum.toDays} days)")
    }
    allWithUpdate
  }

}

sealed case class Increment(t: String)

object Increment {
  val major = Some(Increment("major"))
  val minor = Some(Increment("minor"))
  val patch = Some(Increment("patch"))
}

trait ProjectMod extends LazyLogging {
  def listRemoteRepoUrls(): Seq[String]

  val file: File
  val repo: RepoZ
  val opts: Opts
  val selfVersion: String

  val listDependencies: Seq[Dep]
  val listRawDeps: Seq[Dep]
  val listPluginDependencies: Seq[PluginDep]

  val listProperties: Map[String, String]
  val skipPropertyReplacement: Boolean

  def tryCollectDependencyUpdates(depUpOpts: OptsDepUp, checkOn: Boolean = true, updatePrinter: UpdateCon): Try[Seq[(ProjectMod.GavWithRef, (Seq[String], Duration))]] = {
    try {
      Success(collectDependencyUpdates(depUpOpts, checkOn, updatePrinter))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  def collectDependencyUpdates(depUpOpts: OptsDepUp, checkOn: Boolean = true, updatePrinter: UpdateCon): Seq[(ProjectMod.GavWithRef, (Seq[String], Duration))] = {
    val depForCheck: Seq[Dep] = listGavsForCheck()
    val sdm = selfDepsMod
    val allUrls: Seq[String] = repo.allRepoUrls() ++ listRemoteRepoUrls()
    val proxy = new RepoProxy(repo.createAll(allUrls))
    val result = ProjectMod.collectDependencyUpdates(updatePrinter = updatePrinter,
      depUpOpts = depUpOpts, rootDeps = depForCheck, selfDepsMod = sdm, repoDelegator = proxy, checkOnline = checkOn)
    if (depUpOpts.changeToLatest) {
      val localDepUpFile = new File(file, ".release-dependency-updates")
      val fn: (Gav3, Seq[String]) => String = if (localDepUpFile.canRead) {
        ProjectMod.rangeFnOf(Util.read(localDepUpFile))
      } else {
        (_, b) => b.last
      }
      changeDependecyVersion(ProjectMod.toUpdats(result, fn))
      writeTo(file)
    }
    result
  }

  private[release] def listDeps(): Seq[ProjectMod.Dep] = {
    val fromDeps = PomMod.replacedVersionProperties(listProperties, skipPropertyReplacement)(listDependencies)
    val fromPlugins = listPluginDependencies.map(_.fakeDep())
      .map(in => in.groupId match {
        case "" => in.copy(groupId = "org.apache.maven.plugins")
        case _ => in
      })
    (fromDeps ++ fromPlugins).filterNot(d => d.gav().isEmpty())
  }

  private[release] def listGavs(): Seq[ProjectMod.Gav] = {
    listDeps().map(_.gav()).distinct
  }

  private[release] def listGavsForCheck(): Seq[Dep] = {
    val selfGavs = selfDepsMod.map(_.gav())
    listDeps()
      .filterNot(_.version.isEmpty) // managed plugins are okay
      .filterNot(dep => selfGavs.contains(dep.gav()))
      .distinctBy(_.gav().simpleGav())
  }

  def isNoShop: Boolean = {
    !isShop
  }

  def isShop: Boolean

  def selfDepsModGavs(): Seq[Gav3] = {
    selfDepsMod.map(_.gav().simpleGav().copy(version = None)).distinct
  }

  val selfDepsMod: Seq[Dep]

  def getSelfDepsMod: Seq[Dep] = selfDepsMod

  def suggestReleaseVersion(branchNames: Seq[String] = Nil, tagNames: Seq[String] = Nil, increment: Option[Increment] = None): Seq[String]

  def suggestNextRelease(releaseVersion: String): String

  def listSnapshotDependenciesDistinct: Seq[Dep]

  def writeTo(targetFolder: File): Unit

  def changeVersion(newVersion: String): Unit

  def changeDependecyVersion(patch: Seq[(Gav3, String)]): Unit

  def depTreeFilenameList(): Seq[String]

  def listGavsWithUnusualScope(): Seq[ProjectMod.Gav] = {
    val gavs = listGavs()
    val unusualGavs = gavs.filter(_.feelsUnusual())
    unusualGavs
  }
}
