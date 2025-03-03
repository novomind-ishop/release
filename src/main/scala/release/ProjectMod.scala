package release

import com.google.common.base.Stopwatch
import com.typesafe.scalalogging.LazyLogging
import release.PomMod.{DepTree, abbreviate, unmanaged}
import release.ProjectMod.{Dep, Gav3, PluginDep, UpdateCon}
import release.Starter.{PreconditionsException}

import java.io.File
import java.nio.file.Path
import java.time.{Duration, LocalDate, Period, ZonedDateTime}
import java.util.concurrent.TimeUnit
import scala.collection.immutable.{ListMap, Seq}
import scala.collection.parallel.CollectionConverters._
import scala.util.{Failure, Success, Try}

object ProjectMod extends LazyLogging {

  def listGavsWithUnusualScope(gavs: Seq[ProjectMod.Gav]): Seq[(ProjectMod.Gav, String)] = {
    val unusualGavs = gavs.filter(_.feelsUnusual())
    unusualGavs.map(g => (g, Gav.selectUnusualReason(g)))
  }

  def findOrphanTrees(root: File, knownTrees: Seq[File]): Seq[Path] = {
    val allTrees: Seq[Path] = FileUtils.walk(root).par.filter(p => p.endsWith("dep.tree")).seq.toSeq.map(_.toAbsolutePath.normalize())
    val value: Seq[Path] = knownTrees.map(_.toPath.toAbsolutePath.normalize())
    allTrees.diff(value)
  }

  def toDepChangeMap(in: Seq[(ProjectMod.GavWithRef, Seq[(String, Try[ZonedDateTime])])]): Map[Gav3, Seq[(String, Try[ZonedDateTime])]] = {
    in.map(k => (k._1.gav.simpleGav(), k._2)).to(ListMap)
  }

  def toDepChangeSeq(in: Map[Gav3, Seq[(String, Try[ZonedDateTime])]]): Seq[(ProjectMod.GavWithRef, Seq[(String, Try[ZonedDateTime])])] = {
    in.toSeq.map(e => (GavWithRef(SelfRef.ofGav3(e._1), e._1.toGav()), e._2)).sortBy(_.toString())
  }

  def removeOlderVersions(in: Seq[(ProjectMod.GavWithRef, Seq[(String, Try[ZonedDateTime])])]): Seq[(ProjectMod.GavWithRef, Seq[(String, Try[ZonedDateTime])])] = {
    toDepChangeSeq(removeOlderVersions(toDepChangeMap(in)))
  }

  def removeOlderVersions(in: Map[Gav3, Seq[(String, Try[ZonedDateTime])]]): Map[Gav3, Seq[(String, Try[ZonedDateTime])]] = {
    in.map(gavAndVersion => {
      val versionLiterals: (Gav3, Seq[(Version, Try[ZonedDateTime])]) = (gavAndVersion._1,
        gavAndVersion._2.map(e => (Version.parseSloppy(e._1), e._2)).sortBy(_._1))
      if (versionLiterals._2 == Nil) {
        (gavAndVersion._1, Nil)
      } else {
        (gavAndVersion._1, {
          val current = Version.parseSloppy(gavAndVersion._1.version.get)
          val value = versionLiterals._2
            .filter(v => Version.ordering.gteq(v._1, current))
            .map(e => (e._1.rawInput, e._2))
          if (Seq(current.rawInput) == value.map(_._1)) {
            Nil
          } else {
            value
          }
        })
      }
    })
  }

  val knownScopes = Set("provided", "compile", "runtime", "test", "system", "import", "")
  private val sGroupId = "org.scala-lang"
  private val sArtifactId = "scala-library"
  private val s3ArtifactId = "scala3-library_3"

  def toUpdats(refs: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])], fx: (Gav3, Seq[String]) => String): Seq[(Gav3, String)] = {
    refs.map(in => {
      (in._1.gav.simpleGav(), fx.apply(in._1.gav.simpleGav(), in._2.map(_._1)))
    }).distinct
  }

  def rangeFnOf(in: String): (Gav3, Seq[String]) => String = {
    // TODO later
    (a, b) => b.last
  }

  def libyear(showYear: Boolean, currentGav: Gav3, major: Option[String], versionTimestamps: Seq[(String, Try[ZonedDateTime])],
              yearsFn: ((Gav3, Option[Int], Duration)) => Unit): String = {
    try {
      if (showYear) {
        val workStamps = versionTimestamps.sortBy(k => Version.parseSloppy(k._1))
        if (workStamps.size > 1) {
          val currentVersionWithTimestamp = workStamps.find(s => s._1 == currentGav.version.get)
          if (currentVersionWithTimestamp.isDefined) {
            val majorInt = major.flatMap(_.toIntOption)
            val lastSelection = if (major.isDefined) {
              val r = workStamps.filter(v => {
                Version.parseSloppy(v._1).major == majorInt.getOrElse(-1)
              }).lastOption
              if (r.nonEmpty) {
                r
              } else {
                workStamps.lastOption
              }

            } else {
              workStamps.lastOption
            }
            if (currentVersionWithTimestamp.isDefined && currentVersionWithTimestamp.get._2.isSuccess) {
              val dur = Duration.between(currentVersionWithTimestamp.get._2.get, lastSelection.get._2.get)

              yearsFn.apply((currentGav, majorInt, dur))
              val period: Period = Util.toPeriod(dur)
              s" (libyears: ${period.getYears}Y ${period.getMonths}M [${dur.toDays} days])"
            } else {
              " (libyears: ?????)"
            }

          } else {
            " (libyears: ????)"
          }

        } else {
          " (libyears: ???)"
        }

      } else {
        ""
      }
    } catch {
      case e: Exception => s" (libyears: ?? ${e.getMessage})"
    }
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
    // [INFO] --- dependency:3.3.0:tree (pre-build-validate-tree) ---
    // [WARNING] The artifact com.atlassian.commonmark:commonmark:jar:0.17.0 has been relocated to org.commonmark:commonmark:jar:0.17.0
    val relocationResult = repo.getRelocationOf(gav.groupId, gav.artifactId, gav.version.get)
    if (relocationResult.isDefined) {
      // TODO handle remote relocation
      Seq(gav)
    } else {
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

  def onlySelfAndRangeLast(self: Gav3, versionsToNormalize: Seq[String]): Seq[String] = {
    val sorted = versionsToNormalize.map(Version.parseSloppy)
    val g: Seq[String] = sorted.groupBy(_.primarysOpt.map(_._1)).flatMap(t => t._2.lastOption)
      .filter(v => v.isGreaterEqualsOpt(self.versionSloppy()))
      .map(_.rawInput).toSeq ++ self.version.toSeq

    versionsToNormalize.filter(g.contains)
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

    def versionSloppy(): Option[Version] = version.map(Version.parseSloppy)

    def formatted: String = Gav.format(Seq(groupId, artifactId, version.getOrElse("")))

    def slashedMeta: String = (groupId + '/' + artifactId).replace('.', '/') + "/maven-metadata.xml"
  }

  case class Gav(groupId: String, artifactId: String, version: Option[String], packageing: String = "", classifier: String = "", scope: String = "")
    extends Formated {
    def formatted: String = Gav.format(Seq(groupId, artifactId, version.getOrElse(""), packageing, classifier, scope))

    def simpleGav(): Gav3 = Gav3(groupId, artifactId, version)

    def feelsUnusual(): Boolean = {
      Gav.feelsUnusual(this)
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
    def selectUnusualReason(gav: Gav): String = {
      def rpluk(s:String):String = {
        s.replaceAll("[\\p{C}]", "\u2423")
      }
      if (isUnknownScope(gav.scope)) {
        s"uses unknown scope »${rpluk(gav.scope)}« please use only known scopes: ${ProjectMod.knownScopes.filterNot(_.isEmpty).toSeq.sorted.mkString(", ")}."
      } else if (gav.version.isDefined && Gav.isUnusualElementValue(gav.version.get) && Gav.isContainsRange(gav.version.get)) {
        s"uses version with range »${rpluk(gav.version.get)}«. Please only use a concrete version."
      } else if (gav.version.isDefined && Gav.isUnusualElementValue(gav.version.get)) {
        s"uses version with unknown symbol »${rpluk(gav.version.get)}«. Please remove unknown symbols."
      } else if (gav.version.isDefined && (gav.version.get == "RELEASE" || gav.version.get == "LATEST")) {
        s"uses version »${rpluk(gav.version.get)}« that is part of unstable markers LATEST and RELEASE. Please only use a concrete version."
      } else if (Gav.isUnusualElementValue(gav.artifactId)) {
        s"uses artifactId with unknown symbol »${rpluk(gav.artifactId)}«. Please remove unknown symbols."
      } else if (Gav.isUnusualElementValue(gav.groupId)) {
        s"uses groupId with unknown symbol »${rpluk(gav.groupId)}«. Please remove unknown symbols."
      } else if (Gav.isUnusualElementValue(gav.packageing)) {
        s"uses packageing with unknown symbol »${rpluk(gav.packageing)}«. Please remove unknown symbols."
      } else {
        "uses unusual format"
      }
    }

    def feelsUnusual(gav: Gav) = {
      Gav.isUnusualElementValue(gav.groupId) || Gav.isUnusualElementValue(gav.artifactId) ||
        (gav.version.isDefined && (Gav.isUnusualElementValue(gav.version.get) || gav.version.get == "RELEASE" || gav.version.get == "LATEST")) ||
        Gav.isUnusualElementValue(gav.packageing) || Gav.isUnusualElementValue(gav.classifier) || Gav.isUnknownScope(gav.scope)
    }

    def format(parts: Seq[String]): String = parts.mkString(":").replaceAll("[:]{2,}", ":").replaceFirst(":$", "")

    val empty = Gav(groupId = "", artifactId = "", version = None)

    def isContainsRange(in: String): Boolean = {
      val rangeSet = "[]()".toSet
      val inSet = in.toSet
      inSet.diff(rangeSet) != inSet
    }

    def isUnusualElementValue(in: String): Boolean = {
      val repl = in
        .replaceAll("[\\p{C}\\s]", "\u2423")
        .replaceFirst("^[^\\p{Alpha}^\\p{Digit}]", "")
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
    val virtualParent = SelfRef("virtual-group-of-all-parents", Gav3("maven", "parent", None))
    val extensions = SelfRef(".mvn/extensions.xml", Gav3("maven", "extension", None))
    val undef = SelfRef("X", Gav3("X", "X", None))

    def ofGav3(gav3: Gav3): SelfRef = SelfRef(gav3.formatted, gav3)

    def ofGav(gav: Gav): SelfRef = SelfRef(gav.formatted, gav.simpleGav())

    @deprecated(message = "do not use this", since = "2024-01-01")
    def parse(id: String): SelfRef = {
      val parts = id.split(":").toSeq
      if (parts.size < 3) {
        throw new IllegalStateException(s"not valid id for ref splitting ${id}")
      }
      SelfRef(id, Gav3(parts.head, parts(1), Some(parts(2))))
    }
  }

  def checkForUpdates(in: Seq[Gav3], depUpOpts: OptsDepUp, repo: RepoZ, updatePrinter: UpdateCon, ws: String): Map[Gav3, Seq[(String, Try[ZonedDateTime])]] = {

    val statusLine = StatusLine(in.size, updatePrinter.shellWidth, new StatusPrinter() {
      override def print(string: String): Unit = updatePrinter.println(string)

      override def println(): Unit = updatePrinter.println(ws)
    }, updatePrinter.printProgress)
    val updates: Map[Gav3, Seq[(String, Try[ZonedDateTime])]] = in
      .par
      .map(dep => (dep, {
        statusLine.start()
        val newerVersions = if (depUpOpts.hideUpdates) {
          Nil
        } else {
          if (depUpOpts.filter.isDefined && !depUpOpts.filter.get.matches(dep.formatted)) {
            repo.newerAndPrevVersionsOf(dep.groupId, dep.artifactId, dep.version.get).headOption.toSeq
          } else {
            repo.newerAndPrevVersionsOf(dep.groupId, dep.artifactId, dep.version.get)
          }
        }

        val selectedVersions: Seq[String] = if (depUpOpts.filter.isDefined) {
          val value = newerVersions.map(v => dep.copy(version = Some(v)))
            .filter(gav => depUpOpts.filter.get.matches(gav.formatted))
            .map(_.version.get)
          value
        } else if (depUpOpts.hideUpdates) {
          Seq(dep.version.get)
        } else if (depUpOpts.hideStageVersions) {
          ProjectMod.normalizeUnwantedVersions(dep, newerVersions)
        } else {
          newerVersions
        }

        val reduced = ProjectMod.onlySelfAndRangeLast(dep.toGav().simpleGav(), selectedVersions)

        def selectReleaseDate(gav2: Gav2, v: String): Try[ZonedDateTime] = {
          if (depUpOpts.showLibYears && reduced.nonEmpty && reduced.contains(v)) {
            val latestDate = repo.depDate(gav2.groupId, gav2.artifactId, v)
            if (latestDate.isDefined) {
              Success(latestDate.get)
            } else {
              Failure(new UnsupportedOperationException(s"no date found for ${gav2.formatted}:${v}"))
            }
          } else {
            Failure(new UnsupportedOperationException(s"libyears are not computed for ${gav2.formatted}:${v}"))
          }

        }

        statusLine.end()
        val x: Seq[(String, Try[ZonedDateTime])] = selectedVersions
          .par
          .map(version => (version, selectReleaseDate(dep.toGav2(), version)))
          .seq
        x
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

    def printlnErr(any: String): Unit

    def printlnErr(any: Gav3): Unit
  }

  class StaticPrinter extends UpdateCon {
    val result = new StringBuffer()

    override def println(any: String): Unit = {
      result.append(any).append("\n")
    }

    override def printlnErr(any: String): Unit = {
      println(any)
    }

    override def printlnErr(any: Gav3): Unit = {
      println(any.toString)
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
                               checkOnline: Boolean, ws: String): Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])] = {
    if (checkOnline) {
      repoDelegator.repos.foreach(repo => {
        val reachableResult = repo.isReachable(false)
        if (!reachableResult.online) {
          throw new PreconditionsException(repo.workNexusUrl() + " - repo feels offline - " + reachableResult.msg)
        }
      })

    }
    var years: Seq[(Gav3, Option[Int], Duration)] = Nil
    val stopw = Stopwatch.createStarted()
    updatePrinter.println("I: checking dependencies against binary repository - please wait")

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

    val value: Seq[Gav3] = prepared.flatMap(ProjectMod.relocateGavs(prepared, repoDelegator))
    val updates = checkForUpdates(value, depUpOpts, repoDelegator, updatePrinter, ws = ws)
    val now = LocalDate.now()
    updatePrinter.println(s"I: checked ${value.size} dependencies in ${stopw.toString} (${now.toString})")

    val checkedUpdates: Map[Gav3, Seq[(String, Try[ZonedDateTime])]] = if (depUpOpts.allowDependencyDowngrades) {
      updates
    } else {
      ProjectMod.removeOlderVersions(updates)
    }

    def change(inp: Map[Gav3, Seq[(String, Try[ZonedDateTime])]]) = {
      (relevant ++ ProjectMod.relocatedDeps(relevant, repoDelegator)).distinct
        .map(in => {
          val ref = GavWithRef(in.pomRef, in.gav())
          val a: Seq[(String, Try[ZonedDateTime])] = inp.getOrElse(in.gav().simpleGav(), Nil)
          (ref, a)
        })
        .filterNot((in: (GavWithRef, Seq[(String, Try[ZonedDateTime])])) => depUpOpts.hideLatest && in._2.isEmpty)
    }

    val allWithUpdate: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])] = change(checkedUpdates)
    val allWithUpdateForPrint: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])] = change(ProjectMod.removeOlderVersions(updates))

    allWithUpdateForPrint.groupBy(_._1.pomRef).foreach(element => {
      val ref: SelfRef = element._1
      val mods: Seq[(GavWithRef, Seq[(String, Try[ZonedDateTime])])] = element._2

      def ch(pretty: String, simple: String) = if (updatePrinter.simpleChars) {
        simple
      } else {
        pretty
      }

      updatePrinter.println(ch("║ ", "| ") + "Project GAV: " + ref.id)
      mods.sortBy(_._1.toString).foreach((subElement: (GavWithRef, Seq[(String, Try[ZonedDateTime])])) => {

        val o: Seq[String] = subElement._2.map(_._1)

        val majorVersions: Seq[(String, Seq[String])] = ProjectMod.groupSortReleases(o)

        if (majorVersions != Nil) {
          updatePrinter.println(ch("╠═╦═ ", "+-+- ") + subElement._1.gav.formatted)
        } else {
          updatePrinter.println(ch("╠═══ ", "+--- ") + subElement._1.gav.formatted)
        }

        val majorVersionWithoutSelf = majorVersions.map(e => {
          (e._1, e._2.filterNot(_ == subElement._1.gav.version.get))
        }).filterNot(_._2.isEmpty)
        if (majorVersionWithoutSelf.size == 1) {

          val year = libyear(depUpOpts.showLibYears, subElement._1.gav.simpleGav(), None, subElement._2,
            yearsFn = y => years = years :+ y)
          updatePrinter.println(ch("║ ╚═══ ", "| +--- ") +
            abbreviate(depUpOpts.versionRangeLimit)(majorVersionWithoutSelf.head._2).mkString(", ") + year)
        } else {
          if (majorVersionWithoutSelf != Nil) {
            majorVersionWithoutSelf.tail.reverse.foreach(el => {
              val year = libyear(depUpOpts.showLibYears, subElement._1.gav.simpleGav(), Some(el._1), subElement._2,
                yearsFn = y => years = years :+ y)
              updatePrinter.println(ch("║ ╠═══ ", "| +--- ") + "(" + el._1 + ") " +
                abbreviate(depUpOpts.versionRangeLimit)(el._2).mkString(", ") + year)
            })
            val year = libyear(depUpOpts.showLibYears, subElement._1.gav.simpleGav(), Some(majorVersionWithoutSelf.head._1), subElement._2,
              yearsFn = y => years = years :+ y)
            updatePrinter.println(ch("║ ╚═══ ", "| +--- ") + "(" + majorVersionWithoutSelf.head._1 + ") " +
              abbreviate(depUpOpts.versionRangeLimit)(majorVersionWithoutSelf.head._2).mkString(", ") + year)
          }
        }

      })
      updatePrinter.println(ch("║", "|"))
    })

    // TODO check versions before
    val versionNotFound: Map[Gav3, Seq[(String, Try[ZonedDateTime])]] = updates
      .filterNot(t => if (depUpOpts.filter.isEmpty) {
        false
      } else {
        !depUpOpts.filter.get.matches(t._1.formatted)
      })
      .filter(_._2 == Nil)
    if (versionNotFound.nonEmpty) {
      // TODO throw new PreconditionsException
      updatePrinter.printlnErr("Non existing dependencies for:\n" +
        versionNotFound.toList.map(in => s"»${in._1.formatted}« -> " + (in._2 match {
          case Nil => "Nil"
          case e => e
        }) + "\n  " + repoDelegator.workNexusUrl() + in._1.slashedMeta).sorted.mkString("\n"))
      updatePrinter.printlnErr(ws)
    }

    val unmangedVersions = unmanaged(emptyVersions, relevantGav)
    if (unmangedVersions != Nil) {
      updatePrinter.printlnErr("Empty or managed versions found:")
      unmangedVersions.map(_.simpleGav()).foreach(updatePrinter.printlnErr)
      updatePrinter.printlnErr(ws)
    }

    if (depUpOpts.showLibYears) {
      // https://libyear.com/
      // https://ericbouwers.github.io/papers/icse15.pdf
      updatePrinter.println(ws)
      val workYears = years.distinct
      val fi = workYears
        .groupBy(_._1)
        .flatMap(k => {
          val ident = k._2
          if (ident.size > 1) {
            ident.filterNot(e => e._2.getOrElse(0) <= k._1.versionSloppy().map(_.major).getOrElse(0))
          } else {
            ident
          }
        })
      val sum = fi
        .map(_._3).foldLeft(Duration.ZERO)((a, b) => a.plus(b))

      if (workYears.exists(_._3.isNegative)) {
        updatePrinter.println("WARN: negative durations for:")
        workYears
          .filter(se => {
            se._3.isNegative
          })
          .foreach(e => updatePrinter.println(s"NEGATIVE: ${e._1.formatted} ${e._2.toString}"))
      }
      val period = Util.toPeriod(sum)
      updatePrinter.println(s"Σ libyears: ${period.getYears}Y ${period.getMonths}M (${sum.toDays} days)")
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

  def getDepTreeFileContents: Map[File, DepTree]

  val file: File
  lazy val depInFiles: Seq[(Dep, File)] = throw new NotImplementedError()
  lazy val repo: RepoZ = throw new NotImplementedError()
  val opts: Opts
  val selfVersion: String

  val listDependencies: Seq[Dep]
  val listRawDeps: Seq[Dep]
  val listPluginDependencies: Seq[PluginDep]

  val listProperties: Map[String, String]
  val skipPropertyReplacement: Boolean

  def tryCollectDependencyUpdates(depUpOpts: OptsDepUp, checkOn: Boolean = true, updatePrinter: UpdateCon, ws: String):
  Try[(Seq[(ProjectMod.GavWithRef, Seq[(String, Try[ZonedDateTime])])], RepoProxy)] = {
    Util.timeout(depUpOpts.timeoutSec.getOrElse(90), TimeUnit.SECONDS, _ => {
      try {
        Success(collectDependencyUpdates(depUpOpts, checkOn, updatePrinter, ws))
      } catch {
        case e: Exception => {
          Failure(e)
        }
      }
    }, (e, _) => Failure(e))._1

  }

  def collectDependencyUpdates(depUpOpts: OptsDepUp, checkOn: Boolean = true, updatePrinter: UpdateCon, ws: String):
  (Seq[(ProjectMod.GavWithRef, Seq[(String, Try[ZonedDateTime])])], RepoProxy) = {
    val depForCheck: Seq[Dep] = listGavsForCheck()
    val sdm = selfDepsMod

    val proxy = RepoProxy(repo.allRepoZ() ++ repo.createAll(listRemoteRepoUrls()))
    val allRepoUrls = proxy.repos.flatMap(_.allRepoUrls()).distinct
    if (allRepoUrls.nonEmpty) {
      updatePrinter.println("... " + allRepoUrls.mkString(", "))
    }
    val result = ProjectMod.collectDependencyUpdates(updatePrinter = updatePrinter,
      depUpOpts = depUpOpts, rootDeps = depForCheck, selfDepsMod = sdm, repoDelegator = proxy, checkOnline = checkOn,
      ws = ws)
    if (depUpOpts.changeToLatest) {
      val localDepUpFile = new File(file, ".release-dependency-updates")
      val fn: (Gav3, Seq[String]) => String = if (localDepUpFile.canRead) {
        ProjectMod.rangeFnOf(FileUtils.read(localDepUpFile))
      } else {
        (_, b) => b.last
      }
      changeDependecyVersion(ProjectMod.toUpdats(result, fn))
      writeTo(file)
    }
    (result, proxy)
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

  def listGavsWithUnusualScope(): Seq[(ProjectMod.Gav, String)] = {
    ProjectMod.listGavsWithUnusualScope(gavs = listGavs())
  }
}
