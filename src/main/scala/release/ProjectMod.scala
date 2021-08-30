package release

import com.google.common.base.Stopwatch
import com.typesafe.scalalogging.LazyLogging
import release.PomMod.{abbreviate, unmanged}
import release.ProjectMod.{Dep, Gav3, PluginDep}
import release.Starter.{Opts, OptsDepUp, PreconditionsException}

import java.io.{File, PrintStream}
import java.time.{Duration, LocalDate, Period}
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object ProjectMod extends LazyLogging {

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

  def read(workDirFile: File, out: PrintStream, opts: Opts, repo: Repo, showRead: Boolean = true): ProjectMod = {
    if (PomMod.rootPom(workDirFile).canRead) {
      if (showRead) {
        out.print("I: Reading pom.xmls ..")
      }
      PomMod.withRepo(workDirFile, opts, repo)
    } else if (SbtMod.buildSbt(workDirFile).canRead) {
      if (showRead) {
        out.print("I: Reading build.sbt ..")
      }
      SbtMod.withRepo(workDirFile, opts, repo)
    } else {
      throw new PreconditionsException(workDirFile.toString + " is no maven or sbt project")
    }
  }

  def groupSortReleases(o: Seq[String]): Seq[(String, Seq[String])] = {
    o.map(vs => Version.parseSloppy(vs)).groupBy(_.major).toSeq
      .sortBy(_._1)
      .map(e => e.copy(_1 = e._1.toString, _2 = e._2.sorted.map(_.orginal)))
      .reverse
  }

  def scalaDeps(gavs: Seq[Gav3])(gav: Gav3): Seq[Gav3] = {
    // TODO plugins.sbt - name ?
    val sGroupId = "org.scala-lang"
    val sArtifactId = "scala-library"
    val s3ArtifactId = "scala3-library_3"
    val sFind = gavs.find(gavk => gavk.groupId == sGroupId &&
      (gavk.artifactId == sArtifactId || gavk.artifactId == s3ArtifactId))
    if (sFind.isDefined) {
      val scalaLib = sFind.get
      val scalaMinor = scalaLib.version match {
        case o if o.startsWith("3") => o.replaceFirst("\\.[0-9]+\\.[0-9]+$", "")
        case o => o.replaceFirst("\\.[0-9]+$", "")
      }
      val scalaDepRegex = "^(.*_)[1-9][0-9]*\\.[1-9][0-9]*.*$".r
      if (scalaDepRegex.matches(gav.artifactId)) {
        val newA = scalaDepRegex.replaceAllIn(gav.artifactId, "$1" + scalaMinor)
        Seq(gav, gav.copy(artifactId = newA)).distinct
      } else {
        Seq(gav)
      }
    } else {
      Seq(gav)
    }
  }

  def normalizeUnwantedVersions(gav: Gav3, inVersions: Seq[String]): Seq[String] = {
    val out: Seq[String] = inVersions.filterNot(_.endsWith("-SNAPSHOT"))
      .filterNot(_.contains("patch"))
      .filterNot(_.matches(".*[Mm][0-9]+$"))
      .filterNot(_.matches(".*-[Mm][0-9]+-.*"))
      .filterNot(_.matches(".*-ea-[0-9]+$")) // used by org.immutables
      .filterNot(_.matches(".*-rc-[0-9]+$"))
      .filterNot(_.matches(".*-rc\\.[0-9]+$")) // nosqlunit-redis 1.0.0-rc.4, 1.0.0-rc.5
      .filterNot(_.matches(".*-[0-9a-f]{7}$")) // used by org.typelevel:cats-effect
      .filterNot(_.matches(".*-dev$")) // used by commons-discovery:commons-discovery
      .filterNot(_.matches(".*pr[0-9]+$"))
      .filterNot(_.contains("alpha"))
      .filterNot(_.contains("Alpha"))
      .filterNot(_.contains("ALPHA"))
      .filterNot(_.contains("BETA"))
      .filterNot(_.contains("Beta"))
      .filterNot(_.contains("beta"))
      .filterNot(_.contains("brew"))
      .filterNot(_.matches(".*b[0-9]+.*"))
      .filterNot(_.matches(".*\\-beta$"))
      .filterNot(_.matches(".*SP[0-9]+$"))
      .filterNot(_.matches(".*-SNAP[0-9]+$"))
      .filterNot(_.matches(".*[(sec|SEC)][0-9]+$"))
      .filterNot(_.endsWith("-incubating"))
      .filterNot(_.endsWith("SONATYPE"))
      .filterNot(_.contains("jbossorg"))
      .filterNot(_.contains("-atlassian-"))
      .filterNot(_.matches(".*jenkins-[0-9]+$"))
      .filterNot(_.contains("PFD"))
      .filterNot(_.matches(".*\\.CR[0-9]+$")) // hibernate-validator
      // .filterNot(_.contains("-cdh")) // Cloudera Distribution Including Apache Hadoop
      .filterNot(_.contains("darft"))
      .filterNot(_.startsWith("2003"))
      .filterNot(_.startsWith("2004"))
      .filterNot(_.endsWith("0.11.0-sshd-314-1")) // org.apache.sshd:sshd-sftp
      .filterNot(_.endsWith("-NIGHTLY")) // org.scala-lang:scala3-library_3
      .filterNot(_.endsWith("-PUBLISHED-BY-MISTAKE"))

    val result = if (inVersions.contains(gav.version)) {
      (Seq(gav.version) ++ out).distinct
    } else {
      out
    }
    if (result != inVersions) {
      logger.debug("filtered for " + gav.formatted + ": " + Util.symmetricDiff(result, inVersions))
    }
    result
  }


  case class Dep(pomRef: SelfRef, groupId: String, artifactId: String, version: String, typeN: String,
                 scope: String, packaging: String, classifier: String) {
    val gavWithDetailsFormatted: String = Gav.format(Seq(groupId, artifactId, version, typeN, scope, packaging, classifier))

    def gav() = Gav(groupId, artifactId, version, packaging, classifier, scope)
  }

  case class PluginExec(id: String, goals: Seq[String], phase: String, config: Map[String, String])

  case class PluginDep(pomRef: SelfRef, groupId: String, artifactId: String, version: String, execs: Seq[PluginExec], pomPath: Seq[String]) {

    def fakeDep() = Dep(pomRef, groupId, artifactId, version, "", "", "", "")

    def simpleGav() = Gav3(groupId, artifactId, version)
  }

  case class Gav3(groupId: String, artifactId: String, version: String) {
    def formatted: String = Gav.format(Seq(groupId, artifactId, version))

    def slashedMeta: String = (groupId + '/' + artifactId).replace('.', '/') + "/maven-metadata.xml"
  }

  case class Gav(groupId: String, artifactId: String, version: String, packageing: String = "", classifier: String = "", scope: String = "") {
    def formatted: String = Gav.format(Seq(groupId, artifactId, version, packageing, classifier, scope))

    def simpleGav() = Gav3(groupId, artifactId, version)

    def feelsUnusual(): Boolean = {
      Gav.isUnu(groupId) || Gav.isUnu(artifactId) || Gav.isUnu(version) ||
        Gav.isUnu(packageing) || Gav.isUnu(classifier) || Gav.isUnu(scope)
    }

  }

  object Gav {
    def format(parts: Seq[String]): String = parts.mkString(":").replaceAll("[:]{2,}", ":").replaceFirst(":$", "")

    val empty = Gav(groupId = "", artifactId = "", version = "")

    def isUnu(in: String): Boolean = {
      val repl = in.replaceFirst("^[^\\p{Alpha}^\\p{Digit}]", "")
        .replaceFirst("[^\\p{Alpha}^\\p{Digit}]$", "")
      repl != in
    }
  }

  case class SelfRef(id: String)

  object SelfRef {
    val undef = SelfRef("X")
  }

  case class Version(pre: String, major: Int, minor: Int, patch: Int, low: String, orginal: String) {

    private val lowF = if (Util.isNullOrEmpty(low)) {
      ""
    } else {
      "_" + low
    }

    private val patchF = if (patch == 0) {
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
      pre + major + "." + "%02d".format(minor) + patchF + lowF
    }

    def formatAsSnapshot(): String = {
      pre + major + "." + minor + "." + patch + lowF + "-SNAPSHOT"
    }

    def formatShopAsSnapshot(): String = {
      formatShop() + "-SNAPSHOT"
    }
  }

  object Version {
    private[release] val semverPattern = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)$".r
    private[release] val semverPatternNoBugfix = "^([0-9]+)\\.([0-9]+)$".r
    private[release] val semverPatternNoMinor = "^([0-9]+)$".r
    private[release] val semverPatternLowdash = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)_([0-9]+)$".r
    private[release] val semverPatternLowdashString = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)_(.+)$".r
    private[release] val semverPatternRCEnd = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)-((?:RC|M)[1-9][0-9]*)$".r
    private[release] val semverPatternLetterEnd = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)-([0-9a-zA-Z\\.]+)$".r
    private[release] val stableShop = "^([0-9]+x)-stable.*$".r
    private[release] val shopPattern = "^(RC-)([0-9]{4})\\.([0-9]+)?(?:\\.([0-9]+[0-9]*))?(?:_([0-9]+[0-9]*))?$".r
    private[release] val number = "^([0-9]+)(.*)".r
    private[release] val number2 = "^([0-9]+)\\.([0-9]+)(.*)".r
    private[release] val number3 = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)(.*)".r

    implicit def ordering[A <: Version]: Ordering[A] =
      Ordering.by(e => (e.major, e.minor, e.patch, e.low, e.pre))

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

          case any => undef.copy(orginal = any)
        }
      } catch {
        case e: Exception => e.printStackTrace(); undef.copy(orginal = versionText)
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

  def checkForUpdates(in: Seq[Gav3], shellWidth: Int, depUpOpts: OptsDepUp, aether: Repo): Map[Gav3, (Seq[String], Duration)] = {

    val aetherFetch = StatusLine(in.size, shellWidth)
    val updates: Map[Gav3, (Seq[String], Duration)] = in
      .par
      .map(dep => (dep, {
        aetherFetch.start()
        val newerVersions = if (depUpOpts.filter.isDefined && !depUpOpts.filter.get.matches(dep.formatted)) {
          aether.newerVersionsOf(dep.groupId, dep.artifactId, dep.version).headOption.toSeq
        } else {
          aether.newerVersionsOf(dep.groupId, dep.artifactId, dep.version)
        }

        val selectedVersions = if (depUpOpts.filter.isDefined) {
          newerVersions.map(v => dep.copy(version = v))
            .filter(gav => depUpOpts.filter.get.matches(gav.formatted))
            .map(_.version)
        } else if (depUpOpts.hideStageVersions) {
          ProjectMod.normalizeUnwantedVersions(dep, newerVersions)
        } else {
          newerVersions
        }
        val d = if (depUpOpts.showLibYears && selectedVersions.nonEmpty) {
          val latest = selectedVersions.last // TODO version sort
          if (dep != dep.copy(version = latest)) {
            val currentDate = aether.depDate(dep.groupId, dep.artifactId, dep.version)
            val latestDate = aether.depDate(dep.groupId, dep.artifactId, latest)
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
        aetherFetch.end()
        (selectedVersions, d)
      }))
      .seq
      .toMap

    aetherFetch.finish()
    updates
  }

  def showDependencyUpdates(shellWidth: Int, termOs: Term, depUpOpts: OptsDepUp, workNexusUrl: String,
                            rootDeps: Seq[Dep], selfDepsMod: Seq[Dep], aether: Repo,
                            out: PrintStream, err: PrintStream): Seq[(GavWithRef, (Seq[String], Duration))] = {
    val now = LocalDate.now()
    val stopw = Stopwatch.createStarted()
    out.println("I: checking dependecies against nexus - please wait")

    val selfSimple = selfDepsMod.map(_.gav().simpleGav()).distinct
    val relevant: Seq[Dep] = rootDeps
      .filterNot(_.version == "")
      .filterNot(in => selfSimple.contains(in.gav().simpleGav()))

    val relevantGav = relevant
      .map(_.gav())
      .distinct

    val emptyVersions = rootDeps
      .filter(_.version == "")
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
      .flatMap(ProjectMod.scalaDeps(prepared))
    val updates = checkForUpdates(value, shellWidth, depUpOpts, aether)
    // TODO check again if found scala deps (lowdash)

    out.println(s"I: checked ${value.size} dependecies in ${stopw.elapsed(TimeUnit.MILLISECONDS)}ms (${now.toString})")

    // TODO move Version check to here

    val checkedUpdates: Map[Gav3, (Seq[String], Duration)] = updates.map(gavAndVersion => {
      if (gavAndVersion._2._1 == Nil) {
        (gavAndVersion._1, (Nil, gavAndVersion._2._2)) // TODO remove this, because it is invalid
      } else {
        (gavAndVersion._1, (gavAndVersion._2._1.tail, gavAndVersion._2._2))
      }
    })

    val allWithUpdate: Seq[(GavWithRef, (Seq[String], Duration))] = relevant
      .map(in => {
        val ref = GavWithRef(in.pomRef, in.gav())
        val a: (Seq[String], Duration) = checkedUpdates.getOrElse(in.gav().simpleGav(), (Nil, Duration.ZERO))
        (ref, a)
      })
      .filterNot((in: (GavWithRef, (Seq[String], Duration))) => depUpOpts.hideLatest && in._2._1.isEmpty)

    allWithUpdate.groupBy(_._1.pomRef).foreach(element => {
      val ref: SelfRef = element._1
      val mods: Seq[(GavWithRef, (Seq[String], Duration))] = element._2

      def ch(pretty: String, simple: String) = if (!termOs.isCygwin || termOs.isMinGw) {
        if (termOs.simpleChars) {
          simple
        } else {
          pretty
        }
      } else {
        simple
      }


      out.println(ch("║ ", "| ") + "Project GAV: " + ref.id)
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
          out.println(ch("╠═╦═ ", "+-+- ") + subElement._1.gav.formatted)
        } else {
          out.println(ch("╠═══ ", "+--- ") + subElement._1.gav.formatted)
        }

        if (majorVersions.size == 1) {
          out.println(ch("║ ╚═══ ", "| +--- ") +
            abbreviate(depUpOpts.versionRangeLimit)(majorVersions.head._2).mkString(", ") + libyear)
        } else {
          if (majorVersions != Nil) {
            majorVersions.tail.reverse.foreach(el => {
              out.println(ch("║ ╠═══ ", "| +--- ") + "(" + el._1 + ") " +
                abbreviate(depUpOpts.versionRangeLimit)(el._2).mkString(", "))
            })
            out.println(ch("║ ╚═══ ", "| +--- ") + "(" + majorVersions.head._1 + ") " +
              abbreviate(depUpOpts.versionRangeLimit)(majorVersions.head._2).mkString(", ") + libyear)
          }
        }

      })
      out.println(ch("║", "|"))
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
        err.println("Non existing dependencies for:\n" +
          versionNotFound.toList.map(in => in._1.formatted + "->" + (in._2._1 match {
            case Nil => "Nil"
            case e => e
          }) + "\n  " + workNexusUrl + in._1.slashedMeta).sorted.mkString("\n"))
        err.println()
      }
    }

    {
      val unmangedVersions = unmanged(emptyVersions, relevantGav)
      if (unmangedVersions != Nil) {
        err.println("Empty or managed versions found:")
        unmangedVersions.map(_.simpleGav()).foreach(err.println)
        err.println()
      }
    }

    out.println("term: " + termOs)
    if (depUpOpts.showLibYears) {
      // https://libyear.com/
      // https://ericbouwers.github.io/papers/icse15.pdf
      out.println()
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
        out.println("WARN: negative durations for:")
        updates
          .filter(_._2._2.isNegative)
          .foreach(e => println(s"${e._1} ${e._2._2.toString}"))
      }
      out.println(s"libyears: ${period.getYears}.${period.getMonths} (${sum.toDays} days)")
    }
    allWithUpdate
  }

}

trait ProjectMod extends LazyLogging {
  val file: File
  val repo: Repo
  val opts: Opts
  val selfVersion: String

  val listDependecies: Seq[Dep]
  val listPluginDependencies: Seq[PluginDep]

  val listProperties: Map[String, String]
  val skipPropertyReplacement: Boolean

  def showDependencyUpdates(shellWidth: Int, termOs: Term, depUpOpts: OptsDepUp, workNexusUrl: String,
                            out: PrintStream, err: PrintStream): Unit = {
    val depForCheck: Seq[Dep] = listDependeciesForCheck()
    val sdm = selfDepsMod
    val result = ProjectMod.showDependencyUpdates(shellWidth, termOs, depUpOpts, workNexusUrl,
      depForCheck, sdm, repo,
      out, err)
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
  }

  private[release] def replacedPropertyOf(string: String) = {
    PomMod.replaceProperty(listProperties, skipPropertyReplacement)(string)
  }

  private[release] def replacedVersionProperties(deps: Seq[Dep]) = deps.map(dep => dep.copy(
    version = replacedPropertyOf(dep.version),
    packaging = replacedPropertyOf(dep.packaging),
    typeN = replacedPropertyOf(dep.typeN),
    scope = replacedPropertyOf(dep.scope))
  ).map(in => {
    if (in.toString.contains("$") && !skipPropertyReplacement) {
      throw new IllegalStateException("missing var in " + in)
    }
    in
  })

  private[release] def listDependeciesForCheck(): Seq[Dep] = replacedVersionProperties(listDependecies) ++
    listPluginDependencies.map(_.fakeDep())
      .filterNot(_.version == "") // managed plugins are okay
      .map(in => in.groupId match {
        case "" => in.copy(groupId = "org.apache.maven.plugins")
        case _ => in
      })

  def isNoShop: Boolean = {
    !isShop
  }

  def isShop: Boolean

  def selfDepsMod: Seq[Dep]

  def suggestReleaseVersion(branchNames: Seq[String] = Nil): Seq[String]

  def suggestNextRelease(releaseVersion: String): String

  def listSnapshotsDistinct: Seq[Dep]

  def writeTo(targetFolder: File): Unit

  def changeVersion(newVersion: String): Unit

  def changeDependecyVersion(patch: Seq[(Gav3, String)]): Unit

  def depTreeFilenameList(): Seq[String]
}
