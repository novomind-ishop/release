package release

import java.io.{File, PrintStream}

import com.typesafe.scalalogging.LazyLogging
import release.PomMod.{abbreviate, unmanged}
import release.ProjectMod.{Dep, Gav3, PluginDep, PomRef}
import release.Starter.{Opts, OptsDepUp, TermOs}
import scala.collection.parallel.CollectionConverters._

import scala.annotation.tailrec

object ProjectMod {

  case class Dep(pomRef: PomRef, groupId: String, artifactId: String, version: String, typeN: String,
                 scope: String, packaging: String, classifier: String) {
    val gavWithDetailsFormatted: String = Gav.format(Seq(groupId, artifactId, version, typeN, scope, packaging, classifier))

    def gav() = Gav(groupId, artifactId, version, packaging, classifier, scope)
  }

  case class PluginExec(id: String, goals: Seq[String], phase: String, config: Map[String, String])

  case class PluginDep(pomRef: PomRef, groupId: String, artifactId: String, version: String, execs: Seq[PluginExec], pomPath: Seq[String]) {

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
  }

  object Gav {
    def format(parts: Seq[String]): String = parts.mkString(":").replaceAll("[:]{2,}", ":").replaceFirst(":$", "")

    val empty = Gav(groupId = "", artifactId = "", version = "")
  }

  case class PomRef(id: String)

  object PomRef {
    val undef = PomRef("X")
  }

  case class Version(pre: String, major: Int, minor: Int, patch: Int, low: String) {

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
    private[release] val stableShop = "^([0-9]+x)-stable.*$".r
    private[release] val shopPattern = "^(RC-)([0-9]{4})\\.([0-9]+)?(?:\\.([0-9]+[0-9]*))?(?:_([0-9]+[0-9]*))?$".r

    implicit def ordering[A <: Version]: Ordering[A] = Ordering.by(e => (e.major, e.minor, e.patch))

    val undef: Version = Version("n/a", -1, -1, -1, "")

    private def nullToZero(in: String) = if (in == null || in == "") {
      0
    } else if (in.forall(_.isDigit)) {
      in.toInt
    } else {
      -1
    }

    def fromStringOpt(pre: String, major: String, minor: String, patch: String, low: String): Option[Version] = {
      Some(fromString(pre, major, minor, patch, low))
    }

    def fromString(pre: String, major: String, minor: String, patch: String, low: String): Version = {
      Version(pre, nullToZero(major), nullToZero(minor), nullToZero(patch), Util.nullToEmpty(low))
    }

    def toVersion(m: String): Option[Version] = m match {
      case semverPattern(ma, mi, b) => Version.fromStringOpt("", ma, mi, b, "")
      case semverPatternLowdash(ma, mi, b, low) => Version.fromStringOpt("", ma, mi, b, low)
      case _ => None
    }

    def parse(versionText: String): Version = {
      val snapped = versionText.replaceFirst("-SNAPSHOT", "")

      try {
        snapped match {
          case stableShop(pre) => undef
          case semverPatternLowdash(ma, mi, b, low) => Version.fromString("", ma, mi, b, low)
          case semverPatternLowdashString(ma, mi, b, low) => Version.fromString("", ma, mi, b, low)
          case semverPattern(ma, mi, b) => Version.fromString("", ma, mi, b, "")
          case semverPatternNoBugfix(ma, mi) => Version.fromString("", ma, mi, "", "")
          case semverPatternNoMinor(ma) => Version.fromString("", ma, "", "", "")
          case shopPattern(pre, year, week, minor, low) => Version.fromString(pre, year, week, minor, low)
          case any => undef
        }
      } catch {
        case e: Exception => e.printStackTrace(); undef
      }
    }
  }
}

trait ProjectMod extends LazyLogging {
  val file: File
  val aether: Aether
  val opts: Opts
  val selfVersion: String

  val listDependecies: Seq[Dep]
  val listPluginDependencies: Seq[PluginDep]

  val listProperties: Map[String, String]

  def showDependencyUpdates(shellWidth: Int, termOs: TermOs, depUpOpts: OptsDepUp, workNexusUrl: String,
                            out: PrintStream, err: PrintStream): Unit = {

    if (opts.invalids != Nil) {
      err.println("Invalid options:")
      err.println(opts.invalids.mkString(", "))
      err.println()
      System.exit(1)
    }

    out.println("I: checking dependecies against nexus - please wait")
    val rootDeps = listDependeciesForCheck()

    def normalizeUnwanted(gav: Gav3, inVersions: Seq[String]): Seq[String] = {
      val out: Seq[String] = inVersions.filterNot(_.endsWith("-SNAPSHOT"))
        .filterNot(_.contains("patch"))
        .filterNot(_.matches(".*[Mm][0-9]+$"))
        .filterNot(_.matches(".*-[Mm][0-9]+-.*"))
        .filterNot(_.matches(".*pr[0-9]+$"))
        .filterNot(_.contains("alpha"))
        .filterNot(_.contains("Alpha"))
        .filterNot(_.contains("ALPHA"))
        .filterNot(_.contains("BETA"))
        .filterNot(_.contains("Beta"))
        .filterNot(_.contains("beta"))
        .filterNot(_.contains("brew"))
        .filterNot(_.matches(".*b[0-9]+.*"))
        .filterNot(_.endsWith(".*\\-beta$"))
        .filterNot(_.matches(".*SP[0-9]+$"))
        .filterNot(_.matches(".*-SNAP[0-9]+$"))
        .filterNot(_.matches(".*[(sec|SEC)][0-9]+$"))
        .filterNot(_.endsWith("-incubating"))
        .filterNot(_.endsWith("SONATYPE"))
        .filterNot(_.contains("jbossorg"))
        .filterNot(_.contains("-atlassian-"))
        .filterNot(_.matches(".*jenkins-[0-9]+$"))
        .filterNot(_.contains("PFD"))
        .filterNot(_.contains("-cdh"))
        .filterNot(_.contains("darft"))
        .filterNot(_.startsWith("2003"))
        .filterNot(_.startsWith("2004"))
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

    val selfSimple = selfDepsMod.map(_.gav().simpleGav()).distinct
    val relevant = rootDeps
      .filterNot(_.version == "")
      .filterNot(in => selfSimple.contains(in.gav().simpleGav()))

    val relevantGav = relevant
      .map(_.gav())
      .distinct

    val emptyVersions = rootDeps
      .filter(_.version == "")
      .map(_.gav())
      .distinct

    val aetherFetch = StatusLine(relevantGav.size, shellWidth)
    val updates: Map[Gav3, Seq[String]] = relevantGav.par.map(_.simpleGav())
      .map(in => {
        if (in.version.isEmpty || in.artifactId.isEmpty || in.groupId.isEmpty) {
          throw new IllegalStateException("gav has empty parts: " + in)
        } else {
          in
        }
      })
      .map(dep => (dep, {
        aetherFetch.start()
        val result = aether.newerVersionsOf(dep.groupId, dep.artifactId, dep.version)
        aetherFetch.end()
        result
      }))
      .seq
      .map(in => if (depUpOpts.hideStageVersions) {
        in.copy(_2 = normalizeUnwanted(in._1, in._2))
      } else {
        in
      })
      .toMap

    aetherFetch.finish()

    case class GavWithRef(pomRef: PomRef, gavWithDetailsFormatted: String)

    // TODO move Version check to here

    val checkedUpdates = updates.map(gavAndVersion => {
      if (gavAndVersion._2 == Nil) {
        (gavAndVersion._1, Nil) // TODO remove this, because it is invalid
      } else {
        (gavAndVersion._1, gavAndVersion._2.tail)
      }

    })

    val allWithUpdate: Seq[(GavWithRef, Seq[String])] = relevant.map(in => (GavWithRef(in.pomRef, in.gavWithDetailsFormatted),
      checkedUpdates.getOrElse(in.gav().simpleGav(), Nil))).filterNot(in => depUpOpts.hideLatest && in._2.isEmpty)

    allWithUpdate.groupBy(_._1.pomRef).foreach(element => {
      val ref: PomRef = element._1
      val mods: Seq[(GavWithRef, Seq[String])] = element._2

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
      mods.sortBy(_._1.toString).foreach((subElement: (GavWithRef, Seq[String])) => {

        val o: Seq[String] = subElement._2

        def majorGrouping(in: String): String = {
          val major = in.replaceFirst("\\..*", "")
          if (in == major) {
            "_"
          } else {
            major
          }
        }

        val majorVersions: Seq[(String, Seq[String])] = o.groupBy(majorGrouping).toSeq
          .sortBy(_._1).reverse
        if (majorVersions != Nil) {
          out.println(ch("╠═╦═ ", "+-+- ") + subElement._1.gavWithDetailsFormatted)
        } else {
          out.println(ch("╠═══ ", "+--- ") + subElement._1.gavWithDetailsFormatted)
        }

        if (majorVersions.size == 1) {
          out.println(ch("║ ╚═══ ", "| +--- ") +
            abbreviate(depUpOpts.comactVersionRangeTo)(majorVersions.head._2).mkString(", "))
        } else {
          if (majorVersions != Nil) {
            majorVersions.tail.reverse.foreach(el => {
              out.println(ch("║ ╠═══ ", "| +--- ") + "(" + el._1 + ") " +
                abbreviate(depUpOpts.comactVersionRangeTo)(el._2).mkString(", "))
            })
            out.println(ch("║ ╚═══ ", "| +--- ") + "(" + majorVersions.head._1 + ") " +
              abbreviate(depUpOpts.comactVersionRangeTo)(majorVersions.head._2).mkString(", "))
          }
        }

      })
      out.println(ch("║", "|"))
    })

    {
      // TODO check versions before
      val versionNotFound = updates.filter(_._2 == Nil)
      if (versionNotFound.nonEmpty) {
        // TODO throw new PreconditionsException
        err.println("Non existing dependencies for:\n" +
          versionNotFound.map(in => in._1.formatted + "->" + (in._2 match {
            case Nil => "Nil"
            case e => e
          }) + "\n  " + workNexusUrl + in._1.slashedMeta).toList.sorted.mkString("\n"))
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

  }

  private[release] def replacedPropertyOf(string: String) = PomMod.replaceProperty(listProperties)(string)


  private[release] def replacedVersionProperties(deps: Seq[Dep]) = deps.map(dep => dep.copy(
    version = replacedPropertyOf(dep.version),
    packaging = replacedPropertyOf(dep.packaging),
    typeN = replacedPropertyOf(dep.typeN),
    scope = replacedPropertyOf(dep.scope))
  ).map(in => {
    if (in.toString.contains("$")) {
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
}
