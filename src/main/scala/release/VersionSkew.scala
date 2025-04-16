package release

import release.Lint.{fiCodeCoreDiff, fiFine, fiWarn, lineMax}
import release.ProjectMod.{Dep, Gav}
import release.Term.{info, warn}

import java.io.PrintStream
import java.util.concurrent.atomic.AtomicBoolean

object VersionSkew {
  case class SkewResult(hasDifferentMajors: Boolean, sortedMajors: Seq[String], releaseMajorVersion: String,
                        coreMajorVersions: Seq[(String, Dep)])

  def coreMajorResultOf(mod: ProjectMod, release: Option[String],
                        warnExit: Option[AtomicBoolean] = None, errorExit: Option[AtomicBoolean] = None,
                        out: Option[PrintStream], opts: Option[Opts]): SkewResult = {
    val relevantDeps = if (mod.isNoShop) {
      mod.listDependencies.filter(in => in.groupId.startsWith("com.novomind.ishop.core"))
    } else {
      val selfGavs = mod.selfDepsMod.map(_.gav())
      mod.listDependencies
        .filter(in => in.groupId.startsWith("com.novomind.ishop"))
        .filterNot(in => selfGavs.contains(in.gav()))
    }
    val mrc = coreMajorResultOf(relevantDeps, mod.isNoShop, release)
    if (out.isDefined && mrc.hasDifferentMajors) {
      warnExit.get.set(true)
      // TODO https://kubernetes.io/releases/version-skew-policy/
      // v0.32.7 ⚡️v0.50.3 (version ∆ x expected y)
      out.get.println(warn(s"    Found multiple core major version: »${mrc.sortedMajors.mkString(", ")}«, use only one ${fiWarn} ${fiCodeCoreDiff.apply(mrc)}", opts.get, limit = lineMax))
      val versions = mrc.coreMajorVersions
      val mrcGrouped: Map[String, Seq[Gav]] = versions.groupBy(_._1)
        .map(e => (e._1, e._2.map(_._2.gav()).distinct))

      mrcGrouped.toSeq
        .sortBy(_._1.toIntOption)
        .foreach(gavE => {
          out.get.println(warn(s"      - ${gavE._1} -", opts.get, limit = lineMax))
          gavE._2.foreach(gav => {
            out.get.println(warn(s"      ${gav.formatted} ${fiWarn} ${fiCodeCoreDiff.apply(gav)}", opts.get, limit = lineMax))
          })
        })

    } else {
      if (out.isDefined) {
        out.get.println(info(s"    ${fiFine} no major version diff", opts.get))
      }
    }
    mrc
  }

  def coreMajorResultOf(relevantDeps: Seq[Dep], isNoShop: Boolean, release: Option[String]): SkewResult = {

    if (relevantDeps.nonEmpty) {
      val releaseMajorVersion = if (isNoShop && release.isDefined) {
        release.get.replaceAll("\\..*", "")
      } else {
        val value = relevantDeps.filterNot(_.version.isEmpty).map(_.version.get)
        if (value.isEmpty && release.isDefined) {
          release.get.replaceAll("\\..*", "")
        } else if (value.isEmpty && release.isEmpty) {
          ""
        } else {
          value.maxBy(Version.parse).replaceAll("\\..*", "")
        }

      }
      val relevantFilteredDeps = (if (releaseMajorVersion.matches("[0-9]+")) {
        if (releaseMajorVersion.toInt > 36) {
          relevantDeps
            .filterNot(in => in.groupId == "com.novomind.ishop.shops" &&
              in.artifactId == "ishop-shop-parent" &&
              in.version.get.startsWith("36")) // TODO improve to .toInt > 36 later
            .filterNot(in => in.groupId == "com.novomind.ishop" &&
              in.artifactId == "ishop-meta-parent" &&
              in.version.get.startsWith("36")) // TODO improve to .toInt > 36 later
        } else if (releaseMajorVersion.toInt >= 32) {
          relevantDeps
        } else {
          relevantDeps
            .filterNot(in => in.groupId == "com.novomind.ishop.exi" && in.artifactId == "ishop-ext-authentication")
        }
      } else {
        relevantDeps
      }).filterNot(_.version.isEmpty)
      val coreVersions: Seq[(String, Dep)] = relevantFilteredDeps
        .map(in => (in.version.get, in))
        .distinct
        .filter(_._1.nonEmpty)
        .sortBy(_._1)

      val (sortedMajors: Seq[String], hasDiff: Boolean, coreMajorVersions: Seq[(String, Dep)]) = Release.findDiff(releaseMajorVersion, coreVersions)
      SkewResult(hasDiff, sortedMajors, releaseMajorVersion, coreMajorVersions)
    } else {
      SkewResult(hasDifferentMajors = false, Nil, "", Nil)
    }

  }

}
