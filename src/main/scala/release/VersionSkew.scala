package release

import release.Lint.{UniqCode, fiCodeCoreDiff, fiFine, fiWarn, fiWarnMuted, lineMax}
import release.ProjectMod.{Dep, Gav}
import release.Term.{info, warn}

import java.io.PrintStream
import java.util.concurrent.atomic.AtomicBoolean

object VersionSkew {
  case class SkewResult(hasDifferentMajors: Boolean, sortedMajors: Seq[String], releaseMajorVersion: String,
                        coreMajorVersions: Seq[(String, Gav)], usedLintSkips: Seq[UniqCode])

  def skewResultOf(mod: ProjectMod, releaseVersion: Option[String],
                   warnExit: Option[AtomicBooleanFlip] = None, errorExit: Option[AtomicBooleanFlip] = None,
                   out: Option[PrintStream], opts: Opts, skewStyle: Option[String]): SkewResult = {
    val isNoShop = mod.isNoShop
    val relevantDeps: Seq[Dep] = if (skewStyle.isDefined) {
      mod.listDependencies
    } else if (isNoShop) {
      mod.listDependencies.filter(in => in.groupId.startsWith("com.novomind.ishop.core"))
    } else {
      val selfGavs = mod.selfDepsMod.map(_.gav())
      mod.listDependencies
        .filter(in => in.groupId.startsWith("com.novomind.ishop"))
        .filterNot(in => selfGavs.contains(in.gav()))
    }
    innerSkewResult(releaseVersion, warnExit, out, opts, isNoShop, relevantDeps)
  }

  private[release] def innerSkewResult(releaseVersion: Option[String], warnExit: Option[AtomicBooleanFlip], out: Option[PrintStream],
                                       opts: Opts, isNoShop: Boolean, relevantDeps: Seq[Dep]) = {
    var usedSkips = Seq.empty[UniqCode]
    var warnX = false
    var sjwef = Set.empty[String]
    val mrc = skewResultOfLayer(relevantDeps, isNoShop, releaseVersion)
    if (out.isDefined && mrc.hasDifferentMajors) {
      // TODO https://kubernetes.io/releases/version-skew-policy/
      // v0.32.7 ⚡️v0.50.3 (version ∆ x expected y)
      val code = fiCodeCoreDiff.apply(mrc.copy(usedLintSkips = Nil))
      val mainSkip = opts.lintOpts.skips.contains(code)
      if (mainSkip) {
        out.get.println(info(s"       Found multiple core major version: »${mrc.sortedMajors.mkString(", ")}«, use only one ${fiWarnMuted} $code", opts, limit = lineMax))
        usedSkips = usedSkips :+ code
      } else {
        out.get.println(warn(s"    Found multiple core major version: »${mrc.sortedMajors.mkString(", ")}«, use only one ${fiWarn} $code", opts, limit = lineMax))
        warnX = true
      }


      val versions = mrc.coreMajorVersions
      val mrcGrouped: Map[String, Seq[Gav]] = versions.groupBy(_._1)
        .map(e => (e._1, e._2.map(_._2).distinct))


      mrcGrouped.toSeq
        .sortBy(_._1.toIntOption)
        .foreach(gavE => {
          if (mainSkip) {
            out.get.println(info(s"         - ${gavE._1} -", opts, limit = lineMax))
          } else {
            out.get.println(warn(s"      - ${gavE._1} -", opts, limit = lineMax))
          }
          gavE._2.foreach(gav => {
            val code1 = fiCodeCoreDiff.apply(gav)
            val bool = opts.lintOpts.skips.contains(code1)
            if (bool || mainSkip) {
              out.get.println(info(s"         ${gav.formatted} ${fiWarnMuted} $code1", opts, limit = lineMax))
            }
            if (bool && !mainSkip) {
              usedSkips = usedSkips :+ code1
            } else {
              if (!mainSkip) {
                sjwef = sjwef + gavE._1
                out.get.println(warn(s"      ${gav.formatted} ${fiWarn} $code1", opts, limit = lineMax))
                warnX = true
              }
            }
          })
        })

    } else {
      if (out.isDefined) {
        out.get.println(info(s"    ${fiFine} no major version diff", opts))
      }
    }
    if (warnX && sjwef.size > 1) {
      warnExit.get.set()
    }
    mrc.copy(usedLintSkips = mrc.usedLintSkips ++ usedSkips)
  }

  private[release] def skewResultOfLayer(relevantDeps: Seq[Dep], isNoShop: Boolean, releaseVersion: Option[String]): SkewResult = {
    val usedSkips = Nil
    if (relevantDeps.nonEmpty) {
      val releaseMajorVersion = if (isNoShop && releaseVersion.isDefined) {
        releaseVersion.get.replaceAll("\\..*", "")
      } else {
        val value = relevantDeps.filterNot(_.version.isEmpty).map(_.version.get)
        if (value.isEmpty && releaseVersion.isDefined) {
          releaseVersion.get.replaceAll("\\..*", "")
        } else if (value.isEmpty && releaseVersion.isEmpty) {
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
      SkewResult(hasDiff, sortedMajors, releaseMajorVersion, coreMajorVersions.map(e => (e._1, e._2.gav())), usedSkips)
    } else {
      SkewResult(hasDifferentMajors = false, Nil, "", Nil, usedSkips)
    }

  }

}
