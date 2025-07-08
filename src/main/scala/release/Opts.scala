package release

import com.google.common.base.Strings
import org.eclipse.aether.repository.RemoteRepository

import scala.annotation.tailrec
import scala.util.matching.Regex


object Opts {
  def newRepoZ(opts: Opts): RepoZ = {

    val mirrorNexus: RemoteRepository = Repo.newDefaultRepositoryU(ReleaseConfig.default(opts.useDefaults).mirrorNexusUrl())

    val workNexus: RemoteRepository = Repo.newDefaultRepositoryU(ReleaseConfig.default(opts.useDefaults).workNexusUrl())

    Repo.of(mirrorNexus, workNexus)
  }


  def showDemoChars(inOpt: Opts): Opts = {
    val sys = Term.Sys.default
    sys.out.println()
    sys.out.println("\u001B[31m" + "This text is red!" + "\u001B[0m")
    val r = Term.readFrom(sys, "test press enter",
      "u200B(\u200B),u0009(\u0009),u00A0(\u00A0),u1680(\u1680),,,u2012(\u2012),u2013(\u2013)", inOpt)
    sys.out.print("\u001B[30;45m")
    sys.out.println("demo: " + r)
    sys.out.print("\u001B[0m")
    System.exit(3)
    null
  }


  @tailrec
  def envRead(envs: Seq[(String, String)], inOpt: Opts): Opts = {
    envs match {
      case Nil => inOpt
      case ("RELEASE_NO_GERRIT", k) :: tail => envRead(tail, {
        inOpt.copy(useGerrit = k.toBooleanOption.forall(b => !b))
      })
      case ("RELEASE_LINT_SKIP", k) :: tail => envRead(tail, {
        val skips = Strings.nullToEmpty(k).split(",").toSeq.map(_.trim).distinct.filterNot(_.isEmpty)
        inOpt.copy(lintOpts = inOpt.lintOpts.copy(skips = inOpt.lintOpts.skips ++ skips))
      })
      case ("RELEASE_LINT_TIMESTAMPS", k) :: tail => envRead(tail, {
        inOpt.copy(lintOpts = inOpt.lintOpts.copy(showTimeStamps = Strings.nullToEmpty(k).toBooleanOption.getOrElse(false)))
      })
      case ("RELEASE_LINT_CHECKPACKAGES", k) :: tail => envRead(tail, {
        inOpt.copy(lintOpts = inOpt.lintOpts.copy(checkPackages = Strings.nullToEmpty(k).toBooleanOption.getOrElse(false)))
      })
      case ("RELEASE_SHOW_OPTS", k) :: tail => envRead(tail, {
        inOpt.copy(showOpts = Strings.nullToEmpty(k).toBooleanOption.getOrElse(false))
      })
      case (_, _) :: tail => envRead(tail, inOpt)
      case _ => throw new IllegalStateException("not expected")
    }
  }

  @tailrec
  private def argsDepRead(params: Seq[String], inOpt: Opts): Opts = {
    params.filter(in => in.trim.nonEmpty) match {
      case Nil => inOpt
      case "--help" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showHelp = true)))
      case "-h" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showHelp = true)))
      case "--matches" :: pattern :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(filter = Some(pattern.r))))
      case "--no-filter" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts
        .copy(hideStageVersions = false, hideLatest = false)))
      case "--no-updates" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts
        .copy(hideStageVersions = true, hideLatest = false, hideUpdates = true)))
      case "--no-omit" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(versionRangeLimit = Integer.MAX_VALUE)))
      case "--show-libyears" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showLibYears = true)))
      case "--create-patch" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(changeToLatest = true)))
      // --
      case string :: Nil => argsDepRead(Nil, inOpt.focusDepInvalids().replace(inOpt.depUpOpts.diag.invalids :+ string))
      case string :: tail => argsDepRead(tail, inOpt.focusDepInvalids().replace(inOpt.depUpOpts.diag.invalids :+ string))
      case _ => throw new IllegalStateException("not expected")
    }
  }


  @tailrec
  def argsRead(params: Seq[String], inOpt: Opts): Opts = {
    params.filter(in => in.trim.nonEmpty) match {
      case Nil => inOpt
      case "--simple-chars" :: tail => argsRead(tail, inOpt.copy(simpleChars = true))
      case "--help" :: tail => argsRead(tail, inOpt.copy(showHelp = true))
      case "--check-git" :: tail => {
        Sgit.checkVersion(Sgit.versionOnly(), System.out, System.err, None)
        System.exit(0)
        null
      }
      case "-h" :: tail => argsRead(tail, inOpt.copy(showHelp = true))
      case "--replace" :: tail => argsRead(tail, inOpt) // handled by shell
      case "--show-update-cmd" :: tail => argsRead(tail, inOpt.copy(showUpdateCmd = true, showStartupDone = false))
      case "--no-gerrit" :: tail => argsRead(tail, inOpt.copy(useGerrit = false))
      case "--no-update" :: tail => argsRead(tail, inOpt.copy(doUpdate = false))
      case "--non-interactive" :: tail => argsRead(tail, inOpt.copy(isInteractive = false))
      case "-B" :: tail => argsRead(tail, inOpt.copy(isInteractive = false))
      case "--defaults" :: tail => argsRead(tail, inOpt.copy(useDefaults = true))
      case "--no-jline" :: tail => argsRead(tail, inOpt.copy(useJlineInput = false))
      case "--no-color" :: tail => argsRead(tail, inOpt.copy(colors = false))
      case "--show-opts" :: tail => argsRead(tail, inOpt.copy(showOpts = true))
      case "--no-check-overlap" :: tail => argsRead(tail, inOpt.copy(checkOverlapping = false))
      case "--no-check-project-vars" :: tail => argsRead(tail, inOpt.copy(checkProjectDeps = false))
      // TODO no color env property
      case "--100" :: tail => argsRead(tail, inOpt.copy(versionIncrement = Increment.major))
      case "--010" :: tail => argsRead(tail, inOpt.copy(versionIncrement = Increment.minor))
      case "--001" :: tail => argsRead(tail, inOpt.copy(versionIncrement = Increment.patch))
      case "--demo-chars" :: _ => showDemoChars(inOpt)
      case "--skip-property" :: value :: tail => argsRead(tail, inOpt.copy(skipProperties = inOpt.skipProperties ++ Seq(value)))
      // CMDs
      case "lint" :: tail => argsLintRead(tail, inOpt)
      case "showSelf" :: tail => argsRead(tail, inOpt.copy(showSelfGa = true))
      case "apidiff" :: tail => argsApiDiffRead(tail, inOpt.copy(apiDiff = inOpt.apiDiff.copy(showApiDiff = true)))
      case "suggest-docker-tag" :: tail => argsRead(tail, inOpt.copy(suggestDockerTag = true, showStartupDone = false))
      case "suggest-remote-branch" :: tail => argsRead(tail, inOpt.copy(suggestRemoteBranch = true, showStartupDone = false, isInteractive = false))
      case "versionSet" :: value :: _ => argsRead(Nil, inOpt.copy(versionSet = Some(value)))
      case "shopGASet" :: value :: _ => argsRead(Nil, inOpt.copy(shopGA = Some(value)))
      case "nothing-but-create-feature-branch" :: _ => argsRead(Nil, inOpt.copy(createFeature = true))
      case "showDependencyUpdates" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showDependencyUpdates = true)))

      // --
      case string :: Nil => argsRead(Nil, inOpt.focusInvalids().replace(inOpt.diag.invalids :+ string))
      case string :: tail => argsRead(tail, inOpt.focusInvalids().replace(inOpt.diag.invalids :+ string))
      case _ => throw new IllegalStateException("not expected")
    }
  }

  @tailrec
  def argsAndEnvRead(params: Seq[String], inOpt: Opts, envs: Map[String, String]): Opts = {
    if (envs.isEmpty) {
      argsRead(params, inOpt)
    } else {
      argsAndEnvRead(params, envRead(envs.toSeq, inOpt), Map.empty)
    }
  }

  @tailrec
  def argsApiDiffRead(params: Seq[String], inOpt: Opts): Opts = {
    params.filter(in => in.trim.nonEmpty) match {
      case Nil => inOpt

      case "--pom-only" :: tail => argsApiDiffRead(tail, inOpt.copy(apiDiff = inOpt.apiDiff.copy(pomOnly = true)))
      case "--all" :: tail => argsApiDiffRead(tail, inOpt.copy(apiDiff = inOpt.apiDiff.copy(allModifications = true)))
      case "--help" :: tail => argsApiDiffRead(tail, inOpt.copy(apiDiff = inOpt.apiDiff.copy(showHelp = true)))
      case "-h" :: tail => argsApiDiffRead(tail, inOpt.copy(apiDiff = inOpt.apiDiff.copy(showHelp = true)))
      case string1 :: string2 :: tail => argsApiDiffRead(tail, inOpt.copy(apiDiff =
        inOpt.apiDiff.copy(left = string1, right = string2)))
      // --
      case string :: Nil => argsApiDiffRead(Nil, inOpt.focusApiInvalids().replace(inOpt.apiDiff.diag.invalids :+ string))
      //case string :: tail => argsApiDiffRead(tail, inOpt.focusApiInvalids().replace(inOpt.apiDiff.diag.invalids :+ string))
      case _ => throw new IllegalStateException("not expected")
    }
  }

  @tailrec
  def argsLintRead(params: Seq[String], inOpt: Opts): Opts = {
    params.filter(in => in.trim.nonEmpty) match {
      case Nil => inOpt.copy(lintOpts = inOpt.lintOpts.copy(doLint = true), showStartupDone = false)
      case "--help" :: tail => argsLintRead(tail, inOpt.copy(lintOpts = inOpt.lintOpts.copy(showHelp = true)))
      case "-h" :: tail => argsLintRead(tail, inOpt.copy(lintOpts = inOpt.lintOpts.copy(showHelp = true)))
      case "--timeout-secs" :: secs :: tail => argsLintRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(timeoutSec = secs.toIntOption)))
      case "--strict" :: tail => argsLintRead(tail, inOpt.copy(lintOpts = inOpt.lintOpts.copy(waringsToErrors = true)))
      case v :: tail if v.startsWith("--skip-") => argsLintRead(tail, inOpt.copy(lintOpts = {
        inOpt.lintOpts.copy(skips = inOpt.lintOpts.skips :+ v.replaceFirst("^--skip-", ""))
      }))
      // --
      case string :: Nil => argsLintRead(Nil, inOpt.focusLintInvalids().replace(inOpt.lintOpts.diag.invalids :+ string))
      case string :: tail => argsLintRead(tail, inOpt.focusLintInvalids().replace(inOpt.lintOpts.diag.invalids :+ string))
      case _ => throw new IllegalStateException("not expected")
    }
  }

}

case class OptsApidiff(showApiDiff: Boolean = false, showHelp: Boolean = false,
                       pomOnly: Boolean = false,
                       allModifications: Boolean = false,
                       left: String = null, right: String = null,
                       diag: OptsDiag = OptsDiag.empty()) {

  import Util._

  val isEmpty = left.blank() || right.blank()
  val incompatibleModifications = !allModifications
}

case class OptsDiag(invalids: Seq[String] = Nil)

object OptsDiag {
  def empty() = OptsDiag()
}

case class LintOpts(doLint: Boolean = false, showTimer: Boolean = true, showTimeStamps: Boolean = false,
                    showHelp: Boolean = false, checkPackages: Boolean = true,
                    skips: Seq[String] = Nil, waringsToErrors: Boolean = false,
                    diag: OptsDiag = OptsDiag.empty())


case class OptsDepUp(showDependencyUpdates: Boolean = false, showHelp: Boolean = false,
                     hideLatest: Boolean = true, versionRangeLimit: Integer = 3,
                     hideUpdates: Boolean = false,
                     hideStageVersions: Boolean = true, showLibYears: Boolean = false,
                     changeToLatest: Boolean = false, allowDependencyDowngrades: Boolean = false,
                     timeoutSec: Option[Int] = None,
                     filter: Option[Regex] = None,
                     diag: OptsDiag = OptsDiag.empty())

case class Opts(simpleChars: Boolean = false, diag: OptsDiag = OptsDiag.empty(), showHelp: Boolean = false,
                showUpdateCmd: Boolean = false, versionSet: Option[String] = None, shopGA: Option[String] = None,
                createFeature: Boolean = false, useGerrit: Boolean = true, doUpdate: Boolean = true,
                depUpOpts: OptsDepUp = OptsDepUp(), apiDiff: OptsApidiff = OptsApidiff(),
                useJlineInput: Boolean = false, skipProperties: Seq[String] = Nil,
                colors: Boolean = true, useDefaults: Boolean = false, versionIncrement: Option[Increment] = None,
                lintOpts: LintOpts = LintOpts(), checkOverlapping: Boolean = true,
                checkProjectDeps: Boolean = true,
                showSelfGa: Boolean = false, showStartupDone: Boolean = true, suggestDockerTag: Boolean = false,
                suggestRemoteBranch:Boolean = false,
                isInteractive: Boolean = true, showOpts: Boolean = false, repoSupplier: Opts => RepoZ = Opts.newRepoZ
               ) {
  def newRepo: RepoZ = {
    repoSupplier.apply(this)
  }
}
