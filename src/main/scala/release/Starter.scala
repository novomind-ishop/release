package release

import com.google.common.base.Strings
import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import release.Conf.Tracer
import release.Sgit.GitRemote
import release.Util.pluralize
import release.Xpath.InvalidPomXmlException

import java.awt.Desktop
import java.io.{File, PrintStream}
import java.net.URI
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.matching.Regex

object Starter extends LazyLogging {

  case class FutureError(msg: String, e: Exception)

  implicit class FutureEither[E, A](val wrapped: Future[Either[E, A]])(implicit ec: ExecutionContext) {
    def map[B](f: A => B): FutureEither[E, B] = wrapped.map(_.map(f))

    def flatMap[B](f: A => FutureEither[E, B]): FutureEither[E, B] = wrapped.flatMap {
      case Left(s) => Future(Left(s))
      case Right(a) => f(a).wrapped
    }
  }

  def futureOf[T](ec: ExecutionContext, fn: => T): FutureEither[FutureError, T] = {
    new FutureEither[FutureError, T](Future {
      try {
        Right(fn)
      } catch {
        case e: Exception => Left(FutureError(e.getMessage, e))
      }
    }(ec))(ec)
  }

  def suggestRebase(sys: Term.Sys, sgit: Sgit, branch: String, opts: Opts): () => Unit = {
    logger.trace("ask for rebase")
    sgit.checkout(branch)
    chooseUpstreamIfUndef(sys, sgit, branch, opts)
    if (sgit.isNotDetached) {
      val commintsBehindOrAhead = sgit.commitIds("@{upstream}", branch)
      if (commintsBehindOrAhead != Nil) {
        () => {
          val upstreamCommit = sgit.commitId("@{upstream}")
          val upstreamName = sgit.findUpstreamBranch().get
          if (commintsBehindOrAhead.head == upstreamCommit) {
            val text = "Your branch is " + commintsBehindOrAhead.size +
              s" ${"commit".pluralize(commintsBehindOrAhead.size)} behind defined upstream $upstreamName. Rebase local branch?"
            val update = Term.readFromOneOfYesNo(sys, text, opts)
            if (update == "y") {
              sgit.rebase()
            }
          } else {
            val text = "Your branch is " + commintsBehindOrAhead.size +
              s" ${"commit".pluralize(commintsBehindOrAhead.size)} ahead of defined upstream $upstreamName. Abort?"
            val abort = Term.readFromOneOfYesNo(sys, text, opts)
            if (abort == "y") {
              System.exit(1)
            }
          }

        }
      } else {
        () => {}
      }
    } else {
      () => {}
    }
  }

  def fetchGitAndAskForBranch(sys: Term.Sys, noVerify: Boolean,
                              gitBinEnv: Option[String], workDirFile: File, opts: Opts,
                              skipFetch: Boolean, skipAskForBranchFetch: Boolean): (Sgit, String) = {
    val global = ExecutionContext.global

    def unfechtedGit(file: File): Sgit = {
      Sgit(file = file, doVerify = noVerify, out = sys.out, err = sys.err, gitBin = gitBinEnv, opts = opts)
    }

    def fetchedGit(file: File): Sgit = {
      val git = unfechtedGit(file)
      git.fetchAll()
      git
    }

    @tailrec
    def askReleaseBranch(): String = {
      def workGit(file: File): Sgit = {
        Sgit(file = file, doVerify = noVerify, out = sys.out, err = sys.err, gitBin = gitBinEnv, opts = opts)
      }

      def suggestCurrentBranch(file: File): String = {
        val git = workGit(file)
        val latestCommit = git.commitIdHead()
        val selectedBraches = git.listBranchesLocal().filter(_.commitId == latestCommit)
        val found = selectedBraches.map(_.branchName.replaceFirst("refs/heads/", "")).filterNot(_ == "HEAD")
        if (found.size != 1) {
          val selected: String = found match {
            case k if k.contains("main") => "main"
            case k if k.contains("master") => "master"
            case _ => "main"
          }
          sys.out.println("W: more than one branch found: " + found.mkString(", ") + s"; using \"${selected}\"")
          selected
        } else {
          found.head
        }

      }

      val branch = if (skipAskForBranchFetch) {
        suggestCurrentBranch(workDirFile)
      } else {
        Term.readFrom(sys, "Enter branch name where to start from", suggestCurrentBranch(workDirFile), opts)
      }
      val git = workGit(workDirFile)
      val allBranches = git.listBranchNamesAllFull()

      if (!allBranches.contains(branch) && git.commitIdOpt(branch).isEmpty) {
        sys.out.println("W: invalid branchname: " + branch + "; try one of: " + allBranches.mkString(", ") + " or sha1 or relative name")
        askReleaseBranch()
      } else {
        branch
      }
    }

    val gitFetchF = futureOf(global, {
      if (skipFetch) {
        Tracer.msgAround("unfetch git", logger, () => unfechtedGit(workDirFile))
      } else {
        val result = Tracer.msgAround("fetch git", logger, () => fetchedGit(workDirFile))
        result
      }
    })

    val startBranchF = futureOf(global, askReleaseBranch())

    val gitFetchStatusF = futureOf(global, {
      if (!skipFetch) {
        val printFetching = new AtomicBoolean(true)
        while (!gitFetchF.wrapped.isCompleted) {
          Thread.sleep(100)
          if (printFetching.get() && !gitFetchF.wrapped.isCompleted && startBranchF.wrapped.isCompleted) {
            sys.out.print("I: Fetching from remote ")
            printFetching.set(false)
          }
          if (!gitFetchF.wrapped.isCompleted && startBranchF.wrapped.isCompleted) {
            sys.out.print(".")
          }
        }
        if (gitFetchF.wrapped.isCompleted && startBranchF.wrapped.isCompleted && !printFetching.get()) {
          sys.out.println(". done (a)")
        }
      }

    })

    def toResult(implicit ec: ExecutionContext): FutureEither[FutureError, (Sgit, String)] = {
      val result: FutureEither[FutureError, (Sgit, String)] = for {
        _ <- gitFetchStatusF
        git <- gitFetchF
        startBranch <- startBranchF
      } yield (git, startBranch)
      result
    }

    val o: Either[FutureError, (Sgit, String)] = try {
      Await.result(toResult(global).wrapped, Duration.Inf)
    } catch {
      case _: TimeoutException => throw new TimeoutException("git fetch failed")
    }
    if (o.isLeft) {
      throw o.swap.getOrElse(null).e
    } else {
      o.getOrElse(null)
    }
  }

  case class OptsApidiff(showApiDiff: Boolean = false, showHelp: Boolean = false,
                         pomOnly: Boolean = false,
                         allModifications: Boolean = false,
                         left: String = null, right: String = null,
                         invalids: Seq[String] = Nil) {
    val isEmpty = left.blank() || right.blank()
    val incompatibleModifications = !allModifications
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
      case string :: Nil => argsApiDiffRead(Nil, inOpt.copy(apiDiff = inOpt.apiDiff.copy(invalids = inOpt.apiDiff.invalids :+ string)))
      case string :: tail => argsApiDiffRead(tail, inOpt.copy(apiDiff = inOpt.apiDiff.copy(invalids = inOpt.apiDiff.invalids :+ string)))
    }
  }

  case class LintOpts(doLint: Boolean = false, showTimer: Boolean = true, showHelp: Boolean = false,
                      skips: Seq[String] = Nil, waringsToErrors:Boolean = false, invalids: Seq[String] = Nil)

  @tailrec
  def argsLintRead(params: Seq[String], inOpt: Opts): Opts = {
    params.filter(in => in.trim.nonEmpty) match {
      case Nil => inOpt.copy(lintOpts = inOpt.lintOpts.copy(doLint = true), showStartupDone = false)
      case "--help" :: tail => argsLintRead(tail, inOpt.copy(lintOpts = inOpt.lintOpts.copy(showHelp = true)))
      case "-h" :: tail => argsLintRead(tail, inOpt.copy(lintOpts = inOpt.lintOpts.copy(showHelp = true)))
      case "--strict" :: tail => argsLintRead(tail, inOpt.copy(lintOpts = inOpt.lintOpts.copy(waringsToErrors = true)))
      case v :: tail if v.startsWith("--skip-") => argsLintRead(tail, inOpt.copy(lintOpts = {
        inOpt.lintOpts.copy(skips = inOpt.lintOpts.skips :+ v.replaceFirst("^--skip-", ""))
      }))
      // --
      case string :: Nil => argsLintRead(Nil, inOpt.copy(lintOpts = inOpt.lintOpts.copy(invalids = inOpt.lintOpts.invalids :+ string)))
      case string :: tail => argsLintRead(tail, inOpt.copy(lintOpts = inOpt.lintOpts.copy(invalids = inOpt.lintOpts.invalids :+ string)))
    }
  }

  case class OptsDepUp(showDependencyUpdates: Boolean = false, showHelp: Boolean = false,
                       hideLatest: Boolean = true, versionRangeLimit: Integer = 3,
                       hideStageVersions: Boolean = true, showLibYears: Boolean = false,
                       changeToLatest: Boolean = false,
                       filter: Option[Regex] = None,
                       invalids: Seq[String] = Nil)

  @tailrec
  def argsDepRead(params: Seq[String], inOpt: Opts): Opts = {
    params.filter(in => in.trim.nonEmpty) match {
      case Nil => inOpt
      case "--help" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showHelp = true)))
      case "-h" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showHelp = true)))
      case "--matches" :: pattern :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(filter = Some(pattern.r))))
      case "--no-filter" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts
        .copy(hideStageVersions = false, hideLatest = false)))
      case "--no-omit" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(versionRangeLimit = Integer.MAX_VALUE)))
      case "--show-libyears" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showLibYears = true)))
      case "--create-patch" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(changeToLatest = true)))
      // --
      case string :: Nil => argsDepRead(Nil, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(invalids = inOpt.depUpOpts.invalids :+ string)))
      case string :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(invalids = inOpt.depUpOpts.invalids :+ string)))
    }
  }

  case class Opts(simpleChars: Boolean = false, invalids: Seq[String] = Nil, showHelp: Boolean = false,
                  showUpdateCmd: Boolean = false, versionSet: Option[String] = None, shopGA: Option[String] = None,
                  createFeature: Boolean = false, useGerrit: Boolean = true, doUpdate: Boolean = true,
                  depUpOpts: OptsDepUp = OptsDepUp(), apiDiff: OptsApidiff = OptsApidiff(),
                  useJlineInput: Boolean = true, skipProperties: Seq[String] = Nil,
                  colors: Boolean = true, useDefaults: Boolean = false, versionIncrement: Option[Increment] = None,
                  lintOpts: LintOpts = LintOpts(), checkOverlapping: Boolean = true,
                  checkProjectDeps: Boolean = true,
                  showSelfGa: Boolean = false, showStartupDone: Boolean = true, suggestDockerTag: Boolean = false,
                  isInteractive: Boolean = true, showOpts: Boolean = false
                 )

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
      case (_, _) :: tail => envRead(tail, inOpt)
    }
  }

  @tailrec
  def argsRead(params: Seq[String], inOpt: Opts, envs: Map[String, String] = Util.systemEnvs()): Opts = {
    if (envs.isEmpty) {
      params.filter(in => in.trim.nonEmpty) match {
        case Nil => inOpt
        case "--simple-chars" :: tail => argsRead(tail, inOpt.copy(simpleChars = true))
        case "--help" :: tail => argsRead(tail, inOpt.copy(showHelp = true))
        case "-h" :: tail => argsRead(tail, inOpt.copy(showHelp = true))
        case "--replace" :: tail => argsRead(tail, inOpt) // handled by shell
        case "--show-update-cmd" :: tail => argsRead(tail, inOpt.copy(showUpdateCmd = true, showStartupDone = false))
        case "--no-gerrit" :: tail => argsRead(tail, inOpt.copy(useGerrit = false))
        case "--no-update" :: tail => argsRead(tail, inOpt.copy(doUpdate = false))
        case "--no-interactive" :: tail => argsRead(tail, inOpt.copy(isInteractive = false))
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
        case "versionSet" :: value :: _ => argsRead(Nil, inOpt.copy(versionSet = Some(value)))
        case "shopGASet" :: value :: _ => argsRead(Nil, inOpt.copy(shopGA = Some(value)))
        case "nothing-but-create-feature-branch" :: _ => argsRead(Nil, inOpt.copy(createFeature = true))
        case "showDependencyUpdates" :: tail => argsDepRead(tail, inOpt.copy(depUpOpts = inOpt.depUpOpts.copy(showDependencyUpdates = true)))

        // --
        case string :: Nil => argsRead(Nil, inOpt.copy(invalids = inOpt.invalids :+ string))
        case string :: tail => argsRead(tail, inOpt.copy(invalids = inOpt.invalids :+ string))
      }
    } else {
      argsRead(params, envRead(envs.toSeq, inOpt), Map.empty)
    }

  }

  def connectLeftRight(in: (Seq[ProjectMod.Gav3], Seq[ProjectMod.Gav3])): Seq[(Seq[ProjectMod.Gav3], Seq[ProjectMod.Gav3])] = {
    val changed = Util.symmetricDiff(in._1, in._2)
      .sortBy(_.toString)
    val changedLeft = in._1.filter(s => changed.contains(s))
    val changedRight = in._2.filter(s => changed.contains(s))

    val groupedLeft = changedLeft.groupBy(x => (x.groupId, x.artifactId))
    val groupedRight = changedRight.groupBy(x => (x.groupId, x.artifactId))
    val gav2s = changed.map(x => (x.groupId, x.artifactId)).distinct
    val oo = gav2s.map(g2 => (groupedLeft.getOrElse(g2, Nil), groupedRight.getOrElse(g2, Nil)))

    oo
  }

  def compressToGav(self: Seq[ProjectMod.Gav3], properties: Map[String, String])(in: Seq[ProjectMod.Dep]): Seq[ProjectMod.Gav3] = {
    val rep = PomMod.replaceProperty(properties, true) _
    val all = in.map(_.gav())
      .filterNot(_.scope == "test")
      .map(_.simpleGav())
      .sortBy(_.toString).distinct.map(g => {
      g.copy(groupId = rep(g.groupId), artifactId = rep(g.artifactId), version = g.version.map(rep))
    })
    val blankVersions = all.filter(e => e.version.isDefined && e.version.get.blank())
    val noBlankVersions = all.filterNot(e => e.version.isDefined && e.version.get.blank()).map(_.copy(version = None))
    val remove = blankVersions.filter(bg => noBlankVersions.contains(bg.copy(version = None)))
    all.filterNot(g => remove.contains(g)).filterNot(x => self.contains(x))
  }

  def showDemoChars(inOpt: Opts): Opts = {
    val sys = Term.Sys.default
    sys.out.println()
    sys.out.println("\u001B[31m" + "This text is red!" + "\u001B[0m")
    val r = Term.readFrom(sys, "test press enter",
      "u200B(\u200B),u0009(\u0009),u00A0(\u00A0),u1680(\u1680),,,u2012(\u2012),u2013(\u2013)",
      inOpt)
    sys.out.print("\u001B[30;45m")
    sys.out.println("demo: " + r)
    sys.out.print("\u001B[0m")
    System.exit(3)
    null
  }

  def handleException(err: PrintStream, t: Throwable): Int = {
    t match {
      case x@(_: Sgit.MissingCommitHookException | _: Sgit.MissingGitDirException | _: Sgit.TerminatedByCtrlCException |
              _: PomChecker.ValidationException | _: PreconditionsException | _: Sgit.BranchAlreadyExistsException) => {
        err.println()
        err.println("E: " + x.getMessage)
        x.printStackTrace()
        1
      }
      case x@(_: TimeoutException) => {
        err.println()
        err.println("E: User timeout: " + x.getMessage)
        1
      }
      case x@(_: InvalidPomXmlException) => {
        err.println()
        err.println("E: " + x.getMessage)
        err.println("E: " + x.parent.getMessage)
        1
      }
      case _ => {
        err.println()
        err.println(t)
        t.printStackTrace(err)
        2
      }
    }

  }

  def init(argSeq: Seq[String], sys: Term.Sys): Int = {

    val err = sys.err
    val out = sys.out
    if (argSeq.size <= 4) {
      err.println("usage: $0 \"$(dirname $0)\" \"$(pwd)\" \"${os}\" \"${TERM}\" \"${terminal_cols}\" \"${HOME}\" ${argLine}")
      return 1
    }
    val releaseToolPath = argSeq.head

    val releaseToolDir = new File(releaseToolPath).getAbsoluteFile
    val workDir = argSeq(1)
    val workDirFile = new File(workDir).getAbsoluteFile
    val shellWidth = argSeq(4).toInt

    val otherArgs = argSeq.drop(7).filter(_ != null).map(_.trim).toList
    val opts = argsRead(otherArgs, Opts())
    if (opts.showOpts) {
      out.println(Util.show(opts))
    }
    val interactiveShell = argSeq(5).toBoolean && opts.isInteractive
    if (opts.showStartupDone) {
      out.println(". done (b)")
    }
    val config = ReleaseConfig.default(opts.useDefaults)
    val termOs: Term = Term.select(argSeq(3), argSeq(2), opts.simpleChars, interactiveShell)

    val home = Term.Os.getCurrent match {
      case Term.Os.Windows => argSeq(5).replace('/', '\\')
      case _ => argSeq(5)
    }


    // TODO --batch ## alles mit default wählen

    if (opts.showHelp || opts.invalids != Nil) {
      if (opts.invalids != Nil) {
        out.println("Invalid options:")
        out.println(opts.invalids.mkString(", "))
        out.println()
      }
      out.println("Usage: release [OPTION] [CMD] ...")
      out.println("Note: Calling release without any options creates a normal release.")
      out.println("All options are non-mandatory.")
      out.println()
      out.println("Possible options:")
      out.println("--help, -h            => shows this and exits")
      out.println("--no-gerrit           => use this toggle for non gerrit projects")
      out.println("--no-interactive      => suppresses startup messages")
      out.println("--skip-property value => if you get false positives with property definitions")
      out.println("--defaults            => do not read ${HOME}/.ishop-release")
      out.println()
      out.println("--simple-chars        => use no drawing chars")
      out.println("--no-color            => use no color")
      out.println("--no-jline            => if you have problems with terminal inputs, try this to read from Stdin")
      out.println("--replace             => replaces release jar / only required for development")
      out.println()
      out.println("showDependencyUpdates                => shows dependency updates from nexus option")
      out.println("versionSet newVersion                => changes version like maven")
      out.println("shopGASet newGroupIdAndArtifactId    => changes GroupId and ArtifactId exclusively for Shops")
      out.println("nothing-but-create-feature-branch    => creates a feature branch and changes pom.xmls")
      out.println("apidiff                              => compares two versions of this repo")
      out.println("lint                                 => check project for release problems (read-only),")
      out.println("                                        reads environment variables CI_COMMIT_REF_NAME and CI_COMMIT_TAG")
      out.println("showSelf                             => a list of groupId:artifactId of current project")
      out.println("suggest-docker-tag                   => use with '--no-interactive', reads environment variables")
      out.println("                                        CI_COMMIT_REF_NAME and CI_COMMIT_TAG")
      out.println()
      out.println("Possible environment variables:")
      out.println("export RELEASE_GIT_BIN=$PATH_TO_GIT_EXECUTABLE")
      out.println("export RELEASE_NO_GERRIT=true" + Util.systemEnvs().get("RELEASE_NO_GERRIT").map(k => s" # ${k}").getOrElse(""))
      out.println()
      out.println("Your home dir is: " + config.getUserHome(home))
      out.println(s"InteractiveShell: ${interactiveShell}")
      return 0
    }

    if (opts.depUpOpts.showHelp || opts.depUpOpts.invalids != Nil) {
      if (opts.depUpOpts.invalids != Nil) {
        out.println("Invalid showDependencyUpdates options:")
        out.println(opts.depUpOpts.invalids.mkString(", "))
        out.println()
      }
      out.println("Usage: release showDependencyUpdates [OPTION] [CMD] ...")
      out.println()
      out.println("Possible options:")
      out.println("--help, -h            => shows this and exits")
      out.println("--no-filter           => do not hide unwanted updates")
      out.println("--show-libyears       => see https://libyear.com/")
      out.println("--create-patch        => BETA - change pom.xmls")
      out.println("--matches             => regex to filter dependecy update GAVs e.g. --matches \".*guava.*\"")
      return 0
    }

    if (opts.apiDiff.showHelp || opts.apiDiff.invalids != Nil || opts.apiDiff.showApiDiff && opts.apiDiff.isEmpty) {
      if (opts.apiDiff.invalids != Nil) {
        out.println("Invalid apidiff options:")
        out.println(opts.apiDiff.invalids.mkString(", "))
        out.println()
      }
      out.println("Usage: release apidiff [OPTION] [VERSION_A] [VERSION_B]")
      out.println()
      out.println("Possible options:")
      out.println("--all                 => show unfiltered diff")
      out.println("--pom-only            => shows a diff only of direct dependencies")
      out.println("--help, -h            => shows this and exits")
      return 0
    }

    val gitBinEnv = config.gitBinEnv()
    lazy val releaseToolGit = Sgit(file = releaseToolDir, gitBin = gitBinEnv, doVerify = false, out = out, err = err, opts = opts)

    val updateCmd: String = {
      val updatePath = if (termOs.isCygwin && !termOs.isMinGw) {
        "$(cygpath -u \"" + releaseToolPath + "\")"
      } else {
        releaseToolPath
      }
      "(cd " + updatePath + " && git rebase -q --autostash || git reset --hard @{upstream} && cd -)"
    }

    val createFeatureBranch = opts.createFeature
    val verifyGerrit = opts.useGerrit
    val versionSetMode = opts.versionSet.isDefined
    val shopGASetMode = opts.shopGA.isDefined

    if (opts.showUpdateCmd) {
      out.println(updateCmd)
      return 0
    } else if (opts.doUpdate && releaseToolGit.tryFetchAll().isSuccess) {
      val headVersion = releaseToolGit.commitIdHead()
      val remoteMasterVersion = releaseToolGit.commitId("origin/master")
      if (headVersion != remoteMasterVersion) {
        out.println("Production Version: " + remoteMasterVersion)
        out.println("Your Version:       " + headVersion)
        out.println("Please update your release tool:")
        out.println(updateCmd)
        return 99
      }

    }
    if (opts.suggestDockerTag) {

      val file: File = new File(".").getAbsoluteFile
      val pomModTry = PomMod.withRepoTry(file, opts, Repo.of(opts), failureCollector = None)
      val selfV = pomModTry.map(pm => pm.selfVersion).toOption
      val result = SuggestDockerTag.suggest(System.getenv("CI_COMMIT_REF_NAME"), System.getenv("CI_COMMIT_TAG"), selfV)
      out.println(result._1)
      return result._2
    }
    if (opts.lintOpts.doLint) {
      return Lint.run(out, err, opts, Repo.of(opts), Util.systemEnvs())
    }
    if (opts.showSelfGa) {
      val file: File = new File(".").getAbsoluteFile
      val pomModTry = PomMod.withRepoTry(file, opts, Repo.of(opts), failureCollector = None)
      pomModTry.get.selfDepsModGavs().foreach(gav => out.println(gav.formatted))
      return 0
    }
    if (opts.apiDiff.showApiDiff) {
      ApiDiff.apidiff(opts, opts.apiDiff.left, opts.apiDiff.right)
      return 0
    }
    try {
      val gitAndBranchname = fetchGitAndAskForBranch(sys, verifyGerrit, gitBinEnv, workDirFile, opts,
        skipFetch = !termOs.isInteractice, skipAskForBranchFetch = !termOs.isInteractice)

      def suggestLocalNotesReviewRemoval(activeGit: Sgit): Unit = {
        // git config --add remote.origin.fetch refs/notes/review:refs/notes/review
        // git config --add remote.origin.fetch refs/notes/*:refs/notes/*
        // git fetch
        if (activeGit.listRefNames().contains("refs/notes/review")) {
          val result = Term.readFromOneOfYesNo(sys, "Ref: " + "refs/notes/review" + " found." +
            " This ref leads to an unreadable local history. Do you want to remove them?", opts)
          if (result == "y") {
            val configRefs = activeGit.configGetLocalAllSeq("remote.origin.fetch")
            configRefs.filter(_.startsWith("refs/notes/"))
              .map(_.replaceAll("\\*", "\\\\*"))
              .foreach(in => activeGit.configRemoveLocal("remote.origin.fetch", in))
          }
          activeGit.deleteRef("refs/notes/review")
        }
        val autoCrlf = activeGit.configGetGlobalAllSeq("core.autocrlf")
        if (autoCrlf != Seq("input")) {
          val msg = "You have an unexpected core.autocrlf setting (%s) in your global .gitconfig. ".format(autoCrlf.mkString(", ")) +
            "Please set to '$ git config --global core.autocrlf input'."
          val options = Seq("Change core.autocrlf globaly to 'input'", "Abort release", "Continue")

          @tailrec
          def autoCrlfCheck(): Unit = {
            val result = Term.readChooseOneOf(sys, msg, options, opts)
            if (result == options(1)) {
              System.exit(1)
            } else if (result == options(0)) {
              activeGit.configSetGlobal("core.autocrlf", "input")
            } else if (result == options(2)) {
              // nothing
            } else {
              autoCrlfCheck()
            }
          }

          autoCrlfCheck()

        }

      }

      val git = gitAndBranchname._1
      suggestLocalNotesReviewRemoval(git)
      val startBranch = gitAndBranchname._2
      val askForRebase: () => Unit = if (interactiveShell) {
        suggestRebase(sys, git, startBranch, opts)
      } else {
        () => {}
      }
      logger.trace("readFromPrompt")
      Tracer.withFn(logger, () => "local branches: " + git.listBranchNamesLocal())
      if (versionSetMode) {
        Release.checkLocalChanges(git, startBranch)
        val mod = PomMod.of(workDirFile, opts, failureCollector = None)
        val version = opts.versionSet.get
        val versionWithoutSnapshot = Term.removeTrailingSnapshots(version)
        mod.changeVersion(versionWithoutSnapshot + "-SNAPSHOT")
        mod.writeTo(workDirFile)
        if (git.localChanges() != Nil) {
          out.println("I: Version successfully changed. You have local changes")
        } else {
          out.println("I: Nothing changed")
        }
      } else if (shopGASetMode) {
        Release.checkLocalChanges(git, startBranch)
        val mod = PomMod.of(workDirFile, opts, failureCollector = None)
        val groupIdArtifactIdLine = opts.shopGA.get
        mod.changeShopGroupArtifact(groupIdArtifactIdLine)
        mod.writeTo(workDirFile)
        out.println("I: GroupId and ArtifactId successfully changed. You have local changes")
      } else if (createFeatureBranch) {
        FeatureBranch.work(workDirFile, sys, git, startBranch, askForRebase, releaseToolGit.headStatusValue(), config, opts)
      } else {
        lazy val repo = Repo.of(opts)
        Release.work(workDirFile, sys, askForRebase, startBranch,
          git, termOs, shellWidth, () => releaseToolGit.headStatusValue(), config, repo, opts)
      }

      return 0
    } catch {
      case t: Throwable => {
        return handleException(err, t)
      }
    }

  }

  def transformRemoteToBuildUrls(list: Seq[Sgit.GitRemote], jenkinsBase: String, releaseName: String): Seq[String] = {
    transformRemoteToBuildUrl(list, jenkinsBase).toSeq ++
      transformRemoteToBuildUrlVersion(list, jenkinsBase, releaseName).toSeq
  }

  def transformRemoteToBuildUrlVersion(list: Seq[Sgit.GitRemote], jenkinsBase: String, releaseName: String): Option[String] = {
    val extract = releaseName.replaceFirst("^[^0-9]", "").replaceFirst("[^0-9].*", "")
    if (extract.isEmpty) {
      None
    } else {
      transformRemoteToBuildUrl(list, jenkinsBase).map(r => {
        r.replaceFirst("/$", s"-${extract}/")
      })
    }
  }

  def transformRemoteToBuildUrl(list: Seq[Sgit.GitRemote], jenkinsBase: String): Option[String] = {
    val origin = list.find(_.name == "origin")
    val remote = origin.map(_.position)
    remote.map(_.replaceFirst("^[^/]+//", ""))
      .map(_.replaceFirst("^[^/]+/", ""))
      .map(_.replaceFirst(".git$", ""))
      .map(_.replaceAll("[/]", "-"))
      .map(_.toLowerCase)
      .map(in => jenkinsBase + "/job/" + in + "-tag/")
  }

  def preCheck(path: String): Option[String] = {

    try {
      val httpclient = HttpClients.createDefault
      val httpGet = new HttpGet(path)
      val response = httpclient.execute(httpGet)
      try {
        val statusCode = response.getStatusLine.getStatusCode
        if (statusCode == 200) {
          Some(path)
        } else {
          None
        }
      } finally {
        response.close()
      }
    } catch {
      case e: Exception => println("W: http problem: " + e.getClass.getCanonicalName + " => " + e.getMessage); None
    }

  }

  def tagBuildUrl(git: Sgit, jenkinsBase: String, releaseName: String): Option[String] = {
    // TODO write intial jenkins url to ${HOME}/.nm-release-config; else read
    val list: Seq[GitRemote] = git.listRemotes()
    val paths = transformRemoteToBuildUrls(list, jenkinsBase, releaseName).distinct
    if (paths.nonEmpty) {
      val checked = paths.flatMap(preCheck)
      if (checked.isEmpty) {
        println("W: invalid response for " + paths + " <- " + list)
        println("W: > please create an ISBO ticket")
        None
      } else {
        Some(checked.head)
      }
    } else {
      None
    }

  }

  def openInDefaultBrowser(url: String): Unit = {
    try {
      if (Desktop.isDesktopSupported) {
        Desktop.getDesktop.browse(new URI(url))
      }
    } catch {
      case uoe: UnsupportedOperationException => System.err.println(uoe.getClass.getName + " " + uoe.getMessage)
      case e: Exception => e.printStackTrace()
    }
  }

  @tailrec
  def chooseUpstreamIfUndef(sys: Term.Sys, sgit: Sgit, branch: String, opts: Opts): Unit = {
    val upstream = sgit.findUpstreamBranch()
    if (upstream.isEmpty && sgit.isNotDetached) {
      val newUpstream = Term.readChooseOneOfOrType(sys, "No upstream found, please set",
        Seq("origin/master", "origin/" + branch).distinct, opts)
      val remoteBranchNames = sgit.listBranchNamesRemote()
      if (remoteBranchNames.contains(newUpstream)) {
        sgit.setUpstream(newUpstream)
        return
      } else {
        sys.out.println("W: unknown upstream branch; known are " + remoteBranchNames.mkString(", "))
        chooseUpstreamIfUndef(sys, sgit, branch, opts)
      }
    }
  }

  def sign(sgit: Sgit): String = {
    Util.hashSha1(sgit.diffSafe().mkString("\n"))
  }

  def addExitFn(msg: String, fn: () => Unit): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      fn.apply()
    }))
  }

  class PreconditionsException(msg: String) extends RuntimeException(msg)

  def main(args: Array[String]): Unit = {
    logger.trace("started vm")
    System.exit(init(args.toIndexedSeq, Term.Sys.default))
  }

}
