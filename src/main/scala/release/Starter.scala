package release

import java.awt.Desktop
import java.io.{BufferedReader, File, PrintStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean

import com.google.common.hash.Hashing
import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import release.Conf.Tracer
import release.Sgit.GitRemote
import release.Xpath.InvalidPomXmlException

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Starter extends App with LazyLogging {
  logger.trace("started vm")

  object TermOs {

    def select(term: String, os: String, simpleChars: Boolean) = term match {
      case "xterm" ⇒ TermOs("xterm", os, simpleChars)
      case "xterm-256color" ⇒ TermOs("xterm-256color", os, simpleChars)
      case "screen-256color" ⇒ TermOs("screen-256color", os, simpleChars)
      case "cygwin" ⇒ TermOs("cygwin", os, simpleChars)
      case t ⇒ throw new IllegalStateException(t)
    }
  }

  case class TermOs(term: String, os: String, simpleChars: Boolean) {
    val isCygwin: Boolean = os == "Cygwin" || term == "cygwin"
    val isMinGw: Boolean = os.contains("MINGW")
  }

  case class FutureError(msg: String, e: Exception)

  implicit class FutureEither[E, A](val wrapped: Future[Either[E, A]])(implicit ec: ExecutionContext) {
    def map[B](f: A ⇒ B): FutureEither[E, B] = wrapped.map(_.map(f))

    def flatMap[B](f: A ⇒ FutureEither[E, B]): FutureEither[E, B] = wrapped.flatMap {
      case Left(s) ⇒ Future(Left(s))
      case Right(a) ⇒ f(a).wrapped
    }
  }

  def futureOf[T](ec: ExecutionContext, fn: ⇒ T): FutureEither[FutureError, T] = {
    new FutureEither[FutureError, T](Future {
      try {
        Right(fn)
      } catch {
        case e: Exception ⇒ Left(FutureError(e.getMessage, e))
      }
    }(ec))(ec)
  }

  def suggestRebase(out: PrintStream, sgit: Sgit, branch: String): () ⇒ Unit = {
    logger.trace("ask for rebase")
    sgit.checkout(branch)
    chooseUpstreamIfUndef(out, sgit, branch)
    if (sgit.isNotDetached) {
      val shouldRebase = sgit.commitIds("@{upstream}", branch)
      if (shouldRebase != Nil) {
        () ⇒ {
          val update = Term.readFromOneOfYesNo(out, "Your branch is " + shouldRebase.size +
            " commits behind or ahead defined upstream. Rebase local branch?")
          if (update == "y") {
            sgit.rebase()
          }

        }
      } else {
        () ⇒ {}
      }
    } else {
      () ⇒ {}
    }
  }

  def fetchGitAndAskForBranch(out: PrintStream, err: PrintStream, noVerify: Boolean,
                              gitBinEnv: Option[String], workDirFile: File, in: BufferedReader): (Sgit, String) = {
    val global = ExecutionContext.global

    def fetchGit(file: File): Sgit = {
      val git = Sgit(file = file, doVerify = noVerify, out = out, err = err, gitBin = gitBinEnv)
      git.fetchAll()
      git
    }


    def askReleaseBranch(): String = {
      def workGit(file: File): Sgit = {
        Sgit(file = file, doVerify = noVerify, out = out, err = err, gitBin = gitBinEnv)
      }

      def suggestCurrentBranch(file: File): String = {
        val git = workGit(file)
        val latestCommit = git.commitIdHead()
        val selectedBraches = git.listBranchesLocal().filter(_.commitId == latestCommit)
        val found = selectedBraches.map(_.branchName.replaceFirst("refs/heads/", "")).filterNot(_ == "HEAD")
        if (found.size != 1) {
          out.println("W: more than one branch found: " + found.mkString(", ") + "; using \"master\"")
          "master"
        } else {
          found.head
        }

      }

      val branch = Term.readFrom(out, "Enter branch name where to start from", suggestCurrentBranch(workDirFile), in)
      val git = workGit(workDirFile)
      val allBranches = git.listBranchNamesAllFull()
      if (!allBranches.contains(branch)) {
        // XXX maybe problematic with "detatched heads" or relative position like "HEAD~2"
        out.println("W: invalid branchname: " + branch + "; try one of: " + allBranches.mkString(", "))
        askReleaseBranch()
      } else {
        branch
      }
    }

    val gitFetchF = futureOf(global, {
      val result = Tracer.msgAround("fetch git", logger, () ⇒ fetchGit(workDirFile))
      result
    })

    val startBranchF = futureOf(global, askReleaseBranch())

    val gitFetchStatusF = futureOf(global, {
      val printFetching = new AtomicBoolean(true)
      while (!gitFetchF.wrapped.isCompleted) {
        Thread.sleep(100)
        if (printFetching.get() && !gitFetchF.wrapped.isCompleted && startBranchF.wrapped.isCompleted) {
          out.print("I: Fetching from remote ")
          printFetching.set(false)
        }
        if (!gitFetchF.wrapped.isCompleted && startBranchF.wrapped.isCompleted) {
          out.print(".")
        }
      }
      if (gitFetchF.wrapped.isCompleted && startBranchF.wrapped.isCompleted && !printFetching.get()) {
        out.println(". done")
      }

    })

    def toResult(implicit ec: ExecutionContext): FutureEither[FutureError, (Sgit, String)] = {
      val result: FutureEither[FutureError, (Sgit, String)] = for {
        _ ← gitFetchStatusF
        git ← gitFetchF
        startBranch ← startBranchF
      } yield (git, startBranch)
      result
    }

    val o = try {
      Await.result(toResult(global).wrapped, Duration.Inf)
    } catch {
      case _: TimeoutException ⇒ throw new TimeoutException("git fetch failed")
    }
    if (o.isLeft) {
      throw o.left.get.e
    } else {
      o.right.get
    }
  }

  case class Opts(simpleChars: Boolean = false, invalids: Seq[String] = Nil, showHelp: Boolean = false,
                  showUpdateCmd: Boolean = false, versionSet: Option[String] = None, shopGA: Option[String] = None,
                  createFeature: Boolean = false, verifyGerrit: Boolean = true, doUpdate: Boolean = true,
                  showDependencyUpdates: Boolean = false, jlineDemo: Boolean = false)

  def argsRead(params: Seq[String], inOpt: Opts): Opts = {
    params.filter(in ⇒ in.trim.nonEmpty) match {
      case Nil ⇒ inOpt
      case "--simple-chars" :: tail ⇒ argsRead(tail, inOpt.copy(simpleChars = true))
      case "--help" :: tail ⇒ argsRead(tail, inOpt.copy(showHelp = true))
      case "-h" :: tail ⇒ argsRead(tail, inOpt.copy(showHelp = true))
      case "--replace" :: tail ⇒ argsRead(tail, inOpt) // handled by shell
      case "--show-update-cmd" :: tail ⇒ argsRead(tail, inOpt.copy(showUpdateCmd = true))
      case "--no-gerrit" :: tail ⇒ argsRead(tail, inOpt.copy(verifyGerrit = false))
      case "--no-update" :: tail ⇒ argsRead(tail, inOpt.copy(doUpdate = false))
      // CMDs
      case "versionSet" :: value :: tail ⇒ argsRead(tail, inOpt.copy(versionSet = Some(value)))
      case "shopGASet" :: value :: tail ⇒ argsRead(tail, inOpt.copy(shopGA = Some(value)))
      case "nothing-but-create-feature-branch" :: tail ⇒ argsRead(tail, inOpt.copy(createFeature = true))
      case "showDependencyUpdates" :: tail ⇒ argsRead(tail, inOpt.copy(showDependencyUpdates = true))
      case "jline" :: tail ⇒ argsRead(tail, inOpt.copy(jlineDemo = true))

      // --
      case string :: Nil ⇒ argsRead(Nil, inOpt.copy(invalids = inOpt.invalids :+ string))
      case string :: tail ⇒ argsRead(tail, inOpt.copy(invalids = inOpt.invalids :+ string))
    }

  }

  def init(argSeq: Seq[String], out: PrintStream, err: PrintStream): Int = {

    if (argSeq.size <= 4) {
      err.println("usage: $0 \"$(dirname $0)\" \"$(pwd)\" \"${os}\" \"${TERM}\" \"${terminal_cols}\" ${argLine}")
      return 1
    }
    out.println(". done")

    val releaseToolPath = argSeq.head
    val releaseToolDir = new File(releaseToolPath).getAbsoluteFile
    val workDir = argSeq(1)
    val workDirFile = new File(workDir).getAbsoluteFile
    val shellWidth = argSeq(4).toInt
    val otherArgs = argSeq.drop(5).filter(_ != null).map(_.trim).toList

    val config = ReleaseConfig.default()
    val opts = argsRead(otherArgs, Opts())
    val termOs: TermOs = TermOs.select(argSeq(3), argSeq(2), opts.simpleChars)
    val showHelp = opts.showHelp
    val showUpdateCmd = opts.showUpdateCmd
    val createFeatureBranch = opts.createFeature
    val verifyGerrit = opts.verifyGerrit
    val versionSetMode = opts.versionSet.isDefined
    val shopGASetMode = opts.shopGA.isDefined

    // TODO --batch ## alles mit default wählen

    if (showHelp || opts.invalids != Nil) {
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
      out.println("--help, -h          => shows this and exits")
      out.println("--simple-chars      => use no drawing chars")
      out.println("--replace           => replaces release jar / only required for development")
      out.println("--no-gerrit         => use this toggle for non gerrit projects")
      out.println()
      out.println("showDependencyUpdates                => shows dependency updates from nexus option")
      out.println("versionSet newVersion                => changes version like maven")
      out.println("shopGASet newGroupIdAndArtifactId    => changes GroupId and ArtifactId for Shops")
      out.println("nothing-but-create-feature-branch    => creates a feature branch and changes pom.xmls")
      out.println()
      out.println("Possible environment variables:")
      out.println("export RELEASE_GIT_BIN = $PATH_TO_GIT_BIN")
      out.println()
      out.println("Your home dir is: " + config.getUserNome())
      return 0
    }
    if (opts.jlineDemo) {
      out.println("jline demo .. ")
      val a = Term.readLine("text a: ")
      out.println("a: " + a)

      val b = Term.readLine("text b: ")
      println("b: " + b)

      val c = Term.readLine("text c: ")
      println("c: " + c)
      System.exit(28)
    }

    val gitBinEnv = config.gitBinEnv()
    lazy val releaseToolGit = Sgit(file = releaseToolDir, gitBin = gitBinEnv, doVerify = false, out = out, err = err)

    val updateCmd: String = {
      val updatePath = if (termOs.isCygwin && !termOs.isMinGw) {
        "$(cygpath -u \"" + releaseToolPath + "\")"
      } else {
        releaseToolPath
      }
      "(cd " + updatePath + " && git rebase -q --autostash || git reset --hard @{upstream} && cd -)"
    }
    if (showUpdateCmd) {
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

    def handleException(t: Throwable): Int = {
      t match {
        case x@(_: Sgit.MissingCommitHookException | _: Sgit.MissingGitDirException |
                _: PomChecker.ValidationException | _: PreconditionsException | _: Sgit.BranchAlreadyExistsException) ⇒ {
          err.println()
          err.println("E: " + x.getMessage)
          1
        }
        case x@(_: TimeoutException) ⇒ {
          err.println()
          err.println("E: User timeout: " + x.getMessage)
          1
        }
        case x@(_: InvalidPomXmlException) ⇒ {
          err.println()
          err.println("E: " + x.getMessage)
          err.println("E: " + x.parent.getMessage)
          1
        }
        case _ ⇒ {
          err.println()
          err.println(t)
          t.printStackTrace(err)
          2
        }
      }

    }

    try {
      val gitAndBranchname = fetchGitAndAskForBranch(out, err, verifyGerrit, gitBinEnv, workDirFile, Console.in)

      def suggestLocalNotesReviewRemoval(activeGit: Sgit): Unit = {
        // git config --add remote.origin.fetch refs/notes/review:refs/notes/review
        // git config --add remote.origin.fetch refs/notes/*:refs/notes/*
        // git fetch
        if (activeGit.listRefNames().contains("refs/notes/review")) {
          val result = Term.readFromOneOfYesNo(out, "Ref: " + "refs/notes/review" + " found." +
            " This ref leads to an unreadable local history. Do you want to remove them?")
          if (result == "y") {
            val configRefs = activeGit.configGetAll("remote.origin.fetch").getOrElse(Nil)
            configRefs.filter(_.startsWith("refs/notes/"))
              .map(_.replaceAll("\\*", "\\\\*"))
              .foreach(in ⇒ activeGit.configRemove("remote.origin.fetch", in))
          }
          activeGit.deleteRef("refs/notes/review")
        }

      }

      val git = gitAndBranchname._1
      suggestLocalNotesReviewRemoval(git)
      val startBranch = gitAndBranchname._2
      val askForRebase = suggestRebase(out, git, startBranch)
      logger.trace("readFromPrompt")
      Tracer.withFn(logger, () ⇒ "local branches: " + git.listBranchNamesLocal())
      if (versionSetMode) {
        Release.checkLocalChanges(git, startBranch)
        val mod = PomMod.of(workDirFile, err)
        val version = opts.versionSet.get
        val versionWithoutSnapshot = Term.removeSnapshot(version)
        mod.changeVersion(versionWithoutSnapshot + "-SNAPSHOT")
        mod.writeTo(workDirFile)
        out.println("You have local changes")
      } else if (shopGASetMode) {
        Release.checkLocalChanges(git, startBranch)
        val mod = PomMod.of(workDirFile, err)
        val groupIdArtifactIdLine = opts.shopGA.get
        mod.changeShopGroupArtifact(groupIdArtifactIdLine)
        mod.writeTo(workDirFile)
        out.println("You have local changes")
      } else if (createFeatureBranch) {
        FeatureBranch.work(workDirFile, out, err, git, startBranch, askForRebase, releaseToolGit.headStatusValue(), config)
      } else {
        Release.work(workDirFile, out, err, askForRebase, startBranch,
          git, opts.showDependencyUpdates, termOs, shellWidth, releaseToolGit.headStatusValue(), config)
      }

      return 0
    } catch {
      case t: Throwable ⇒ {
        return handleException(t)
      }
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
      .map(in ⇒ jenkinsBase + "/job/" + in + "-tag/")
  }

  def tagBuildUrl(git: Sgit, jenkinsBase: String): Option[String] = {
    // TODO write intial jenkins url to ${HOME}/.nm-release-config; else read
    val list: Seq[GitRemote] = git.listRemotes
    val path = transformRemoteToBuildUrl(list, jenkinsBase)
    if (path.isDefined) {
      try {
        val httpclient = HttpClients.createDefault
        val httpGet = new HttpGet(path.get)
        val response = httpclient.execute(httpGet)
        try {
          val statusCode = response.getStatusLine.getStatusCode
          if (statusCode == 200) {
            path
          } else {
            println("W: invalid response for (" + statusCode + ") " + path.get + " <- " + list)
            println("W: > please create an ISPS ticket")
            None
          }
        } finally {
          response.close()
        }
      } catch {
        case e: Exception ⇒ println("W: http problem: " + e.getClass.getCanonicalName + " => " + e.getMessage); None
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
      case e: Exception ⇒ e.printStackTrace()
    }
  }

  def chooseUpstreamIfUndef(out: PrintStream, sgit: Sgit, branch: String): Unit = {
    val upstream = sgit.findUpstreamBranch()
    if (upstream.isEmpty && sgit.isNotDetached) {
      val newUpstream = Term.readChooseOneOfOrType(out, "No upstream found, please set", Seq("origin/master", "origin/" + branch).distinct)
      val remoteBranchNames = sgit.listBranchNamesRemote()
      if (remoteBranchNames.contains(newUpstream)) {
        sgit.setUpstream(newUpstream)
        return
      } else {
        out.println("W: unknown upstream branch; known are " + remoteBranchNames.mkString(", "))
        chooseUpstreamIfUndef(out, sgit, branch)
      }
    }
  }

  def sign(): String = {
    Hashing.sha1()
      .hashString(LocalDateTime.now().toString, StandardCharsets.UTF_8)
      .toString
  }

  def addExitFn(msg: String, fn: () ⇒ Unit): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread(() ⇒ {
      fn.apply()
    }))
  }

  class PreconditionsException(msg: String) extends RuntimeException(msg)

  System.exit(init(args, System.out, System.err))

}
