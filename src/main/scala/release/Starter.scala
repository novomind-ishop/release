package release

import java.awt.Desktop
import java.io.{File, PrintStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.concurrent.{TimeUnit, TimeoutException}

import com.google.common.hash.Hashing
import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import release.Sgit.GitRemote
import release.Xpath.InvalidPomXmlException

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Properties

object Starter extends App with LazyLogging {

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

  def init(argSeq: Seq[String], out: PrintStream, err: PrintStream): Int = {

    if (argSeq.size <= 4) {
      err.println("usage: $0 \"$(dirname $0)\" \"$(pwd)\" \"${os}\" \"${TERM}\" \"${terminal_cols}\" ${argLine}")
      return 1
    }

    val releaseToolPath = argSeq.head
    val releaseToolDir = new File(releaseToolPath).getAbsoluteFile
    val workDir = argSeq(1)
    val workDirFile = new File(workDir).getAbsoluteFile
    val shellWidth = argSeq(4).toInt
    val restArgs = argSeq.drop(5).filter(_ != null).map(_.trim).toList
    val debugKey = restArgs.contains("debug")

    def debug(message: String): Unit = {
      debugFn(() ⇒ message)
    }

    def debugFn(message: () ⇒ String): Unit = {
      if (debugKey) {
        out.println("DEBUG: " + message.apply())
      }
    }
    val config = ReleaseConfig.default()
    debug("init")
    val termOs: TermOs = TermOs.select(argSeq(3), argSeq(2), restArgs.contains("simpleChars"))
    val dependencyUpdates = restArgs.contains("depUp")
    val showHelp = restArgs.contains("help") || restArgs.contains("--help")
    val showUpdateCmd = restArgs.contains("--show-update-cmd")
    val showGit = restArgs.contains("showGit")
    val createFeatureBranch = restArgs.contains("nothing-but-create-feature-branch")
    val noVerify = !restArgs.contains("noVerify")
    val jenkinsTrigger = restArgs.contains("jenkinsTrigger")
    val versionSetFilter: (List[String] ⇒ Boolean) = in ⇒ in.head == "versionSet"
    val versionSetMode = restArgs.sliding(2).exists(versionSetFilter)
    val shopGASetFilter: (List[String] ⇒ Boolean) = in ⇒ in.head == "shopGASet"
    val shopGASetMode = restArgs.sliding(2).exists(shopGASetFilter)

    // TODO --batch ## alles mit default wählen

    if (showHelp) {
      out.println("Usage: release [OPTION]...")
      out.println("Note: Calling release without any options creates a normal release.")
      out.println("All options are non-mandatory.")
      out.println()
      out.println("Possible args:")
      out.println("help/--help      => shows this and exits")
      out.println("depUp            => shows dependency updates from nexus option")
      out.println("simpleChars      => use no drawing chars")
      out.println("showGit          => shows all git commands for debug")
      out.println("replace          => replaces release jar / only required for development")
      out.println("noVerify         => use this toggle for non gerrit projects")
      out.println("jenkinsTrigger   => beta: jenkins trigger for builds")
      out.println()
      out.println("versionSet newVersion                => changes version like maven")
      out.println("shopGASet newGroupIdAndAtifactId     => changes GroupId and ArtifactId for Shops")
      out.println("nothing-but-create-feature-branch    => creates a feature branch and changes pom.xmls")
      out.println()
      out.println("Possible environment variables:")
      out.println("export RELEASE_GIT_BIN = $PATH_TO_GIT_BIN")
      out.println()
      out.println("Your home dir is: " + System.getProperty("user.home"))
      return 0
    }
    val gitBinEnv = Properties.envOrNone("RELEASE_GIT_BIN")
    val releaseToolGit = Sgit(file = releaseToolDir, gitBin = gitBinEnv, showGitCmd = showGit, doVerify = false, out = out, err = err)

    val updateCmd: String = {
      val updatePath = if (termOs.isCygwin && !termOs.isMinGw) {
        "$(cygpath -u \"" + releaseToolPath + "\")"
      } else {
        releaseToolPath
      }
      "(cd " + updatePath + " && git rebase -q --autostash && cd -)"
    }
    if (showUpdateCmd) {
      out.println(updateCmd)
      return 0
    } else if (!restArgs.contains("no-update") && releaseToolGit.tryFetchAll().isSuccess) {
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

    def fetchGitAndAskForBranch(): (Sgit, String) = {

      def fetchGit(file: File): Sgit = {
        val git = Sgit(file = file, showGitCmd = showGit, doVerify = noVerify, out = out, err = err, gitBin = gitBinEnv)
        git.fetchAll()
        git
      }

      implicit val ec = ExecutionContext.global
      val gitF: Future[Sgit] = Future {
        debug("fetching git")
        val out = fetchGit(workDirFile)
        debug("fetched git")
        out
      }

      def askReleaseBranch(): String = {
        def workGit(file: File):Sgit = {
          Sgit(file = file, showGitCmd = showGit, doVerify = noVerify, out = out, err = err, gitBin = gitBinEnv)
        }

        def suggestCurrentBranch(file: File): String = {
          val git = workGit(file)
          val latestCommit = git.commitIdHead()
          val selectedBraches = git.branchListLocal().filter(_.commitId == latestCommit)
          val found = selectedBraches.map(_.branchName.replaceFirst("refs/heads/", "")).filterNot(_ == "HEAD")
          if (found.size != 1) {
            out.println("W: more than one branch found: " + found.mkString(", ") + "; using \"master\"")
            "master"
          } else {
            found.head
          }

        }

        val branch = Term.readFrom(out, "Enter branch name where to start from", suggestCurrentBranch(workDirFile))
        val git = workGit(workDirFile)
        val allBranches = git.branchNamesAllFull()
        if (!allBranches.contains(branch)) {
          out.println("W: invalid branchname: " + branch + "; try one of: " + allBranches.mkString(", "))
          askReleaseBranch()
        } else {
          branch
        }
      }

      val startBranchF: Future[String] = Future {
        debug("ask for release")
        askReleaseBranch()
      }
      val result = for {
        git <- gitF
        startBranch <- startBranchF
      } yield (git, startBranch)

      try {
        Await.result(result, Duration.create(60, TimeUnit.SECONDS))
      } catch {
        case _: TimeoutException ⇒ throw new TimeoutException("git fetch failed")
      }
    }

    def suggestRebase(sgit: Sgit, branch: String): () ⇒ Unit = {
      debug("ask for rebase")
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

    def handleException(t: Throwable): Int = {
      t match {
        case x@(_: Sgit.MissigCommitHookException | _: Sgit.MissigGitDirException |
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
      val gitAndBranchname = fetchGitAndAskForBranch()
      val git = gitAndBranchname._1
      val startBranch = gitAndBranchname._2
      val askForRebase = suggestRebase(git, startBranch)
      debug("readFromPrompt")
      debugFn(() ⇒ "local branches: " + git.branchNamesLocal())
      if (versionSetMode) {
        Release.checkLocalChanges(git, startBranch)
        val mod = PomMod(workDirFile)
        val version = restArgs.sliding(2).find(versionSetFilter).get.last
        val versionWithoutSnapshot = Term.removeSnapshot(version)
        mod.changeVersion(versionWithoutSnapshot + "-SNAPSHOT")
        mod.writeTo(workDirFile)
        println("You have local changes")
      } else if (shopGASetMode) {
        Release.checkLocalChanges(git, startBranch)
        val mod = PomMod(workDirFile)
        val groupIdArtifactIdLine = restArgs.sliding(2).find(shopGASetFilter).get.last
        mod.changeShopGroupArtifact(groupIdArtifactIdLine)
        mod.writeTo(workDirFile)
        println("You have local changes")
      } else if (jenkinsTrigger) {
        if (config.isInNovomindNetwork) {
          out.println(tagBuildUrl(git, config.jenkinsBaseUrl()))
          out.println("WIP try to notify jenkins to create new jenkins jobs")
          out.println("WIP try to notify created release job")
          return 0
        } else {
          out.println("only available in novomind network")
          return 1
        }

      } else if (createFeatureBranch) {
        FeatureBranch.work(workDirFile, out, err, git, startBranch, askForRebase, releaseToolGit.headStatusValue(), config)
      } else {
        Release.work(workDirFile, out, err, askForRebase, startBranch,
          git, dependencyUpdates, termOs, shellWidth, releaseToolGit.headStatusValue(), config)
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
    val list: Seq[GitRemote] = git.remoteList
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
      val remoteBranchNames = sgit.branchNamesRemote()
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
