package release

import java.io.{File, IOException, PrintStream}
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.concurrent.TimeoutException
import java.util.jar.Manifest

import com.google.common.hash.Hashing
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jgit.api.errors.RefAlreadyExistsException
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.StdIn

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
    val restArgs = argSeq.drop(5).filter(_ != null).map(_.trim)
    val debugKey = restArgs.contains("debug")

    def debug(message: String): Unit = {
      if (debugKey) {
        out.println("DEBUG: " + message)
      }
    }

    debug("init")
    val termOs: TermOs = TermOs.select(argSeq(3), argSeq(2), restArgs.contains("simpleChars"))
    val dependencyUpdates = restArgs.contains("depUp")
    val showHelp = restArgs.contains("help") || restArgs.contains("--help")
    val showGit = restArgs.contains("showGit")
    val noVerify = !restArgs.contains("noVerify")
    // TODO --batch ## alles mit default wählen

    val releaseToolGit = Sgit(releaseToolDir, showGitCmd = showGit, doVerify = false, out, err)

    def readFromOneOfYesNo(text: String) = readFromOneOf(text, Seq("y", "n"))

    def readFromOneOf(text: String, possibleValues: Seq[String]): String = {
      out.print(text + " [%s]: ".format(possibleValues.mkString("/")))
      val line = StdIn.readLine
      line match {
        case null ⇒ System.exit(1); null
        case any: String if possibleValues.contains(any.trim) ⇒ any.trim
        case _: String ⇒ readFromOneOf(text, possibleValues)
      }
    }

    def readChooseOneOfOrType(text: String, possibleValues: Seq[String]): String = {
      possibleValues match {
        case vals if vals.size == 1 ⇒ readFrom(text, possibleValues.head)
        case vals ⇒ {
          out.println(text)
          val mapped: Map[String, String] = possibleValues.zip(Stream.from(1))
            .map(in ⇒ (in._2.toString, in._1)).foldLeft(Map.empty[String, String])(_ + _)
          possibleValues.zip(Stream.from(1))
            .map(in ⇒ (in._2, in._1)).foreach(p ⇒ out.println("[" + p._1 + "] " + p._2))
          out.print("Enter option or type [" + possibleValues.head + "]: ")
          val line = StdIn.readLine
          line match {
            case null ⇒ System.exit(1); null
            case "" ⇒ mapped.head._2
            case any: String if mapped.contains(any) ⇒ mapped(any)
            case other: String ⇒ other.trim
          }
        }
      }
    }

    def readFromYes(text: String) = readFrom(text, "y")

    def readFrom(text: String, defaultValue: String): String = {
      out.print(text + " [%s]: ".format(defaultValue))
      val line = StdIn.readLine()
      line match {
        case null ⇒ System.exit(1); null
        case "" ⇒ defaultValue
        case any: String ⇒ Option(any.trim).getOrElse(defaultValue)
      }
    }

    def suggestCurrentBranch(file: File): String = {
      val git = Sgit(file, showGitCmd = showGit, doVerify = noVerify, out, err)
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

    def fetchGit(file: File): Sgit = {
      val git = Sgit(file, showGitCmd = showGit, doVerify = noVerify, out, err)
      git.fetchAll()
      git
    }

    def offerAutoFixForReleaseSnapshots(mod: PomMod): PomMod = {
      val plugins = mod.listPluginDependecies
      if (mod.hasShopPom) {
        // TODO check if core needs this checks too
        PomChecker.check(plugins)
      }

      case class ReleaseInfo(gav: String, released: Boolean)
      val snaps = mod.listSnapshotsDistinct.map(_.gav()) ++
        plugins.map(_.gav()).filter(_.version.contains("SNAPSHOT"))
      val aetherStateLine = StatusLine(snaps.size, shellWidth)
      val snapState: Seq[ReleaseInfo] = snaps
        .par
        .map(in ⇒ {
          aetherStateLine.start()
          val released = Aether.existsGav(in.groupId, in.artifactId, in.version.replace("-SNAPSHOT", ""))
          aetherStateLine.end()
          ReleaseInfo(in.formatted, released)
        }).seq
      aetherStateLine.finish()

      if (snapState.nonEmpty) {
        out.println("")
        // TODO later autofix
        out.println("Snapshots found for (fix manually):")

        def info(rel: Boolean): String = if (rel) {
          "Release found for "
        } else {
          "No Release for    "
        }

        snapState
          .sortBy(_.toString)
          .map(in ⇒ info(in.released) + in.gav)
          .foreach(println)
        out.println("")

        val again = readFromOneOfYesNo("Try again?")
        if (again == "n") {
          System.exit(1)
        } else {
          offerAutoFixForReleaseSnapshots(PomMod(mod.file))
        }
      }
      mod
    }

    def manifestVersion(): String = {
      val key = "Git-Head-Rev"
      try {
        val resource = getClass.getClassLoader.getResource("META-INF/MANIFEST.MF")
        val manifest = new Manifest(resource.openStream())
        manifest.getMainAttributes.getValue(key)
      } catch {
        case e: IOException ⇒ e.printStackTrace(); "UNDEF-KEY-" + key
      }
    }

    if (!restArgs.contains("noUpdate")) {
      releaseToolGit.fetchAll()
      val headVersion = releaseToolGit.commitIdHead()
      val remoteMasterVersion = releaseToolGit.commitId("origin/master")
      if (headVersion != remoteMasterVersion) {
        out.println("Production Version: " + remoteMasterVersion)
        out.println("Your Version:       " + headVersion)
        val updatePath = if (termOs.isCygwin && !termOs.isMinGw) {
          "$(cygpath -u \"" + releaseToolPath + "\")"
        } else {
          releaseToolPath
        }
        out.println("Please update your release tool: (cd " + updatePath + " && git rebase && cd -)")
        return 1
      }

    }

    def suggestRebase(sgit: Sgit, branch: String): () ⇒ Unit = {
      debug("ask for rebase")
      sgit.checkout(branch)
      chooseUpstreamIfUndef(sgit, branch)
      val shouldRebase = sgit.commitIds("@{upstream}", branch)
      if (shouldRebase != Nil) {
        () ⇒ {
          val update = readFromOneOfYesNo("Your branch is " + shouldRebase.size +
            " commits behind or ahead defined upstream. Rebase local branch?")
          if (update == "y") {
            sgit.rebase()
          }

        }
      } else {
        () ⇒ {}
      }
    }

    def askReleaseBranch(): String = {
      readFrom("Enter branch name where the release should start from", suggestCurrentBranch(workDirFile))
    }

    def chooseUpstreamIfUndef(sgit: Sgit, branch: String): Unit = {
      val upstream = sgit.findUpstreamBranch()
      if (upstream.isEmpty) {
        val newUpstream = readChooseOneOfOrType("No upstream found, please set", Seq("origin/master", "origin/" + branch).distinct)
        sgit.setUpstream(newUpstream)
      }
    }

    def sign(): String = {
      Hashing.sha1()
        .hashString(LocalDateTime.now().toString, StandardCharsets.UTF_8)
        .toString
    }

    def readFromPrompt(rebaseFn: () ⇒ Unit, branch: String, sgit: Sgit): Seq[Unit] = {
      if (sgit.hasLocalChanges) {
        val changes = sgit.localChanges().take(5)
        val changesOut = changes match {
          case c if c.size <= 5 ⇒ c.mkString("\n")
          case c ⇒ c.mkString("\n") + "\n..."
        }
        throw new PreconditionsException("Your branch: \"" + branch + "\" has local changes, please commit or reset\n" + changesOut)
      }
      rebaseFn.apply()
      registerExitFn("cleanup branches", () ⇒ {
        sgit.checkout(sgit.currentBranch)
      })
      sgit.checkout(branch)
      chooseUpstreamIfUndef(sgit, branch)
      val mod = PomMod(workDirFile)

      if (dependencyUpdates) {
        val showUpdates = readFromYes("Show dependency updates?")
        if (showUpdates == "y") {
          mod.showDependecyUpdates(shellWidth, termOs, out)
        }
      }

      val newMod = offerAutoFixForReleaseSnapshots(mod)
      if (newMod.hasNoShopPom) {
        out.println("---------")
        out.println("1. MAJOR version when you make incompatible API changes,")
        out.println("2. MINOR version when you add functionality in a backwards-compatible manner, and")
        out.println("3. PATCH version when you make backwards-compatible bug fixes.")
        out.println("   see also: http://semver.org/")
        out.println("---------")
      }

      val release = readChooseOneOfOrType("Enter the release version", newMod.suggestReleaseVersion())
      val releaseWitoutSnapshot = release.replaceFirst("-SNAPSHOT$", "")
      val nextReleaseWithoutSnapshot = readFrom("Enter the next version without -SNAPSHOT", newMod.suggestNextRelease(release))

      val nextSnapshot = nextReleaseWithoutSnapshot + "-SNAPSHOT"
      if (newMod.selfVersion != nextSnapshot) {
        newMod.changeVersion(nextSnapshot)
      }

      val releaseBrachName = "release/" + releaseWitoutSnapshot
      sgit.createBranch(releaseBrachName)
      if (newMod.selfVersion != nextSnapshot) {
        newMod.writeTo(workDirFile)
      }
      val toolSh1 = releaseToolGit.headStatusValue()
      val headCommitId = sgit.commitIdHead()
      if (sgit.hasNoLocalChanges) {
        out.println("skipped release commit on " + branch)
      } else {
        out.print("Commit pom changes ..")
        sgit.doCommitPomXmls(
          """[ishop-release] prepare for next iteration - %s
            |
            |Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>
            |Releasetool-sign: %s
            |Releasetool-sha1: %s""".stripMargin.format(nextReleaseWithoutSnapshot, sign(), toolSh1))

        out.println(". done")
      }
      out.print("checkout " + releaseBrachName + " ..")
      sgit.checkout(releaseBrachName)
      out.println(". done")
      val releaseMod = PomMod(workDirFile)
      if (releaseMod.selfVersion != release) {
        releaseMod.changeVersion(release)
        releaseMod.writeTo(workDirFile)
      }

      if (sgit.hasNoLocalChanges) {
        out.println("skipped release commit on " + releaseBrachName)
      } else {
        out.print("Commit pom changes ..")
        sgit.doCommitPomXmls(
          """[ishop-release] perform to - %s
            |
            |Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>
            |Releasetool-sign: %s
            |Releasetool-sha1: %s""".stripMargin.format(release, sign(), toolSh1))
        out.println(". done")
      }
      if (releaseMod.hasNoShopPom) {
        sgit.doTag(release)
      }
      out.print("checkout " + branch + " ..")
      sgit.checkout(branch)
      out.println(". done")
      if (newMod.hasNoShopPom) {
        sgit.deleteBranch(releaseBrachName)
      }
      out.println(sgit.graph())
      val sendToGerrit = readFromOneOfYesNo("Send to Gerrit?")
      val selectedBranch = sgit.findUpstreamBranch().getOrElse(branch)
      if (sendToGerrit == "y") {

        if (sgit.hasChangesToPush) {
          val result = sgit.pushFor(srcBranchName = branch, targetBranchName = selectedBranch, pushTags = newMod.hasNoShopPom)
          // try to notify jenkins about tag builds
        }
        if (newMod.hasShopPom) {
          val result = sgit.push(srcBranchName = "release/" + releaseWitoutSnapshot,
            targetBranchName = "release/" + releaseWitoutSnapshot, pushTags = newMod.hasNoShopPom)
          // TODO try to trigger job updates for jenkins
          // TODO try to trigger job execution in loop with abort
        }
        out.println("done.")
      } else {
        val pushTagOrBranch = if (newMod.hasNoShopPom) {
          "git push origin tag v" + releaseWitoutSnapshot + "; "
        } else {
          "git push -u origin release/" + releaseWitoutSnapshot + ":release/" + releaseWitoutSnapshot + ";"
        }
        val deleteTagOrBranch = if (newMod.hasNoShopPom) {
          "git tag -d v" + release + "; "
        } else {
          "git branch -D release/" + releaseWitoutSnapshot + ";"
        }
        out.println(
          ("""commands for local rollback:
             |  git reset --hard """ + headCommitId +
            """; """ + deleteTagOrBranch +
            """
              |
              |commands for sending to remote:
              |  git push origin """ + branch +
            """:refs/for/""" + selectedBranch +
            """; """ + pushTagOrBranch +
            """
              |NOTE: the "send to remote" command pushes the HEAD via Gerrit Code Review, this might not be needed for branches != master""").stripMargin)
      }
      Nil
    }

    def registerExitFn(msg: String, fn: () ⇒ Unit): Unit = {
      Runtime.getRuntime.addShutdownHook(new Thread(() ⇒ {
        fn.apply()
      }))
    }

    class PreconditionsException(msg: String) extends RuntimeException(msg)

    def handleException(t: Throwable): Int = {
      t match {
        case x@(_: RefAlreadyExistsException | _: Sgit.MissigCommitHookException |
                _: PomChecker.ValidationException | _: PreconditionsException) ⇒ {
          err.println()
          err.println("E: " + x.getMessage)
          1
        }

        case x@(_: PomMod.InvalidPomXmlException) ⇒ {
          err.println()
          err.println("E: " + x.getMessage)
          err.println("E: " + x.parent.getMessage)
          1
        }
        case _ ⇒ {
          err.println(t)
          t.printStackTrace(err)
          2
        }
      }

    }

    def fetchGitAndAskForBranch(): (Sgit, String) = {
      implicit val ec = ExecutionContext.global
      val gitF: Future[Sgit] = Future {
        debug("fetching git")
        val out = fetchGit(workDirFile)
        debug("fetched git")
        out
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
        Await.result(result, Span(60, Seconds))
      } catch {
        case _: TimeoutException ⇒ throw new TimeoutException("git fetch failed")
      }
    }

    if (showHelp) {
      out.println("Possible args:")
      out.println("help/--help => shows this and exits")
      out.println("depUp       => shows dependency updates from nexus option")
      out.println("simpleChars => use no drawing chars")
      out.println("showGit     => shows all git commands for debug")
      out.println("replace     => replaces release jar / only required for development")
      out.println("noVerify    => use this toggle for non gerrit projects")
      return 0
    }

    try {
      val gitAndBranchname = fetchGitAndAskForBranch()
      val git = gitAndBranchname._1
      val startBranch = gitAndBranchname._2
      val askForRebase = suggestRebase(git, startBranch)
      debug("change")
      readFromPrompt(askForRebase, startBranch, git)
      return 0
    } catch {
      case t: Throwable ⇒ {
        return handleException(t)
      }
    }

  }

  System.exit(init(args, System.out, System.err))

}
