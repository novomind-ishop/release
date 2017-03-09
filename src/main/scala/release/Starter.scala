package release

import java.io.{File, IOException}
import java.util.jar.Manifest

import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jgit.api.errors.RefAlreadyExistsException
import release.PomMod.Dep

import scala.io.StdIn

object Starter extends App with LazyLogging {

  val argSeq: Seq[String] = args.toSeq
  val releaseToolPath = argSeq.head
  val releaseToolDir = new File(releaseToolPath).getAbsoluteFile
  val workDir = argSeq(1)
  val workDirFile = new File(workDir).getAbsoluteFile
  val termOs: TermOs = TermOs.select(argSeq(3), argSeq(2))
  val shellWidth = argSeq(4).toInt
  val restArgs = argSeq.drop(5).filter(_ != null).map(_.trim)
  val dependencyUpdates = restArgs.contains("depUp")
  val showHelp = restArgs.contains("help")
  val showGit = restArgs.contains("showGit")
  val noVerify = !restArgs.contains("noVerify")

  val releaseToolGit = Sgit(releaseToolDir, showGitCmd = showGit, doVerify = false)

  private def readFromOneOf(text: String, possibleValues: Seq[String]): String = {
    print(text + " [%s]: ".format(possibleValues.mkString("/")))
    val line = StdIn.readLine
    line match {
      case null ⇒ System.exit(1); null
      case any: String if possibleValues.contains(any.trim) ⇒ any.trim
      case _: String ⇒ readFromOneOf(text, possibleValues)
    }
  }

  private def readChooseOneOfOrType(text: String, possibleValues: Seq[String]): String = {
    possibleValues match {
      case vals if vals.size == 1 ⇒ readFrom(text, possibleValues.head)
      case vals ⇒ {
        println(text)
        val mapped: Map[String, String] = possibleValues.zip(Stream.from(1))
          .map(in ⇒ (in._2.toString, in._1)).foldLeft(Map.empty[String, String])(_ + _)
        possibleValues.zip(Stream.from(1))
          .map(in ⇒ (in._2, in._1)).foreach(p ⇒ println("[" + p._1 + "] " + p._2))
        print("Enter option or type [" + possibleValues.head + "]: ")
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

  private def readFrom(text: String, defaultValue: String): String = {
    print(text + " [%s]: ".format(defaultValue))
    val line = StdIn.readLine()
    line match {
      case null ⇒ System.exit(1); null
      case "" ⇒ defaultValue
      case any: String ⇒ Option(any.trim).getOrElse(defaultValue)
    }
  }

  private def suggestCurrentBranch(file: File) = {
    try {
      val git = Sgit(file, showGitCmd = showGit, doVerify = noVerify)
      val latestCommit = git.commitIdHead()
      val selectedBraches = git.branchListLocal().filter(_.getObjectId.getName == latestCommit)
      val found = selectedBraches.map(_.getName.replaceFirst("refs/heads/", ""))
      if (found.size != 1) {
        throw new IllegalStateException("more then one branch found " + found)
      } else {
        found.head
      }
    } catch {
      case e: IllegalStateException ⇒ {
        println("W: " + e.getMessage)
        logger.debug("", e)
        "master"
      }
    }
  }

  private def fetchGit(file: File): Sgit = {
    val git = Sgit(file, showGitCmd = showGit, doVerify = noVerify)
    git.fetchAll()
    git
  }

  def checkForSnapshotsIn(mod: PomMod): Seq[Dep] = {
    val deps = mod.listDependecies
    deps
  }

  def offerAutoFixForReleaseSnapshots(mod: PomMod): PomMod = {
    case class ReleaseInfo(gav: String, released: Boolean)
    val snaps = mod.listSnapshotsDistinct
    val aetherStateLine = StatusLine(snaps.size, shellWidth)
    val snapState: Seq[ReleaseInfo] = snaps
      .par
      .map(in ⇒ {
        aetherStateLine.start()
        val released = Aether.existsGav(in.groupId, in.artifactId, in.version.replace("-SNAPSHOT", ""))
        aetherStateLine.end()
        ReleaseInfo(in.gav, released)
      }).seq
    aetherStateLine.finish()

    if (snapState.nonEmpty) {
      println("")
      // TODO later autofix
      println("Snapshots found for (fix manually):")

      def info(rel: Boolean): String = if (rel) {
        "Release found for "
      } else {
        "No Release for    "
      }

      snapState
        .sortBy(_.toString)
        .map(in ⇒ info(in.released) + in.gav)
        .foreach(println)
      println("")

      val again = readFromOneOf("Try again?", Seq("y", "n"))
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
    val headlg = releaseToolGit.commitIds("HEAD", "origin/master")
    if (headVersion != remoteMasterVersion) {
      println("Production Version: " + remoteMasterVersion)
      println("Your Version:       " + headVersion)
      println("Please update your ishop tools: (cd " + releaseToolPath + " && git rebase && cd -)")
      println("_diff: " + headlg.map(_.substring(0, 7)))
      System.exit(1)
    }

  }

  def suggestRebase(sgit: Sgit): () ⇒ Unit = {
    if (false) {
      () ⇒ {
        val update = readFromOneOf("Remote changes detected. Rebase local branch?", Seq("y", "n"))

      }
    } else {
      () ⇒ {
        println("Git updated - check for remote updates manually")
      }
    }
  }

  def askReleaseBranch(): String = {
    readFrom("Enter branch name where the release should start from", suggestCurrentBranch(workDirFile))
  }

  def readFromPrompt(rebaseFn: () ⇒ Unit, branch: String, sgit: Sgit): Seq[Unit] = {
    if (sgit.hasLocalChanges) {
      throw new IllegalStateException(branch + " has local changes, please commit or reset")
    }
    rebaseFn.apply()
    registerExitFn("cleanup branches", () ⇒ {
      sgit.checkout(sgit.currentBranch)
    })
    sgit.checkout(branch)
    val upstream = sgit.findUpstreamBranch()
    if (upstream.isEmpty) {
      val newUpstream = readChooseOneOfOrType("No upstream found, please set", Seq("origin/master", "origin/" + branch))
      sgit.setUpstream(newUpstream)
    }
    val mod = PomMod(workDirFile)

    if (dependencyUpdates) {
      val showUpdates = readFromOneOf("Show dependency updates?", Seq("y", "n"))
      if (showUpdates == "y") {
        mod.showDependecyUpdates(shellWidth, termOs)
      }
    }
    checkForSnapshotsIn(mod)
    val newMod = offerAutoFixForReleaseSnapshots(mod)
    if (newMod.hasNoShopPom) {
      println("---------")
      println("1. MAJOR version when you make incompatible API changes,")
      println("2. MINOR version when you add functionality in a backwards-compatible manner, and")
      println("3. PATCH version when you make backwards-compatible bug fixes.")
      println("   see also: http://semver.org/")
      println("---------")
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
      println("skipped release commit on " + branch)
    } else {
      sgit.doCommitPomXmls(
        """[ishop-release] prepare for next iteration - %s
          |
          |Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>
          |Releasetool-sha1: %s""".stripMargin.format(nextReleaseWithoutSnapshot, toolSh1))

    }
    print("checkout " + releaseBrachName + " ..")
    sgit.checkout(releaseBrachName)
    println(". done")
    val releaseMod = PomMod(workDirFile)
    if (releaseMod.selfVersion != release) {
      releaseMod.changeVersion(release)
      releaseMod.writeTo(workDirFile)
    }

    if (sgit.hasNoLocalChanges) {
      println("skipped release commit on " + releaseBrachName)
    } else {
      sgit.doCommitPomXmls(
        """[ishop-release] perform to - %s
          |
          |Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>
          |Releasetool-sha1: %s""".stripMargin.format(release, toolSh1))

    }
    if (releaseMod.hasNoShopPom) {
      sgit.doTag(release)
    }
    print("checkout " + branch + " ..")
    sgit.checkout(branch)
    println(". done")
    if (newMod.hasNoShopPom) {
      sgit.deleteBranch(releaseBrachName)
    }
    println(sgit.graph())
    val sendToGerrit = readFromOneOf("Send to Gerrit?", Seq("y", "n"))
    val selectedBranch = sgit.findUpstreamBranch().getOrElse(branch)
    if (sendToGerrit == "y") {

      if (sgit.hasChangesToPush) {
        val result = sgit.pushFor(srcBranchName = branch, targetBranchName = selectedBranch, pushTags = newMod.hasNoShopPom)
      }
      if (newMod.hasShopPom) {
        val result = sgit.push(srcBranchName = "release/" + releaseWitoutSnapshot,
          targetBranchName = "release/" + releaseWitoutSnapshot, pushTags = newMod.hasNoShopPom)
      }
      println("done.")
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
      println(
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

  private def registerExitFn(msg: String, fn: () ⇒ Unit): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread(() ⇒ {
      fn.apply()
    }))
  }

  private def handleException(t: Throwable) = {
    t match {
      case x: Sgit.MissigCommitHookException ⇒ {
        println()
        println(x.getMessage)
        System.exit(1)
      }
      case x: RefAlreadyExistsException ⇒ {
        println("E: " + x.getMessage)
        System.exit(1)
      }
      case _ ⇒ {
        println(t)
        t.printStackTrace()
        System.exit(2)
      }
    }

  }

  if (showHelp) {
    println("Possible args:")
    println("help     => shows this and exits")
    println("depUp    => shows dependency updates from nexus option")
    println("showGit  => shows all git commands for debug")
    println("replace  => replaces release jar / only required for development")
    println("noVerify => use this toggle for non gerrit projects")
    System.exit(0)
  }
  try {
    val git: Sgit = fetchGit(workDirFile)
    val startBranch: String = askReleaseBranch()
    val askForRebase = suggestRebase(git)
    readFromPrompt(askForRebase, startBranch, git)
  } catch {
    case t: Throwable ⇒ {
      handleException(t)
    }
  }

  object TermOs {

    def select(term: String, os: String) = term match {
      case "xterm" ⇒ TermOs("xterm", os)
      case "xterm-256color" ⇒ TermOs("xterm-256color", os)
      case "screen-256color" ⇒ TermOs("screen-256color", os)
      case "cygwin" ⇒ TermOs("cygwin", os)
      case t ⇒ throw new IllegalStateException(t)
    }
  }

  case class TermOs(term: String, os: String) {
    val isCygwin: Boolean = term.equals("cygwin")
    val isMinGw: Boolean = os.contains("MINGW")
  }

}
