package release

import java.io.File
import java.net.URI

import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.CannotDeleteCurrentBranchException
import org.eclipse.jgit.diff.DiffEntry
import org.eclipse.jgit.lib.Ref

import scala.collection.JavaConverters
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class Sgit(file: File, showGitCmd: Boolean, doVerify: Boolean) extends LazyLogging {

  private def gitRoot = Sgit.findGit(file.getAbsoluteFile)

  private def dotGit = new File(gitRoot, ".git")

  private def init(): Unit = {
    gitNative(Seq("init", file.getAbsolutePath), useWorkdir = false)
  }

  private def clone(src: File, dest: File): Unit = {
    gitNative(Seq("clone", "-q", src.getAbsolutePath, dest.getAbsolutePath), useWorkdir = false)
  }

  private[release] def config(key: String, value: String): Unit = {
    gitNative(Seq("config", key, value))
  }

  private[release] def addByString(f: String): Unit = {
    config("core.safecrlf", "false")
    gitNative(Seq("add", f))
    config("core.safecrlf", "warn")
  }

  private[release] def add(f: File): Unit = {
    addByString(f.getAbsolutePath)
  }

  private[release] def commit(message: String): Unit = {
    val lines = message.replaceAll("\r", "").lines.toList
    val ms = if (lines.size == 1) {
      lines ++ Seq("")
    } else {
      lines
    } ++ Seq("Change-Id:")
    if (ms(1).nonEmpty) {
      throw new IllegalStateException("non empty line")
    }
    config("core.safecrlf", "false")
    val oldComitId = commitIdHeadOpt()
    gitNative(Seq("commit", "--no-verify", "-m", ms.mkString("\n").trim))
    if (oldComitId.isDefined) {
      val newCommitId = commitIdHead()
      gitNative(Seq("reset", oldComitId.get))
      gitNative(Seq("add", "-A"))
      gitNative(Seq("commit", "--no-verify", "-m", "\"" + ms.map(_.replaceFirst("^Change-Id:", "Change-Id: I" + newCommitId)).mkString("\n").trim + "\""))
    }
    config("core.safecrlf", "warn")
  }

  def verify(): Unit = {

    def checkCommitMessageHook(_dotGit: File): Unit = {

      val commitMsgHook = if (_dotGit.isDirectory) {
        new File(new File(_dotGit, "hooks"), "commit-msg")
      } else if (_dotGit.isFile) {
        val workTreeDef: String = Source.fromFile(_dotGit).mkString
          .replaceFirst("gitdir: ", "")
          .replaceFirst("\\.git.*$", ".git")
          .trim
        val realGitDir = new File(workTreeDef)
        new File(new File(realGitDir, "hooks"), "commit-msg")
      } else {
        throw new IllegalStateException("invalid git folder")
      }

      if (!(commitMsgHook.canRead && commitMsgHook.canExecute)) {
        throw new Sgit.MissigCommitHookException(
          """
            |E: please download a commit-message hook and retry
            |E: The hook should be at: %s
            |I: see https://git-ishop.novomind.com:9091/Documentation/user-changeid.html#creation
            |# scp -p -P 19418 $USERNAME@git-ishop.novomind.com:hooks/commit-msg .git/hooks/
            |
        """.stripMargin.format(commitMsgHook.getAbsolutePath))
      }
    }

    if (dotGit.exists()) {
      checkCommitMessageHook(dotGit)
    }

  }

  gitNative(Seq("--version"), useWorkdir = false) match {
    case v: String if v.startsWith("git version 2.8") ⇒ // do nothing
    case v: String if v.startsWith("git version 2.9") ⇒ // do nothing
    case v: String if v.startsWith("git version 2.10") ⇒ // do nothing
    case v: String if v.startsWith("git version 2.11") ⇒ // do nothing
    case v: String if v.startsWith("git version 2.12") ⇒ // do nothing
    case v: String ⇒ println("W: unknown git version: \"" + v + "\"")
  }

  @Deprecated
  private lazy val git = Git.open(gitRoot)

  if (doVerify) {
    verify()
  }

  def currentBranch: String = gitNative(Seq("rev-parse", "--abbrev-ref", "HEAD"))

  def fetchAll(): Unit = {
    gitNative(Seq("fetch", "-q", "--all", "--tags"))
  }

  def diff(): Seq[String] = {
    val call: java.util.List[DiffEntry] = git.diff().call()
    JavaConverters.asScalaBufferConverter(call).asScala.map(_.toString)
  }

  def branchListLocal(): Seq[Ref] = {
    JavaConverters.iterableAsScalaIterable(git.branchList().call()).toSeq
  }

  def doCommitPomXmls(message: String): Unit = {
    val modPom = status()
    val modDistinct = modPom.map(in ⇒ in.replaceFirst(".*/", "")).distinct
    if (modDistinct != Seq("pom.xml")) {
      throw new IllegalStateException("only pom changes are allowed = " + modPom.mkString(", "))
    }

    modPom.foreach(addByString)

    val statusAfterAdd = status()
    if (statusAfterAdd.nonEmpty) {
      throw new IllegalStateException("uncommited changes: " + statusAfterAdd.mkString(", "))
    }

    commit(message)
  }

  def setUpstream(upstreamName: String): Unit = {
    gitNative(Seq("branch", "--set-upstream-to=" + upstreamName))
  }

  def findUpstreamBranch(): Option[String] = {
    val upstreamOpt = gitNativeOpt(Seq("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"))
    selectUpstream(upstreamOpt)
  }

  private[release] def selectUpstream(upstreamOpt: Option[String]): Option[String] = {
    upstreamOpt
      .filter(_.startsWith("origin/"))
      .map(_.replaceFirst("^origin/", ""))
  }

  def commitIdHeadOpt(): Option[String] = {
    commitIdOpt("HEAD")
  }

  def commitIdHead(): String = {
    commitId("HEAD")
  }

  def commitIdOpt(ref: String): Option[String] = {
    gitNativeOpt(Seq("log", "-n1", "--pretty=%H", ref))
  }

  def commitId(ref: String): String = {
    gitNative(Seq("log", "-n1", "--pretty=%H", ref))
  }

  def commitIds(fromRef: String, toRef: String): Seq[String] = {
    val changes = gitNative(Seq("log", fromRef + "..." + toRef, "--pretty=%H"))
    changes.lines.toList
  }

  def hasChangesToPush: Boolean = {
    val branchInfo = gitNative(Seq("status", "--branch", "--porcelain", currentBranch))
    branchInfo.contains("ahead")
  }

  def hasLocalChanges: Boolean = {
    val status = git.status().call()
    status.hasUncommittedChanges
  }

  def hasNoLocalChanges: Boolean = {
    !hasLocalChanges
  }

  def headStatusValue(): String = {
    val modStar = if (hasLocalChanges) {
      "*"
    } else {
      ""
    }
    commitIdHead() + modStar
  }

  def graph(): String = {
    try {
      gitNative(Seq("log", "HEAD", "--no-color", "--all", "--graph", "-3", "--oneline", "--decorate", "--"))
    } catch {
      case e: Exception ⇒ e.printStackTrace(); "no - graph"
    }
  }

  private def status(): Seq[String] = {
    val status = git.status().call()
    val modPom: Seq[String] = JavaConverters.asScalaSet(status.getModified).toSeq
    modPom
  }

  def doTag(tagName: String): Unit = {
    try {
      git.tag().setName("v" + tagName).call()
    } catch {
      // TODO wenn tag schon existiert
      case e: RuntimeException ⇒ Util.err(e.getMessage)
    }
  }

  def deleteBranch(branchName: String): Unit = {
    try {
      git.branchDelete()
        .setForce(true)
        .setBranchNames(branchName)
        .call()
    } catch {
      case e: CannotDeleteCurrentBranchException ⇒ println(e.getClass.getCanonicalName + " " + e.getMessage)
      case t: Throwable ⇒ throw t
    }
  }

  def checkout(newBranchName: String): Unit = {
    val branch = currentBranch
    if (branch != newBranchName) {
      gitNative(Seq("checkout", "-q", newBranchName))
    }
  }

  def createBranch(branchName: String): Unit = {
    git.branchCreate()
      .setName(branchName)
      .call()
  }

  def push(srcBranchName: String, targetBranchName: String, pushTags: Boolean): Seq[String] = {
    pushInner(srcBranchName, targetBranchName, pushTags, "refs/heads/")
  }

  def pushFor(srcBranchName: String, targetBranchName: String, pushTags: Boolean): Seq[String] = {
    pushInner(srcBranchName, targetBranchName, pushTags, "refs/for/")
  }

  private def pushInner(srcBranchName: String, targetBranchName: String, pushTags: Boolean, refPrefix: String): Seq[String] = {
    val refDef = srcBranchName + ':' + refPrefix + targetBranchName
    if (pushTags) {
      Seq(gitNative(Seq("push", "-q", "--tags", "-u", "origin", refDef)))
    } else {
      Seq(gitNative(Seq("push", "-q", "-u", "origin", refDef)))
    }

  }

  private[release] def gitNativeOpt(args: Seq[String]): Option[String] = {
    try {
      Some(gitNative(args, showErrors = false))
    } catch {
      case t: Throwable ⇒ None
    }
  }

  private[release] def gitNative(args: Seq[String], showErrors: Boolean = true, useWorkdir: Boolean = true): String = {
    if (!gitRoot.isDirectory) {
      throw new IllegalStateException("invalid git dir: " + gitRoot.getAbsolutePath)
    }
    val workdir = if (useWorkdir) {
      Seq("-C", gitRoot.getAbsolutePath)
    } else {
      Nil
    }
    val gitCmdCall = Sgit.selectedGitCmd() ++ workdir ++ Seq("--no-pager") ++ args
    if (showGitCmd) {
      println(gitCmdCall)
    }
    Sgit.native(gitCmdCall, syserrErrors = showErrors)
  }
}

object Sgit {

  private[release] def native(cmd: Seq[String], syserrErrors: Boolean): String = {
    import sys.process._
    val out: ProcessLogger = new ProcessLogger {
      override def out(s: ⇒ String): Unit = {}

      override def err(s: ⇒ String): Unit = {
        if (syserrErrors) {
          System.err.println(cmd.mkString("!"))
          System.err.println("err:" + s)
        }
      }

      override def buffer[T](f: ⇒ T): T = f
    }

    val result: String = cmd !! out
    result.trim
  }

  private[release] def selectedGitCmd(): Seq[String] = {
    val gitCygCmd: Try[Seq[String]] = try {
      Success(Seq(native(Seq("cygpath", "-daw", "/usr/bin/git"), syserrErrors = false)))
    } catch {
      case e: Exception ⇒ Failure(e)
    }
    val gitCmd: Seq[String] = if (gitCygCmd.isSuccess) {
      gitCygCmd.get
    } else {
      val winGit = new File(new URI("file:///C:/Programme/Git/bin/git.exe"))
      if (winGit.canExecute) {
        Seq(winGit.getAbsolutePath)
      } else {
        Seq("git")
      }
    }
    gitCmd
  }

  class MissigCommitHookException(msg: String) extends RuntimeException(msg)

  private[release] def findGit(in: File): File = {
    if (new File(in, ".git").exists()) {
      in
    } else {
      findGit(in.getParentFile)
    }
  }

  private[release] def clone(src: File, dest: File, showGitCmd: Boolean = false, verify: Boolean = true): Sgit = {
    val o = Sgit(dest, showGitCmd, doVerify = verify)
    o.clone(src, dest)
    Sgit(dest, showGitCmd, doVerify = false)
  }

  private[release] def init(f: File, showGitCmd: Boolean = false, verify: Boolean = true): Sgit = {
    val sgit = Sgit(f, showGitCmd, doVerify = verify)
    sgit.init()
    sgit
  }
}
