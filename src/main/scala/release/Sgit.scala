package release

import java.io.{File, PrintStream}
import java.net.URI

import com.typesafe.scalalogging.LazyLogging
import release.Sgit.BranchAlreadyExistsException

import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.util.{Failure, Success, Try}

case class Sgit(file: File, showGitCmd: Boolean, doVerify: Boolean, out: PrintStream, err: PrintStream) extends LazyLogging {

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

  private[release] def addAll(f: Seq[String]): Unit = {
    config("core.safecrlf", "false")
    gitNative(Seq("add") ++ f)
    config("core.safecrlf", "warn")
  }

  private[release] def add(f: File): Unit = {
    addByString(f.getAbsolutePath)
  }

  private[release] def commitAll(message: String): Unit = {
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
    // _gen_ChangeIdInput() {
    //   echo "tree `git write-tree`"
    //   if parent=`git rev-parse "HEAD^0" 2>/dev/null`
    //   then
    //   echo "parent $parent"
    //   fi
    //   echo "author `git var GIT_AUTHOR_IDENT`"
    //   echo "committer `git var GIT_COMMITTER_IDENT`"
    //   echo
    //   printf '%s' "$clean_message"
    // }
    // _gen_ChangeId() {
    //   _gen_ChangeIdInput |
    //     git hash-object -t commit --stdin
    // }
    gitNative(Seq("commit", "--no-verify", "-m", ms.mkString("\n").trim))
    if (oldComitId.isDefined) {
      val newCommitId = commitIdHead()
      gitNative(Seq("reset", oldComitId.get))
      gitNative(Seq("add", "-A"))
      gitNative(Seq("commit", "--no-verify", "-m", ms.map(_.replaceFirst("^Change-Id:", "Change-Id: I" + newCommitId)).mkString("\n").trim))
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

  Sgit.checkVersion(this, out, err)

  if (doVerify) {
    verify()
  }

  def currentBranch: String = gitNative(Seq("rev-parse", "--abbrev-ref", "HEAD"))

  def fetchAll(): Unit = {
    gitNative(Seq("fetch", "-q", "--all", "--tags"), errMapper = Sgit.fetchFilter)
  }

  def remoteAdd(name: String, url: String): Unit = {
    gitNative(Seq("remote", "add", name, url))
  }

  def remoteRemove(name: String): Unit = {
    gitNative(Seq("remote", "remove", name))
  }

  case class GitShaBranch(commitId: String, branchName: String) {
    if (commitId.length != 40) {
      throw new IllegalStateException("invalid commit id length: " + commitId.length + " (" + commitId + ")")
    }

    if (!commitId.matches("[0-9a-f]{40}")) {
      throw new IllegalStateException("invalid commit id: " + commitId + "")
    }
  }

  def branchNamesLocal(): Seq[String] = branchListLocal().map(_.branchName.replaceFirst("refs/heads/", "")).sorted

  def branchListLocal(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev"))
      .lines.toSeq
      .map(in ⇒ {
        val parts = in.replaceFirst("^\\*", "").trim.split("[ \t]+")
        GitShaBranch(parts(1), "refs/heads/" + parts(0))
      }).toList
  }

  def branchNamesRemote(): Seq[String] = branchListRemote().map(_.branchName.replaceFirst("refs/remotes/origin/", "")).sorted

  def branchNamesAll():Seq[String] = (branchNamesRemote() ++ branchNamesLocal()).distinct.sorted

  def branchListRemote(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev", "--remote"))
      .lines.toSeq
      .flatMap(in ⇒ in match {
        case l: String if l.trim.startsWith("origin/HEAD") ⇒ None
        case l: String if l.trim.startsWith("origin/") ⇒ {
          val parts = l.replaceFirst("^\\*", "").trim.split("[ \t]+")
          Some(GitShaBranch(parts(1), "refs/remotes/" + parts(0)))
        }
        case _: String ⇒ None
      }).toList
  }

  def localPomChanges(): Seq[String] = {
    localChangesWithFilter(Seq("pom.xml"))
  }

  def localChangesWithFilter(filter: Seq[String]): Seq[String] = {
    val modPom = localChanges()
    val modWithoutState = modPom.map(in ⇒ in.replaceFirst("^[^ ]+ ", ""))

    val modDistinct = modWithoutState.map(in ⇒ filter.foldLeft(in)((i, fi) ⇒ i.replaceFirst(".*/" + fi, fi))).distinct.sorted
    val mod = modDistinct.filterNot(in ⇒ filter.contains(in))
    if (mod.nonEmpty) {
      throw new IllegalStateException("only (" + filter.mkString(", ") + ") changes are allowed => " +
        modPom.mkString(", ") + " => " + modDistinct.filterNot(in ⇒ filter.contains(in)).mkString(", ") + " <= " + modDistinct.mkString(", "))
    }
    modWithoutState
  }

  def doCommitPomXmlsAnd(message: String, otherFilenames: Seq[String]): Unit = {
    doCommitWithFilter(message, Seq("pom.xml") ++ otherFilenames)
  }

  def doCommitPomXmls(message: String): Unit = {
    doCommitPomXmlsAnd(message, Nil)
  }

  def doCommitWithFilter(message: String, filter: Seq[String]): Unit = {
    val distinctFilter = filter.distinct.sorted
    addAll(localChangesWithFilter(distinctFilter))
    val statusAfterAdd = localChanges().filterNot(line ⇒ distinctFilter.exists(key ⇒ line.endsWith(key)))
    if (statusAfterAdd.nonEmpty) {
      throw new IllegalStateException("uncommitted changes: " + statusAfterAdd.mkString(", "))
    }

    commitAll(message)
  }

  def setUpstream(upstreamName: String): Unit = {
    gitNative(Seq("branch", "--set-upstream-to=" + upstreamName))
  }

  def findUpstreamBranch(): Option[String] = {
    val upstreamOpt = gitNativeOpt(Seq("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"))
    selectUpstream(upstreamOpt)
  }

  private[release] def selectUpstream(upstreamOpt: Option[String]): Option[String] = {
    val remotes = upstreamOpt.exists(_.startsWith("remotes/"))
    if (remotes) {
      throw new IllegalStateException("you have local branches that overlapps with remotes - please clean up first")
    }
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

  def localChanges(): Seq[String] = {
    val status = gitNative(Seq("status", "--porcelain"))
    status.lines.toList.map(_.replaceAll("[ ]+", " ").trim)
  }

  def hasLocalChanges: Boolean = {
    localChanges() != Nil
  }

  def rebase(): Unit = {
    gitNative(Seq("rebase", "-q"))
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

  def listTags(): Seq[String] = gitNative(Seq("tag", "--list")).lines.toList

  private def checkedTagName(tagName: String): String = {
    if (tagName.startsWith("v")) {
      throw new IllegalArgumentException("tags must not start with 'v', but was " + tagName)
    }
    "v" + tagName
  }

  def doTag(tagName: String): Unit = {
    val newTag = checkedTagName(tagName)
    val allTags = listTags()
    if (allTags.contains(newTag)) {
      throw new IllegalStateException("tag " + newTag + " already exists")
    }
    gitNative(Seq("tag", newTag))
  }

  def deleteBranch(branchName: String): Unit = {
    if (branchNamesLocal().contains(branchName)) {
      gitNative(Seq("branch", "-D", branchName))
    } else {
      throw new IllegalStateException("branch '" + branchName + "' not found.")
    }
  }

  def checkout(newBranchName: String): Unit = {
    val branch = currentBranch
    if (branch != newBranchName) {
      gitNative(Seq("checkout", "-q", newBranchName))
    }
  }

  def createBranch(branchName: String): Unit = {
    try {
    gitNative(Seq("branch", branchName))
    } catch {
      case e:RuntimeException if e.getMessage.contains("A branch named '" + branchName + "' already exists.") ⇒
        throw new BranchAlreadyExistsException(e.getMessage)
      case t:Throwable ⇒ throw t
    }
  }

  def pushTag(tagName: String): Seq[String] = {
    pushInnerRaw(checkedTagName(tagName))
  }

  def pushHeads(srcBranchName: String, targetBranchName: String): Seq[String] = {
    pushInner(srcBranchName, targetBranchName, "refs/heads/")
  }

  def pushFor(srcBranchName: String, targetBranchName: String): Seq[String] = {
    pushInner(srcBranchName, targetBranchName, "refs/for/")
  }

  private[release] def pushInner(srcBranchName: String, targetBranchName: String, refPrefix: String): Seq[String] = {
    pushInnerRaw(srcBranchName + ':' + refPrefix + targetBranchName)
  }

  private[release] def pushInnerRaw(src: String): Seq[String] = {
    Seq(gitNative(Seq("push", "-q", "-u", "origin", src), errMapper = Sgit.gerritPushFilter))
  }

  private[release] def gitNativeOpt(args: Seq[String]): Option[String] = {
    try {
      Some(gitNative(args, showErrors = false))
    } catch {
      case t: Throwable ⇒ None
    }
  }

  private[release] def gitNative(args: Seq[String], showErrors: Boolean = true, useWorkdir: Boolean = true,
                                 cmdFilter: String ⇒ Boolean = _ ⇒ false,
                                 errMapper: String ⇒ Option[String] = errLine ⇒ Some(errLine)): String = {
    if (!gitRoot.isDirectory) {
      throw new IllegalStateException("invalid git dir: " + gitRoot.getAbsolutePath)
    }
    val workdir = if (useWorkdir) {
      Seq("-C", gitRoot.getAbsolutePath)
    } else {
      Nil
    }
    val gitCmdCall = Sgit.selectedGitCmd(err) ++ workdir ++ Seq("--no-pager") ++ args
    if (showGitCmd) {
      out.println(gitCmdCall.mkString(" "))
    }
    Sgit.native(gitCmdCall, syserrErrors = showErrors, cmdFilter, err, errMapper)
  }
}

object Sgit {

  private var gits = Map.empty[Seq[String], Unit]

  private[release] def checkVersion(sgit: Sgit, out: PrintStream, err: PrintStream): Unit = synchronized {
    val cmd: Seq[String] = selectedGitCmd(err)
    val git = gits.get(cmd)
    if (git.isEmpty) {
      // git lg --date=short --simplify-by-decoration --pretty=format:'(%cd)%d'
      val result: Unit = sgit.gitNative(Seq("--version"), useWorkdir = false) match {
        case v: String if v.startsWith("git version 2.8") ⇒
          // (2016-06-06) - (tag: v2.8.4)
          out.println("W: please update your git version, \"" + v + "\" support ends at 2017-07-01");
        case v: String if v.startsWith("git version 2.9") ⇒
          // (2016-08-12) - (tag: v2.9.3)
          out.println("W: please update your git version, \"" + v + "\" support ends at 2017-08-01");
        case v: String if v.startsWith("git version 2.10") ⇒ // do nothing (2016-10-28) - (tag: v2.10.2)
        case v: String if v.startsWith("git version 2.11") ⇒ // do nothing (2017-02-02) - (tag: v2.11.1)
        case v: String if v.startsWith("git version 2.12") ⇒ // do nothing (2017-03-20) - (tag: v2.12.1)
        case v: String if v.startsWith("git version 2.13") ⇒ // do nothing
        case v: String ⇒ out.println("W: unknown git version: \"" + v + "\"");
      }
      gits = gits ++ Map(cmd → result)
    }
  }

  private[release] def outLogger(syserrErrors: Boolean, cmd: Seq[String],
                                 errOut: String ⇒ Unit, outLog: String ⇒ Unit,
                                 errorLineMapper: String ⇒ Option[String]): ProcessLogger = {
    new ProcessLogger {
      override def out(s: ⇒ String): Unit = {
        outLog.apply(s)
      }

      override def err(s: ⇒ String): Unit = {
        if (syserrErrors && s.nonEmpty) {
          val out = errorLineMapper.apply(s)
          if (out.isDefined) {
            errOut.apply(out.get)

          }
        }
      }

      override def buffer[T](f: ⇒ T): T = f
    }
  }

  private[release] def fetchFilter(line: String) = line match {
    case l: String if l.matches("Total [0-9]+ \\(delta [0-9]+\\), reused [0-9]+ \\(delta [0-9]+\\)") ⇒ None
    case l: String ⇒ Some(l)
  }

  private[release] def gerritPushFilter(line: String): Option[String] = {
    line match {
      case l if l == "remote: " ⇒ None
      case l if l.startsWith("remote: Processing change") ⇒ None
      case l if l.startsWith("remote: error:") ⇒ None
      case l if l.startsWith("To ssh:") ⇒ None
      case l if l.trim == "remote: New Changes:" ⇒ None
      case l if l.startsWith("remote:   http") ⇒ Some(l.replaceFirst(".*http", "See http"))
      case l if l.contains("[remote rejected]") ⇒ Some(l.replaceFirst(".*\\[remote rejected\\]", "[remote rejected]"))
      case l ⇒ Some("git-err: '" + l + "'")
    }

  }

  private[release] def native(cmd: Seq[String], syserrErrors: Boolean,
                              cmdFilter: String ⇒ Boolean, err: PrintStream,
                              errLineMapper: String ⇒ Option[String]): String = {
    import sys.process._

    var errors: String = ""
    var stdout = ""

    def logError(in: String): Unit = {
      err.println(in)
      errors = errors.concat(" ").concat(in).trim
    }

    def logOut(in: String): Unit = {
      stdout = stdout.concat(" ").concat(in).trim
    }

    try {
      val result: String = cmd !! outLogger(syserrErrors, cmd, logError, logOut, errLineMapper)
      result.trim
    } catch {
      case e: RuntimeException if e.getMessage != null &&
        e.getMessage.startsWith("Nonzero exit value:") && cmd.head.contains("git") ⇒
        val msg = e.getMessage + "; git " + cmd.drop(3).mkString(" ") +
          "; " + Seq(errors, stdout).filterNot(_.isEmpty).mkString("; ")
        throw new RuntimeException(msg.trim)
      case e: Throwable ⇒ {
        throw e
      }
    }

  }

  private[release] def selectedGitCmd(err: PrintStream): Seq[String] = {
    val gitCygCmd: Try[Seq[String]] = try {
      Success(Seq(native(Seq("cygpath", "-daw", "/usr/bin/git"), syserrErrors = false, _ ⇒ false, err, s ⇒ Some(s))))
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
  class BranchAlreadyExistsException(msg: String) extends RuntimeException(msg)

  private[release] def findGit(in: File): File = {
    if (new File(in, ".git").exists()) {
      in
    } else {
      findGit(in.getParentFile)
    }
  }

  private[release] def clone(src: File, dest: File, showGitCmd: Boolean = false, verify: Boolean = true): Sgit = {
    val o = Sgit(dest, showGitCmd, doVerify = verify, out = System.out, err = System.err)
    o.clone(src, dest)
    Sgit(dest, showGitCmd, doVerify = false, out = System.out, err = System.err)
  }

  private[release] def init(f: File, showGitCmd: Boolean = false, verify: Boolean = true): Sgit = {
    val sgit = Sgit(f, showGitCmd, doVerify = verify, out = System.out, err = System.err)
    sgit.init()
    sgit
  }
}
