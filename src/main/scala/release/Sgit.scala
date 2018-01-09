package release

import java.io.{File, PrintStream}
import java.net.URI

import com.typesafe.scalalogging.LazyLogging
import release.Sgit.{BranchAlreadyExistsException, GitRemote, GitShaBranch, MissigGitDirException}
import release.Starter.PreconditionsException

import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.util.{Failure, Success, Try}

case class Sgit(file: File, showGitCmd: Boolean, doVerify: Boolean, out: PrintStream, err: PrintStream,
                checkExisting: Boolean = true, gitBin: Option[String]) extends LazyLogging {

  private val gitRoot = Sgit.findGit(file.getAbsoluteFile, file.getAbsoluteFile, checkExisting)

  private val dotGit = new File(gitRoot, ".git")

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
    val msx = if (lines.size == 1) {
      lines ++ Seq("")
    } else {
      lines
    }

    val ms = msx ++ Seq("Change-Id:")

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

    def doCommit(id: String) =
      gitNative(Seq("commit", "--no-verify", "-m",
        ms.map(_.replaceFirst("^Change-Id:", "Change-Id: I" + id)).mkString("\n").trim))

    if (oldComitId.isDefined) {
      gitNative(Seq("commit", "--no-verify", "-m", ms.mkString("\n").trim))
      val newCommitId = commitIdHead()
      gitNative(Seq("reset", oldComitId.get))
      gitNative(Seq("add", "-A"))
      doCommit(newCommitId)
    } else {
      doCommit("0000000000000000000000000000000000000000")
      out.println("W: no old commit id")
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
        throw new MissigGitDirException("invalid git folder")
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

    if (dotGit.exists() && checkExisting) {
      checkCommitMessageHook(dotGit)
    } else if (checkExisting) {
      throw new MissigGitDirException("no .git dir in " + file.getAbsolutePath + " was found (2)")
    }

  }

  Sgit.checkVersion(this, out, err, gitBin)

  if (doVerify) {
    verify()
  }

  def currentBranch: String = gitNative(Seq("rev-parse", "--abbrev-ref", "HEAD"))

  def lsFiles(): Seq[String] = gitNative(Seq("ls-files")).lines.toList.map(_.trim).map(in ⇒ if (in.startsWith("\"") && in.endsWith("\"")) {
    println("W: missing standard c char unescaper. Provide a patch if you see this warning")
    // https://git-scm.com/docs/git-config#git-config-corequotePath
    in
  } else {
    in
  })

  def fetchAll(): Unit = {
    gitNative(Seq("fetch", "-q", "--all", "--tags"), errMapper = Sgit.fetchFilter)
  }

  def tryFetchAll(): Try[Unit] = {
    try {
      Success(fetchAll())
    } catch {
      case any: Throwable ⇒ Failure(any)
    }
  }

  def remoteAdd(name: String, url: String): Unit = {
    gitNative(Seq("remote", "add", name, url))
  }

  def remoteRemove(name: String): Unit = {
    gitNative(Seq("remote", "remove", name))
  }

  def remoteList(): Seq[GitRemote] = {
    val out = gitNative(Seq("remote", "-v"))
    out.lines.toList.map(in ⇒ {
      val splitted = Sgit.splitLineOnWhitespace(in)
      GitRemote(splitted(0), splitted(1), splitted(2))
    })
  }

  def branchNamesLocal(): Seq[String] = branchListLocal().map(_.branchName.replaceFirst("refs/heads/", "")).sorted

  def branchListLocal(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev"))
      .lines.toSeq
      .map(in ⇒ {
        val parts = Sgit.splitLineOnBranchlist(in)
        GitShaBranch(parts(1), "refs/heads/" + parts(0))
      }).toList
  }

  def branchNamesRemoteShort(): Seq[String] = branchNamesRemote().map(_.replaceFirst("origin/", "")).sorted

  def branchNamesRemote(): Seq[String] = branchListRemoteRaw().map(_.branchName).sorted

  def branchNamesAll(): Seq[String] = (branchNamesRemoteShort() ++ branchNamesLocal()).distinct.sorted

  private[release] def branchListRemoteRaw(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev", "--remote"))
      .lines.toSeq
      .flatMap(in ⇒ in match {
        case l: String if l.trim.startsWith("origin/HEAD") ⇒ None
        case l: String if l.trim.startsWith("origin/") ⇒ {
          val parts = l.replaceFirst("^\\*", "").trim.split("[ \t]+")
          Some(GitShaBranch(parts(1), parts(0)))
        }
        case _: String ⇒ None
      }).toList
  }

  def branchListRemoteRefRemotes(): Seq[GitShaBranch] = {
    branchListRemoteRaw().map(in ⇒ in.copy(branchName = "refs/remotes/" + in.branchName))
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
    val changes = gitNative(Seq("log", "--pretty=%H", fromRef + "..." + toRef))
    changes.lines.toList
  }

  def hasChangesToPush: Boolean = {
    val status = gitNative(Seq("status", "--branch", "--porcelain", currentBranch))
    status.contains("ahead")
  }

  def isNotDetached: Boolean = !isDetached

  def isDetached: Boolean = {
    val status = gitNative(Seq("status", "--branch", "--porcelain"))
    status.startsWith("## HEAD (no branch)")
  }

  def localChanges(): Seq[String] = {
    val status = gitNative(Seq("status", "--porcelain"))
    status.lines.toList.map(_.replaceAll("[ ]+", " ").trim)
  }

  def hasLocalChanges: Boolean = {
    localChanges() != Nil
  }

  def stash(): Unit = {
    gitNative(Seq("stash", "-u"))
  }

  def stashPop(): Unit = {
    gitNative(Seq("stash", "pop"))
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
      case e: RuntimeException if e.getMessage.contains("A branch named '" + branchName + "' already exists.") ⇒
        throw new BranchAlreadyExistsException(e.getMessage)
      case t: Throwable ⇒ throw t
    }
  }

  def pushTag(tagName: String): Seq[String] = {
    pushInnerRaw(checkedTagName(tagName))
  }

  def pushHeads(srcBranchName: String, targetBranchName: String): Seq[String] = {
    pushInner(srcBranchName, targetBranchName, "refs/heads/")
  }

  def pushFor(srcBranchName: String, targetBranchName: String): Seq[String] = {
    if (findUpstreamBranch().isDefined) {
      val commitsWithoutChangeId = commitIds("@{upstream}", currentBranch)
        .map(in ⇒ (in, commitMessageBody(in)))
        .filterNot(in ⇒ {
          in._2.exists(iin ⇒ iin.matches("^Change-Id: I[a-f0-9]{40}"))
        })

      if (commitsWithoutChangeId.nonEmpty) {
        throw new PreconditionsException("The commits " + commitsWithoutChangeId.map(_._1).mkString(", ") +
          " has no ChangeId lines. Please amend them manually.")
      }
    }
    pushInner(srcBranchName, targetBranchName, "refs/for/")
  }

  private[release] def pushInner(srcBranchName: String, targetBranchName: String, refPrefix: String): Seq[String] = {
    pushInnerRaw(srcBranchName + ':' + refPrefix + targetBranchName)
  }

  private[release] def pushInnerRaw(src: String): Seq[String] = {
    Seq(gitNative(Seq("push", "-q", "-u", "origin", src), errMapper = Sgit.gerritPushFilter))
  }

  private[release] def revertHead(): Seq[String] = {
    Seq(gitNative(Seq("revert", "--no-edit", "HEAD")))
  }

  private[release] def commitMessageBody(commitId: String): Seq[String] = {
    gitNative(Seq("log", "-n1", "--pretty=%b", commitId)).split("[\r\n]+").toList
  }

  private[release] def resetHard(commitId: String): Seq[String] = {
    Seq(gitNative(Seq("reset", "--hard", commitId)))
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
    if (!gitRoot.isDirectory && checkExisting) {
      throw new IllegalStateException("invalid git dir: " + gitRoot.getAbsolutePath)
    }
    val workdir = if (useWorkdir) {
      Seq("-C", gitRoot.getAbsolutePath)
    } else {
      Nil
    }
    val gitCmdCall = Sgit.selectedGitCmd(err, gitBin) ++ workdir ++ Seq("--no-pager") ++ args
    if (showGitCmd) {
      out.println(gitCmdCall.mkString(" "))
    }
    val ff = Sgit.native(gitCmdCall, syserrErrors = showErrors, cmdFilter, err, errMapper)
    ff
  }
}

object Sgit {

  private[release] def splitLineOnWhitespace(in: String): Seq[String] = in.replaceFirst("^\\*", "").trim.split("[ \t]+")

  private[release] def splitLineOnBranchlist(in: String): Seq[String] = {
    val trimmed = in.replaceFirst("^\\*", "").trim.replaceFirst("^\\([^\\)]+", "(branch_name_replaced")
    val out = trimmed.split("[ \t]+").toList
    out.flatMap(in ⇒ if (in == "(branch_name_replaced)") {
      Seq(out(1))
    } else {
      Seq(in)
    })
  }

  case class GitShaBranch(commitId: String, branchName: String) {
    if (commitId.length != 40) {
      throw new IllegalStateException("invalid commit id length: " + commitId.length + " (" + commitId + ")")
    }

    if (!commitId.matches("[0-9a-f]{40}")) {
      throw new IllegalStateException("invalid commit id: " + commitId + "")
    }
  }

  case class GitRemote(name: String, position: String, remoteType: String)

  private var gits = Map.empty[Seq[String], Unit]

  private[release] def checkVersion(sgit: Sgit, out: PrintStream, err: PrintStream, gitBin: Option[String]): Unit = synchronized {
    val cmd: Seq[String] = selectedGitCmd(err, gitBin)
    val git = gits.get(cmd)
    if (git.isEmpty) {
      // git lg --tags --date=short --simplify-by-decoration --pretty=format:'(%cd)%d'
      val result: Unit = sgit.gitNative(Seq("--version"), useWorkdir = false) match {
        case v: String if v.startsWith("git version 1") ⇒
          // (2014-12-17) - (tag: v1.9.5)
          throw new IllegalStateException("git version 1.9.5 support ended at 2016-01-01")
        case v: String if v.startsWith("git version 2.8") ⇒
          // (2016-06-06) - (tag: v2.8.4)
          throw new IllegalStateException("git version 2.8 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.9") ⇒
          // (2016-08-12) - (tag: v2.9.3)
          throw new IllegalStateException("git version 2.9 support ended at 2017-08-01")
        case v: String if v.startsWith("git version 2.10") ⇒
          // (2016-10-28) - (tag: v2.10.2)
          out.println("W: please update your git version, \"" + v + "\" support ends at 2017-11-01");
        case v: String if v.startsWith("git version 2.11") ⇒
          // 2017-02-02) - (tag: v2.11.1)
          out.println("W: please update your git version, \"" + v + "\" support ends at 2017-11-01");
        case v: String if v.startsWith("git version 2.12") ⇒ // do nothing (2017-03-20) - (tag: v2.12.1)
        case v: String if v.startsWith("git version 2.13") ⇒ // do nothing (2017-09-22) - (tag: v2.13.6)
        case v: String if v.startsWith("git version 2.14") ⇒ // do nothing (2017-10-23) - (tag: v2.14.3)
        case v: String if v.startsWith("git version 2.15") ⇒ // do nothing
        case v: String ⇒ out.println("W: unknown/untested git version: \"" + v + "\". Please create a ticket at ISPS.");
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

  private[release] def selectedGitCmd(err: PrintStream, gitBin: Option[String]): Seq[String] = {
    if (gitBin.isDefined) {
      gitBin.toSeq
    } else {
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
  }

  class MissigCommitHookException(msg: String) extends RuntimeException(msg)

  class MissigGitDirException(msg: String) extends RuntimeException(msg)

  class BranchAlreadyExistsException(msg: String) extends RuntimeException(msg)

  private[release] def findGit(start: File, in: File, checkExisting: Boolean): File = {
    if (new File(in, ".git").exists()) {
      in
    } else if (checkExisting) {
      throw new MissigGitDirException("no .git dir in " + start.getAbsolutePath + " was found")
    } else {
      in
    }
  }

  private[release] def clone(src: File, dest: File, showGitCmd: Boolean = false, verify: Boolean = true): Sgit = {
    val o = Sgit(dest, showGitCmd, doVerify = verify, out = System.out, err = System.err, checkExisting = false, None)
    o.clone(src, dest)
    o.copy(doVerify = false, checkExisting = true)
  }

  private[release] def init(f: File, showGitCmd: Boolean = false, verify: Boolean = true): Sgit = {
    val sgit = Sgit(f, showGitCmd, doVerify = verify, out = System.out, err = System.err, checkExisting = false, None)
    sgit.init()
    sgit
  }
}
