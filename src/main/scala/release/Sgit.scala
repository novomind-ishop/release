package release

import java.io.{File, PrintStream}
import java.math.BigInteger
import java.net.URI
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import release.Conf.Tracer
import release.Sgit.{BranchAlreadyExistsException, GitRemote, GitShaBranch, MissingGitDirException}
import release.Starter.{Opts, PreconditionsException}

import scala.annotation.tailrec
import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.util.{Failure, Success, Try}

case class Sgit(file: File, doVerify: Boolean, out: PrintStream, err: PrintStream,
                checkExisting: Boolean = true, gitBin: Option[String], opts: Starter.Opts) extends LazyLogging {

  private val gitRoot = Sgit.findGit(file.getAbsoluteFile, file.getAbsoluteFile, checkExisting)

  private val dotGit = new File(gitRoot, ".git")

  private def init(): Unit = {
    gitNative(Seq("init", file.getAbsolutePath), useWorkdir = false)
  }

  private def clone(src: File, dest: File): Unit = {
    gitNative(Seq("clone", "-q", src.getAbsolutePath, dest.getAbsolutePath), useWorkdir = false)
  }

  private[release] def isShallowClone: Boolean = {
    Util.only(gitNative(Seq("rev-parse", "--is-shallow-repository")), "only one boolean expected").toBoolean
  }

  private def configRemoveLocalAll(key: String, value: String): Unit = {
    gitNative(Seq("config", "--local", "--unset-all", key, value))
  }

  private[release] def configRemoveLocal(key: String, value: String): Unit = {
    val values = configGetLocalAll(key)
    values match {
      case None => // do nothing
      case s: Some[Seq[String]] if s.get.size == 1 => gitNative(Seq("config", "--local", "--unset", key, value))
      case s: Some[Seq[String]] if s.get.size > 1 => configRemoveLocalAll(key, value)
    }

  }

  private[release] def configAddLocal(key: String, value: String): Unit = {
    gitNative(Seq("config", "--local", "--add", key, value))
  }

  private[release] def configSetGlobal(key: String, value: String): Unit = {
    gitNative(Seq("config", "--global", key, value))
  }

  private[release] def configSetLocal(key: String, value: String): Unit = {
    gitNative(Seq("config", "--local", key, value))
  }

  private[release] def configGetLocalAll(key: String): Option[Seq[String]] = {
    gitNativeOpt(Seq("config", "--local", "--get-all", key))
  }

  private[release] def configGetLocalAllSeq(key: String): Seq[String] = {
    configGetLocalAll(key).getOrElse(Nil).toList
  }

  private[release] def configGetGlobalAll(key: String): Option[Seq[String]] = {
    gitNativeOpt(Seq("config", "--global", "--get-all", key))
  }

  private[release] def configGetGlobalAllSeq(key: String): Seq[String] = {
    configGetGlobalAll(key).getOrElse(Nil).toList
  }

  private[release] def addByString(f: String): Unit = {
    configSetLocal("core.safecrlf", "false")
    gitNative(Seq("add", f))
    configSetLocal("core.safecrlf", "warn")
  }

  private[release] def addAll(f: Seq[String]): Unit = {
    configSetLocal("core.safecrlf", "false")
    gitNative(Seq("add") ++ f)
    configSetLocal("core.safecrlf", "warn")
  }

  private[release] def add(f: File): Unit = {
    addByString(f.getAbsolutePath)
  }

  private[release] def commitAll(message: String): Unit = {
    val lines = message.replaceAll("\r", "").linesIterator.toList
    val msx = if (lines.size == 1) {
      lines ++ Seq("")
    } else {
      lines
    }

    val ms = msx ++ Seq("Change-Id:")

    if (ms(1).nonEmpty) {
      throw new IllegalStateException("non empty line")
    }
    configSetLocal("core.safecrlf", "false")
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
    configSetLocal("core.safecrlf", "warn")
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
        throw new MissingGitDirException("invalid git folder")
      }

      if (!(commitMsgHook.canRead && commitMsgHook.canExecute)) {
        throw new Sgit.MissingCommitHookException(
          """
            |E: please download a commit-message hook and retry
            |E: The hook should be at: %s
            |I: see %s/Documentation/user-changeid.html#creation
            |# scp -p -P %s $USERNAME@%s:hooks/commit-msg .git/hooks/
            |
        """.stripMargin.format(commitMsgHook.getAbsolutePath,
            ReleaseConfig.default(opts.useDefaults).gerritBaseUrl(),
            ReleaseConfig.default(opts.useDefaults).gerritPort(),
            ReleaseConfig.default(opts.useDefaults).gerritHostname()))
      }
    }

    if (dotGit.exists() && checkExisting) {
      checkCommitMessageHook(dotGit)
    } else if (checkExisting) {
      throw new MissingGitDirException("no .git dir in " + file.getAbsolutePath + " was found (2)")
    }

  }

  Sgit.checkVersion(this, out, err, gitBin)
  if (doVerify) {
    verify()
  }

  def currentBranch: String = Util.only(gitNative(Seq("rev-parse", "--abbrev-ref", "HEAD")), "only one branch expected")

  def lsFiles(): Seq[String] = gitNative(Seq("ls-files")).map(_.trim).map(in => if (in.startsWith("\"") && in.endsWith("\"")) {
    Sgit.unescape(in.substring(1).dropRight(1))
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
      case any: Throwable => Failure(any)
    }
  }

  def addRemote(name: String, url: String): Unit = {
    gitNative(Seq("remote", "add", name, url))
  }

  def removeRemote(name: String): Unit = {
    gitNative(Seq("remote", "remove", name))
  }

  def listRemotes(): Seq[GitRemote] = {
    val out = gitNative(Seq("remote", "-v"))
    out.map(in => {
      try {
        val splitted = Sgit.splitLineOnWhitespace(in)
        GitRemote(splitted(0), splitted(1), splitted(2))
      } catch {
        case e: Exception => throw new IllegalStateException("invalid line: \"" + in + "\"", e)
      }
    })
  }

  def listBranchNamesLocal(): Seq[String] = listBranchesLocal().map(_.branchName.replaceFirst("refs/heads/", "")).sorted

  def listBranchesLocal(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev"))
      .flatMap(Sgit.splitLineOnBranchlistErr(err))
      .map(parts => {
        GitShaBranch(parts._2, "refs/heads/" + parts._1)
      }).toList
  }

  def listBranchNamesRemoteShort(): Seq[String] = listBranchNamesRemote().map(_.replaceFirst("origin/", "")).sorted

  def listBranchNamesRemote(): Seq[String] = listBranchRemoteRaw().map(_.branchName).sorted

  def listBranchNamesAll(): Seq[String] = (listBranchNamesRemoteShort() ++ listBranchNamesLocal()).distinct.sorted

  def listBranchNamesAllFull(): Seq[String] = (listBranchNamesRemote() ++ listBranchNamesLocal()).distinct.sorted

  def listBranchRemoteRefRemotes(): Seq[GitShaBranch] = {
    listBranchRemoteRaw().map(in => in.copy(branchName = "refs/remotes/" + in.branchName))
  }

  private[release] def listBranchRemoteRaw(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev", "--remote"))
      .flatMap(in => in match {
        case l: String if l.trim.startsWith("origin/HEAD") => None
        case l: String if l.trim.startsWith("origin/") => {
          val parts = l.replaceFirst("^\\*", "").trim.split("[ \t]+")
          Some(GitShaBranch(parts(1), parts(0)))
        }
        case _: String => None
      }).toList
  }

  def listRefNames(): Seq[String] = listRefs().map(_.branchName)

  def listRefs(): Seq[GitShaBranch] = {
    gitNative(Seq("show-ref"))
      .map(in => {
        val parts = in.replaceFirst("^\\*", "").trim.split("[ \t]+")
        GitShaBranch(parts(0), parts(1))
      }).sortBy(_.branchName).toList
  }

  def deleteRef(refname: String): Unit = {
    updateRef(Seq("-d", refname))
  }

  private def updateRef(args: Seq[String]): Unit = {
    gitNative(Seq("update-ref") ++ args)
  }

  def localPomChanges(): Seq[String] = {
    localChangesWithFilter(Seq("pom.xml"))
  }

  def localChangesWithFilter(filter: Seq[String]): Seq[String] = {
    val modPom = localChanges()
    val modWithoutState = modPom.map(in => in.replaceFirst("^[^ ]+ ", ""))

    val modDistinct = modWithoutState.map(in => filter.foldLeft(in)((i, fi) => i.replaceFirst(".*/" + fi, fi))).distinct.sorted
    val mod = modDistinct.filterNot(in => filter.contains(in))
    if (mod.nonEmpty) {
      throw new IllegalStateException("only (" + filter.mkString(", ") + ") changes are allowed => " +
        modPom.mkString(", ") + " => " + modDistinct.filterNot(in => filter.contains(in)).mkString(", ") + " <= " + modDistinct.mkString(", "))
    }
    modWithoutState
  }

  def doCommitPomXmlsAnd(message: String, otherFilenames: Seq[String]): Unit = {
    doCommitWithFilter(message, Seq("pom.xml") ++ otherFilenames)
  }

  def doCommitPomXmls(message: String): Unit = {
    doCommitPomXmlsAnd(message, Nil)
  }

  private[release] def doCommitWithFilter(message: String, filter: Seq[String]): Unit = {
    val distinctFilter = filter.distinct.sorted
    addAll(localChangesWithFilter(distinctFilter))
    val statusAfterAdd = localChanges().filterNot(line => distinctFilter.exists(key => line.endsWith(key)))
    if (statusAfterAdd.nonEmpty) {
      throw new IllegalStateException("uncommitted changes: " + statusAfterAdd.mkString(", "))
    }

    commitAll(message)
  }

  def setUpstream(upstreamName: String): Unit = {
    gitNative(Seq("branch", "--set-upstream-to=" + upstreamName))
  }

  def findUpstreamBranch(): Option[String] = {
    val upstreamOpt:Option[String] = gitNativeOpt(Seq("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"))
      .map(Util.only(_, "only one upstream expected"))
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
    gitNativeOpt(Seq("log", "-n1", "--pretty=%H", ref)).map(Util.only(_, "only one commitid expected"))
  }

  def commitId(ref: String): String = {
    Util.only(gitNative(Seq("log", "-n1", "--pretty=%H", ref)), "only one commitid expected")
  }

  def commitIds(fromRef: String, toRef: String): Seq[String] = {
    gitNative(Seq("log", "--pretty=%H", fromRef + "..." + toRef))
  }

  def hasChangesToPush: Boolean = {
    val status = gitNative(Seq("status", "--branch", "--porcelain", currentBranch)).mkString(" ")
    status.contains("ahead")
  }

  def isNotDetached: Boolean = !isDetached

  def isDetached: Boolean = {
    val status = gitNative(Seq("status", "--branch", "--porcelain")).mkString(" ")
    status.startsWith("## HEAD (no branch)")
  }

  def localChanges(): Seq[String] = {
    val status = gitNative(Seq("status", "--porcelain"))
    status.map(_.replaceAll("[ ]+", " ").trim)
  }

  def hasLocalChanges: Boolean = {
    localChanges() != Nil
  }

  def stash(): Unit = {
    gitNative(Seq("stash", "-u"), errMapper = Sgit.stashFilter)
  }

  def stashPop(): Unit = {
    // TODO try
    gitNative(Seq("stash", "pop"))
  }

  def rebase(): Unit = {
    // TODO try
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
      gitNative(Seq("log", "HEAD", "--no-color", "--branches", "--remotes", "--tags", "--graph",
        "-3", "--oneline", "--decorate", "--")).mkString("\n")
    } catch {
      case e: Exception => e.printStackTrace(); "no - graph"
    }
  }

  def listTags(): Seq[String] = gitNative(Seq("tag", "--list"))

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
    if (listBranchNamesLocal().contains(branchName)) {
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
      case e: RuntimeException if e.getMessage.contains("A branch named '" + branchName + "' already exists.") =>
        throw new BranchAlreadyExistsException(e.getMessage)
      case t: Throwable => throw t
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
        .map(in => (in, commitMessageBody(in)))
        .filterNot(in => {
          in._2.exists(iin => iin.matches("^Change-Id: I[a-f0-9]{40}"))
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
    gitNative(Seq("push", "-q", "-u", "origin", src), errMapper = Sgit.gerritPushFilter)
  }

  private[release] def revertHead(): Seq[String] = {
    gitNative(Seq("revert", "--no-edit", "HEAD"))
  }

  private[release] def commitMessageBody(commitId: String): Seq[String] = {
    gitNative(Seq("log", "-n1", "--pretty=%b", commitId))
  }

  private[release] def resetHard(commitId: String): Seq[String] = {
    gitNative(Seq("reset", "--hard", commitId))
  }

  private[release] def gitNativeOpt(args: Seq[String]): Option[Seq[String]] = {
    try {
      Some(gitNative(args, showErrors = false))
    } catch {
      case t: Throwable => None
    }
  }

  private[release] def gitNative(args: Seq[String], showErrors: Boolean = true, useWorkdir: Boolean = true,
                                 cmdFilter: String => Boolean = _ => false,
                                 errMapper: String => Option[String] = errLine => Some(errLine)): Seq[String] = {
    if (!gitRoot.isDirectory && checkExisting) {
      throw new IllegalStateException("invalid git dir: " + gitRoot.getAbsolutePath)
    }
    val workdir = if (useWorkdir) {
      Seq("-C", gitRoot.getAbsolutePath)
    } else {
      Nil
    }
    val gitCmdCall = Sgit.selectedGitCmd(err, gitBin) ++ workdir ++ Seq("--no-pager") ++ args

    val ff = Tracer.msgAround[String](gitCmdCall.mkString(" "), logger,
      () => Sgit.native(gitCmdCall, syserrErrors = showErrors, cmdFilter, err, errMapper))
    ff.linesIterator.toList.dropWhile(in => in.trim.isEmpty)
  }
}

object Sgit {

  private[release] def splitLineOnWhitespace(in: String): Seq[String] = in.replaceFirst("^\\*", "")
    .trim.split("[ \t]+").toIndexedSeq

  private[release] def splitLineOnBranchlist(in: String): Option[(String, String)] = {
    splitLineOnBranchlistErr(System.err)(in)
  }

  private[release] def splitLineOnBranchlistErr(err: PrintStream)(in: String): Option[(String, String)] = {
    val trimmed = in.replaceFirst("^\\*", "").trim.replaceFirst("^\\([^\\)]+", "(branch_name_replaced")
    val out = trimmed.split("[ \t]+").toList
    val result = out.flatMap(in => if (in == "(branch_name_replaced)") {
      Seq(out(1))
    } else {
      Seq(in)
    })
    if (result.size >= 2 && GitShaBranch.matchesGitSha(result(1))) {
      Some(result.head, result(1))
    } else {
      err.println("W: Unknown branch definition (check commit messages for second line empty, first line char limit):" +
        " \"" + in + "\". See: git branch --list --verbose --no-abbrev")
      None
    }
  }

  sealed case class GitShaBranch(commitId: String, branchName: String) {
    if (commitId.length != 40) {
      throw new IllegalStateException("invalid commit id length: " + commitId.length + " (" + commitId + ")")
    }

    if (!GitShaBranch.matchesGitSha(commitId)) {
      throw new IllegalStateException("invalid commit id: " + commitId + "")
    }
  }

  object GitShaBranch {
    def matchesGitSha(in: String): Boolean = in.matches("[0-9a-f]{40}")
  }

  case class GitRemote(name: String, position: String, remoteType: String)

  private var gits = Map.empty[Seq[String], Unit]

  private[release] def checkVersion(sgit: Sgit, out: PrintStream, err: PrintStream, gitBin: Option[String]): Unit = synchronized {
    val cmd: Seq[String] = selectedGitCmd(err, gitBin)
    val git = gits.get(cmd)
    if (git.isEmpty) {
      // git lg --tags --date=short --simplify-by-decoration --pretty=format:'(%cd)%d'
      val result: Unit = sgit.gitNative(Seq("--version"), useWorkdir = false).mkString("") match {
        case v: String if v.startsWith("git version 1") =>
          // (2014-12-17) - (tag: v1.9.5)
          throw new IllegalStateException("git version 1.9.5 support ended at 2016-01-01")
        case v: String if v.startsWith("git version 2.0.") =>
          throw new IllegalStateException("git version 2.0 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.1.") =>
          throw new IllegalStateException("git version 2.1 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.2.") =>
          throw new IllegalStateException("git version 2.2 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.3.") =>
          throw new IllegalStateException("git version 2.3 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.4.") =>
          throw new IllegalStateException("git version 2.4 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.5.") =>
          throw new IllegalStateException("git version 2.5 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.6.") => // (2017-05-05) - (tag: v2.6.7)
          throw new IllegalStateException("git version 2.6 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.7.") => // (2017-07-30) - (tag: v2.7.6)
          throw new IllegalStateException("git version 2.7 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.8.") => // (2016-06-06) - (tag: v2.8.4)
          throw new IllegalStateException("git version 2.8 support ended at 2017-07-01")
        case v: String if v.startsWith("git version 2.9.") => // (2016-08-12) - (tag: v2.9.3)
          throw new IllegalStateException("git version 2.9 support ended at 2017-08-01")
        case v: String if v.startsWith("git version 2.10.") => // (2016-10-28) - (tag: v2.10.2)
          throw new IllegalStateException("git version 2.10 support ended at 2017-11-01")
        case v: String if v.startsWith("git version 2.11.") => // 2017-02-02) - (tag: v2.11.1)
          throw new IllegalStateException("git version 2.11 support ended at 2017-11-01")
        case v: String if v.startsWith("git version 2.12.") => // (2017-03-20) - (tag: v2.12.1)
          throw new IllegalStateException("git version 2.12 support ended at 2018-04-02")
        case v: String if v.startsWith("git version 2.13.") => // (2017-09-22) - (tag: v2.13.6)
          throw new IllegalStateException("git version 2.13 support ended at 2018-05-02")
        case v: String if v.startsWith("git version 2.14.") => // (2017-10-23) - (tag: v2.14.3)
          throw new IllegalStateException("git version 2.13 support ended at 2018-07-02")
        case v: String if v.startsWith("git version 2.15.") => // (2017-11-28) - (tag: v2.15.1)
          if (ReleaseConfig.isTravisCi()) {
            err.println("W: please update your git version, \"" + v + "\" support ends at 2018-11-02")
          } else {
            throw new IllegalStateException("git version 2.15 support ended at 2018-10-18 because of CVE-2018-17456")
          }
        case v: String if v.startsWith("git version 2.16.") => // (2018-02-15) - (tag: v2.16.2)
          if (ReleaseConfig.isTravisCi()) {
            err.println("W: please update your git version, \"" + v + "\" support ends at 2018-12-02")
          } else {
            throw new IllegalStateException("git version 2.16 support ended at 2018-10-18 because of CVE-2018-17456")
          }
        case v: String if v.startsWith("git version 2.17.") => // do nothing (2018-04-02) - (tag: v2.17.0)
          if (!ReleaseConfig.isTravisCi()) {
            throw new IllegalStateException("git version 2.17 support ended at 2018-10-18 because of CVE-2018-17456")
          }
        case v: String if v.startsWith("git version 2.18.") => // do nothing (2018-06-21) - (tag: v2.18.0)
          if (!ReleaseConfig.isTravisCi()) {
            throw new IllegalStateException("git version 2.18 support ended at 2018-10-18 because of CVE-2018-17456")
          }
        case v: String if v.startsWith("git version 2.19.0") => // do nothing (2018-09-10) - (tag: v2.19.0)
          if (!ReleaseConfig.isTravisCi()) {
            if (Sgit.getOs == Os.Darwin) {
              // https://git-scm.com/download/mac
              // latest public release is 2.19.0 from 2018-09-27
              err.println("W: git version 2.19.0 support ended at 2018-10-18 because of CVE-2018-17456, but no newer releases are available")
            } else {
              throw new IllegalStateException("git version 2.19.0 support ended at 2018-10-18 because of CVE-2018-17456")
            }
          }
        case v: String if v.startsWith("git version 2.19.") => // do nothing (2018-09-27) - (tag: v2.19.1) -- fixes CVE-2018-17456
        case v: String if v.startsWith("git version 2.20.") => // do nothing (2018-12-09) (tag: v2.20.0)
        case v: String => out.println("W: unknown/untested git version: \"" + v + "\". Please create a ticket at ISPS.");
      }
      gits = gits ++ Map(cmd -> result)
    }
  }

  private[release] def outLogger(syserrErrors: Boolean, cmd: Seq[String],
                                 errOut: String => Unit, outLog: String => Unit,
                                 errorLineMapper: String => Option[String]): ProcessLogger = {
    new ProcessLogger {
      override def out(s: => String): Unit = {
        outLog.apply(s)
      }

      override def err(s: => String): Unit = {
        if (syserrErrors && s.nonEmpty) {
          val out = errorLineMapper.apply(s)
          if (out.isDefined) {
            errOut.apply(out.get)

          }
        }
      }

      override def buffer[T](f: => T): T = f
    }
  }

  private[release] def stashFilter(line: String) = line match {
    case l: String if l.endsWith("warning: command substitution: ignored null byte in input") => None
    case l: String => Some(l)
  }

  private[release] def fetchFilter(line: String) = line match {
    case l: String if l.matches("Total [0-9]+ \\(delta [0-9]+\\), reused [0-9]+ \\(delta [0-9]+\\)") => None
    case l: String => Some(l)
  }

  private[release] def gerritPushFilter(line: String): Option[String] = {
    line match {
      case l if l == "remote: " => None
      case l if l.startsWith("remote: Processing change") => None
      case l if l.startsWith("remote: error:") => None
      case l if l.startsWith("To ssh:") => None
      case l if l.trim == "remote: New Changes:" => None
      case l if l.startsWith("remote:   http") => Some(l.replaceFirst(".*http", "See http"))
      case l if l.contains("[remote rejected]") => Some(l.replaceFirst(".*\\[remote rejected\\]", "[remote rejected]"))
      case l => Some("git-err: '" + l + "'")
    }

  }

  private[release] def native(cmd: Seq[String], syserrErrors: Boolean,
                              cmdFilter: String => Boolean, err: PrintStream,
                              errLineMapper: String => Option[String]): String = {
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
        e.getMessage.startsWith("Nonzero exit value:") && cmd.head.contains("git") =>
        val msg = e.getMessage + "; git " + cmd.drop(3).mkString(" ") +
          "; " + Seq(errors, stdout).filterNot(_.isEmpty).mkString("; ")
        if (e.getMessage.startsWith("Nonzero exit value: 130")) {
          throw new TerminatedByCtrlCException(msg.trim)
        } else {
          throw new RuntimeException(msg.trim)
        }
      case e: Throwable => {
        throw e
      }
    }

  }

  private[release] def selectedGitCmd(err: PrintStream, gitBin: Option[String]): Seq[String] = {
    if (gitBin.isDefined) {
      gitBin.toSeq
    } else {
      val gitCygCmd: Try[Seq[String]] = try {
        Success(Seq(native(Seq("cygpath", "-daw", "/usr/bin/git"), syserrErrors = false, _ => false, err, s => Some(s))))
      } catch {
        case e: Exception => Failure(e)
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

  def unescape(str: String): String = {
    @tailrec
    def cate(in: Seq[Char], result: Seq[Byte] = Nil): Seq[Byte] = {
      in match {
        case '\\' :: c0 :: c1 :: c2 :: tail => {
          val b = new BigInteger(new String(Array(c0, c1, c2)), 8).byteValue()
          cate(tail, result :+ b)
        }
        case c :: tail => cate(tail, result :+ c.toByte)
        case Nil => result
      }
    }

    if (str.matches(".*\\\\[0-9]{3}.*")) {
      new String(cate(str.toList).toArray, StandardCharsets.UTF_8)
    } else {
      str
    }
  }

  class MissingCommitHookException(msg: String) extends RuntimeException(msg)

  class MissingGitDirException(msg: String) extends RuntimeException(msg)

  class TerminatedByCtrlCException(msg: String) extends RuntimeException(msg)

  class BranchAlreadyExistsException(msg: String) extends RuntimeException(msg)

  private[release] def findGit(start: File, in: File, checkExisting: Boolean): File = {
    if (new File(in, ".git").exists()) {
      in
    } else if (checkExisting) {
      throw new MissingGitDirException("no .git dir in " + start.getAbsolutePath + " was found")
    } else {
      in
    }
  }

  private[release] def clone(src: File, dest: File, verify: Boolean = true): Sgit = {
    val o = Sgit(dest, doVerify = verify, out = System.out, err = System.err, checkExisting = false, None, Opts())
    o.clone(src, dest)
    o.copy(doVerify = false, checkExisting = true)
  }

  private[release] def init(f: File, verify: Boolean = true): Sgit = {
    val sgit = Sgit(f, doVerify = verify, out = System.out, err = System.err, checkExisting = false, None, Opts())
    sgit.init()
    sgit
  }

  object Os {
    val Windows = Os("win")
    val Linux = Os("Linux")
    val Darwin = Os("Darwin")
  }

  sealed case class Os(name: String)

  lazy val getOs: Os = {
    System.getProperty("os.name") match {
      case "Windows 10" => Os.Windows
      case "Linux" => Os.Linux
      case "Mac OS X" => Os.Darwin
      case other => throw new IllegalStateException("unknown os: " + other)
    }
  }
}
