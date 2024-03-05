package release

import com.google.common.base.Strings
import com.typesafe.scalalogging.LazyLogging
import release.Conf.Tracer
import release.Sgit._
import release.Starter.{Opts, PreconditionsException}

import java.io.{File, PrintStream}
import java.math.BigInteger
import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.ZonedDateTime
import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.annotation.tailrec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Failure, Success, Try}

trait SgitVersion {
  def version(): String
}

trait SgitDiff {
  def diffSafe(): Seq[String]
}

trait SgitDetached {
  def isNotDetached: Boolean = !isDetached

  def isDetached: Boolean
}

case class Sgit(file: File, doVerify: Boolean, out: PrintStream, err: PrintStream,
                checkExisting: Boolean = true, checkGitRoot: Boolean = true,
                gitBin: Option[String], opts: Starter.Opts) extends LazyLogging with SgitVersion with SgitDiff with SgitDetached {

  private val gitRoot: File = if (checkGitRoot) {
    Sgit.findGit(file.getAbsoluteFile, checkExisting)
  } else {
    null
  }

  private val dotGit = new File(gitRoot, ".git")

  def version(): String = {
    gitNative(Seq("--version"), useWorkdir = false).mkString("")
  }

  private def init(): Unit = {
    gitNative(Seq("init", file.getAbsolutePath), useWorkdir = false)
  }

  private def cloneRemote(src: String, dest: File, depth: Int): Unit = {
    val shalow: Seq[String] = if (depth > 0) {
      Seq("--depth", depth.toString)
    } else {
      Nil
    }
    gitNative(Seq("clone") ++ shalow ++ Seq("-q", src, dest.getAbsolutePath), useWorkdir = false)
  }

  private def clone(src: File, dest: File): Unit = {
    gitNative(Seq("clone") ++ Seq("-q", src.getAbsolutePath, dest.getAbsolutePath), useWorkdir = false)
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
      case s: Some[Seq[String]] => throw new IllegalStateException(s.toString)
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

    def doCommit(changeId: String): Seq[String] = {
      gitNative(Seq("commit", "--no-verify", "-m",
        ms.map(_.replaceFirst("^Change-Id:", "Change-Id: I" + changeId)).mkString("\n").trim))
    }

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
            |Hint: if this not a gerrit repo use '--no-gerrit' or
            |      export RELEASE_NO_GERRIT=true
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

  def currentBranch: String = Util.only(gitNative(Seq("rev-parse", "--abbrev-ref", "HEAD"), showErrorsOnStdErr = false), "only one branch expected")

  def currentBranchOpt: Option[String] = {
    try {
      Some(currentBranch)
    } catch {
      case _: Throwable => None
    }
  }

  def currentTags: Option[Seq[String]] = {
    if (currentBranchOpt == Some("HEAD")) {
      try {
        Some(gitNative(Seq("describe", "--tags"), showErrorsOnStdErr = false))
      } catch {
        case _: Throwable => None
      }
    } else {
      None
    }
  }

  def lsFiles(): Seq[String] = gitNative(Seq("ls-files")).map(_.trim).map(in => if (in.startsWith("\"") && in.endsWith("\"")) {
    Sgit.unescape(in.substring(1).dropRight(1))
  } else {
    in
  })

  def lsFilesAbsolute(): Seq[File] = {
    lsFiles().map(entry => new File(gitRoot, entry).getAbsoluteFile)
  }

  def fetchAll(): Unit = {
    gitNative(Seq("fetch", "--all", "--tags"), errMapper = Sgit.fetchFilter())
  }

  def tryFetchAll(): Try[Unit] = {
    try {
      Success(fetchAll())
    } catch {
      case any: Throwable => Failure(any)
    }
  }

  def remotePruneOrigin(): Unit = {
    gitNative(Seq("remote", "prune", "origin"))
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

  def listBranchNamesLocal(): Seq[String] = {
    listBranchesLocal().map(_.branchName.replaceFirst("refs/heads/", "")).sorted
  }

  def listBranchesLocal(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev"))
      .flatMap(Sgit.splitLineOnBranchlistErr(err))
      .flatMap(in => if (in._1 == in._2) {
        None
      } else {
        Some(in)
      })
      .map(parts => {
        GitShaBranch(parts._2, "refs/heads/" + parts._1)
      }).toList
  }

  def listBranchNamesRemoteShort(): Seq[String] = {
    listBranchNamesRemote().map(_.replaceFirst("origin/", "")).sorted
  }

  def remoteHead(timeout: Duration = Duration(10, TimeUnit.SECONDS)): Try[Option[String]] = {
    val executor = Executors.newSingleThreadExecutor() // TODO reuse executor later
    val call = new Callable[Option[String]] {
      override def call(): Option[String] = {
        try {
          val value = gitNative(Seq("remote", "show", "origin"), showErrorsOnStdErr = false)
            .filter(_.startsWith("  HEAD branch"))
            .map(_.trim)

          value.headOption
        } catch {
          case _: Throwable => None
        }
      }
    }
    val future = executor.submit(call)
    val result: Option[String] = try {
      future.get(timeout._1, timeout._2)
    } catch {
      case te: TimeoutException => {
        future.cancel(true)
        return Failure(te)
      }
    } finally {
      executor.shutdownNow();
    }
    Success(result)
  }

  def remoteHeadRaw(): Try[Option[String]] = {
    Sgit.toRawRemoteHead(remoteHead())
  }

  private[release] def listCommitterNames(): Seq[String] = {
    gitNative(Seq("log", "--all", "--pretty=%an", "--since=14.days")).distinct.sorted
  }

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
    val upstreamOpt: Option[String] = gitNativeOpt(Seq("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"))
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

  def hasTagsToPush: Boolean = {
    try {
      tagsToPush.exists(_.contains("*"))
    } catch {
      case _: Exception => false
    }
  }

  private def tagsToPush: Seq[String] = {
    gitNative(Seq("push", "--tags", "--dry-run", "--porcelain"))
  }

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

  def worktreeAdd(f: String): Unit = {
    gitNative(Seq("worktree", "add", f))
  }

  def worktreeRemove(f: String): Unit = {
    gitNative(Seq("worktree", "remove", f))
    deleteBranch(f)
  }

  def diff(): Seq[String] = {
    diff(Nil)
  }

  def diff(f: String): Seq[String] = {
    diff(Seq(f))
  }

  def diff(opts: Seq[String]): Seq[String] = {
    gitNative(Seq("diff") ++ opts)
  }

  def diffSafe(): Seq[String] = {
    val innerDiff = try {
      diff("HEAD")
    } catch {
      case _: RuntimeException => diff("--cached")
      case e: Throwable => throw e
    }
    innerDiff.filterNot(_.startsWith("index "))
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

  def logShortOpt(limit: Int = -1, path: String = ""): Option[String] = {
    logTry(limit, path, Seq("--online")) match {
      case Success(k) => k.headOption
      case Failure(k) => {
        None
      }
    }
  }

  def logTry(limit: Int = -1, path: String = "", args: Seq[String] = Nil): Try[Seq[String]] = {
    try {
      val pElement = if (Strings.isNullOrEmpty(path)) {
        "HEAD"
      } else {
        path
      }
      val limitElements: Seq[String] = if (limit > 0) {
        Seq("-n", limit.toString)
      } else {
        Nil
      }
      val value = Seq("log", pElement) ++ limitElements ++ Seq("--no-color", "--branches", "--remotes", "--tags", "--")
      Success(gitNative(value, showErrorsOnStdErr = false).map(_.trim))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def log(limit: Int = -1, path: String = ""): String = {
    val result = logTry(limit, path)
    result match {
      case Success(k) => k.mkString("\n")
      case Failure(k) => {
        k.printStackTrace();
        "no - log"
      }
    }

  }

  def logGraph(): String = {
    try {
      gitNative(Seq("log", "HEAD", "--no-color", "--branches", "--remotes", "--tags", "--graph",
        "-3", "--oneline", "--decorate", "--"))
        .map(_.trim).mkString("\n")
    } catch {
      case e: Exception => e.printStackTrace(); "no - graph"
    }
  }

  def listTags(): Seq[String] = gitNative(Seq("tag", "--list"))

  def listTagsWithDate(): Seq[GitTagWithDate] = {
    Sgit.parseTagLog(gitNative(Seq("log", "--tags", "--simplify-by-decoration", "--pretty=%aI%d")))
      .sortBy(-_.date.toEpochSecond)
  }

  private def checkedTagName(tagName: String): String = {
    if (tagName.startsWith("v")) {
      throw new IllegalArgumentException("tags must not start with 'v', but was " + tagName)
    }
    "v" + tagName
  }

  def deleteTag(tagName: String): Unit = {
    val deleteTag = checkedTagName(tagName)
    val allTags = listTags()
    if (allTags.contains(deleteTag)) {
      gitNative(Seq("tag", "-d", deleteTag))
    } else {
      throw new IllegalStateException("tag " + deleteTag + " could not found")
    }
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
      Some(gitNative(args, showErrorsOnStdErr = false))
    } catch {
      case t: Throwable => None
    }
  }

  private[release] def gitNative(args: Seq[String], showErrorsOnStdErr: Boolean = true, useWorkdir: Boolean = true,
                                 cmdFilter: String => Boolean = _ => false,
                                 errMapper: String => Option[String] = errLine => Some(errLine)): Seq[String] = {
    if (checkExisting && !gitRoot.isDirectory) {
      throw new IllegalStateException("invalid git dir: " + gitRoot.getAbsolutePath)
    }
    val workdir = if (useWorkdir) {
      Seq("-C", gitRoot.getAbsolutePath)
    } else {
      Nil
    }
    val gitCmdCall = Sgit.selectedGitCmd(err, gitBin) ++ workdir ++ Seq("--no-pager") ++ args

    val ff = Tracer.msgAround[String](gitCmdCall.mkString(" "), logger,
      () => Sgit.native(gitCmdCall, syserrErrors = showErrorsOnStdErr, cmdFilter, err, errMapper))
    ff.linesIterator.toList.dropWhile(in => in.trim.isEmpty)
  }
}

object Sgit {
  def toRawRemoteHead(remoteHead: Try[Option[String]]): Try[Option[String]] = {
    remoteHead.map(_.map(_.replaceFirst("HEAD branch: ", "origin/")))
  }

  def versionOnly(): SgitVersion = {
    Sgit(null, doVerify = false, System.out, System.err,
      checkExisting = false, checkGitRoot = false,
      gitBin = None, opts = Opts())
  }

  private[release] def splitLineOnWhitespace(in: String): Seq[String] = in.replaceFirst("^\\*", "")
    .trim.split("[ \t]+").toIndexedSeq

  private[release] def splitLineOnBranchlist(in: String): Option[(String, String)] = {
    splitLineOnBranchlistErr(System.err)(in)
  }

  private[release] def splitLineOnBranchlistErr(err: PrintStream)(in: String): Option[(String, String)] = {
    val trimmed = in.replaceFirst("^\\*", "")
      .replaceFirst("^\\+", "") // worktree
      .trim.replaceFirst("^\\([^\\)]+", "(branch_name_replaced")
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

  case class GitTagWithDate(name: String, date: ZonedDateTime)

  private var gits = Map.empty[Seq[String], Unit]

  private[release] def checkVersion(sgitVersion: SgitVersion, out: PrintStream, err: PrintStream, gitBin: Option[String]): Unit = synchronized {
    val cmd: Seq[String] = selectedGitCmd(err, gitBin)
    val cmdLine = cmd.mkString(" ")
    val git = gits.get(cmd)
    if (git.isEmpty) {

      val result: Unit = sgitVersion.version() match {
        case v: String if v.startsWith("git version 1") => // (2014-12-17) - (tag: v1.9.5)
          throw new YourGitInstallationIsToOldException("1.9.5", "2016-01-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.0.") =>
          throw new YourGitInstallationIsToOldException("2.0", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.1.") =>
          throw new YourGitInstallationIsToOldException("2.1", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.2.") =>
          throw new YourGitInstallationIsToOldException("2.2", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.3.") =>
          throw new YourGitInstallationIsToOldException("2.3", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.4.") =>
          throw new YourGitInstallationIsToOldException("2.4", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.5.") =>
          throw new YourGitInstallationIsToOldException("2.5", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.6.") => // (2017-05-05) - (tag: v2.6.7)
          throw new YourGitInstallationIsToOldException("2.6", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.7.") => // (2017-07-30) - (tag: v2.7.6)
          throw new YourGitInstallationIsToOldException("2.7", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.8.") => // (2016-06-06) - (tag: v2.8.4)
          throw new YourGitInstallationIsToOldException("2.8", "2017-07-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.9.") => // (2016-08-12) - (tag: v2.9.3)
          throw new YourGitInstallationIsToOldException("2.9", "2017-08-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.10.") => // (2016-10-28) - (tag: v2.10.2)
          throw new YourGitInstallationIsToOldException("2.10", "2017-11-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.11.") => // 2017-02-02) - (tag: v2.11.1)
          throw new YourGitInstallationIsToOldException("2.11", "2017-11-01", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.12.") => // (2017-03-20) - (tag: v2.12.1)
          throw new YourGitInstallationIsToOldException("2.12", "2018-04-02", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.13.") => // (2017-09-22) - (tag: v2.13.6)
          throw new YourGitInstallationIsToOldException("2.13", "2018-05-02", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.14.") => // (2017-10-23) - (tag: v2.14.3)
          throw new YourGitInstallationIsToOldException("2.13", "2018-07-02", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.15.") => // (2017-11-28) - (tag: v2.15.1)
          throw new YourGitInstallationIsToOldException("2.15", "2018-10-18", "n/a", " because of CVE-2018-17456", cmdLine)
        case v: String if v.startsWith("git version 2.16.") => // (2018-02-15) - (tag: v2.16.2)
          throw new YourGitInstallationIsToOldException("2.16", "2018-10-18", "n/a", " because of CVE-2018-17456", cmdLine)
        case v: String if v.startsWith("git version 2.17.") => // (2018-04-02) - (tag: v2.17.0)
          throw new YourGitInstallationIsToOldException("2.17", "2018-10-18", "n/a", " because of CVE-2018-17456", cmdLine)
        case v: String if v.startsWith("git version 2.18.") => // (2018-06-21) - (tag: v2.18.0)
          throw new YourGitInstallationIsToOldException("2.18", "2018-10-18", "n/a", " because of CVE-2018-17456", cmdLine)
        case v: String if v.startsWith("git version 2.19.0") => // (2018-09-10) - (tag: v2.19.0)
          throw new YourGitInstallationIsToOldException("2.19.0", "2018-10-18", "n/a", " because of CVE-2018-17456", cmdLine)
        case v: String if v.startsWith("git version 2.19.") => // (2018-09-27) - (tag: v2.19.1) -- fixes CVE-2018-17456
          throw new YourGitInstallationIsToOldException("2.19", "2019-09-03", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.20.") => // (2018-12-09) (tag: v2.20.0) -- no format change
          throw new YourGitInstallationIsToOldException("2.20", "2019-09-03", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.21.") => // (2019-02-24) (tag: v2.21.0) -- introduce output change
          throw new YourGitInstallationIsToOldException("2.21", "2020-03-07", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.22.") => // (2019-06-07) (tag: v2.22.0)
          throw new YourGitInstallationIsToOldException("2.22", "2020-03-07", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.23.") => // (2019-08-16) (tag: v2.23.0)
          throw new YourGitInstallationIsToOldException("2.23", "2020-03-07", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.24.") => // (2019-11-04) (tag: v2.24.0)
          throw new YourGitInstallationIsToOldException("2.24", "2020-03-07", "n/a", "", cmdLine)
        case v: String if v.startsWith("git version 2.25.") => // (2020-01-13) (tag: v2.25.0)
          throw new YourGitInstallationIsToOldException(version = "2.25", ended = "2020-09-11", announced = "2020-08-11",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.26.") => // (2020-04-19) (tag: v2.26.2)
          throw new YourGitInstallationIsToOldException(version = "2.26",
            ended = "2021-06-24", announced = "2021-02-11", announcedEnd = "2021-03-11",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.27.") => // (2020-05-31) (tag: v2.27.0)
          throw new YourGitInstallationIsToOldException(version = "2.27",
            ended = "2021-06-24", announced = "2021-02-11", announcedEnd = "2021-03-11",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.28.") => // (2020-07-26) (tag: v2.28.0)
          throw new YourGitInstallationIsToOldException(version = "2.28",
            ended = "2021-06-24", announced = "2021-02-11", announcedEnd = "2021-03-11",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.29.") => // (2020-10-19) (tag: v2.29.0)
          throw new YourGitInstallationIsToOldException(version = "2.29",
            ended = "2021-11-01", announced = "2021-06-24", announcedEnd = "2021-09-11",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.30.") => // (2020-12-27) (tag: v2.30.0)
          if (ReleaseConfig.isJenkinsK()) {
            List.tabulate(10)(_ =>
              err.println("W: please update your git version, \"" + v + "\" support endet at 2021-11-01")
            )
          } else {
            throw new YourGitInstallationIsToOldException(version = "2.30",
              ended = "2021-11-01", announced = "2021-06-24", announcedEnd = "2021-09-11",
              msg = "", gitPath = cmdLine)
          }
        case v: String if v.startsWith("git version 2.31.") => // do nothing (2021-03-15) (tag: v2.31.0)
          err.println("W: please update your git version, \"" + v + "\" support ends at 2022-02-01")
          throw new YourGitInstallationIsToOldException(version = "2.31",
            ended = "2022-02-22", announced = "2021-11-01", announcedEnd = "2022-02-01",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.32.") => // do nothing (2021-06-06) (tag: v2.32.0)
          throw new YourGitInstallationIsToOldException(version = "2.32",
            ended = "2023-01-03", announced = "2021-02-22", announcedEnd = "2022-04-01",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.33.") => // do nothing (2021-08-16) (tag: v2.33.0)
          throw new YourGitInstallationIsToOldException(version = "2.33",
            ended = "2023-01-03", announced = "2022-06-29", announcedEnd = "2022-08-01",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.34.") => // do nothing (2021-11-14) (tag: v2.34.0)
          if (false && ReleaseConfig.isDocker()) {
            // do nothing
          } else {
            throw new YourGitInstallationIsToOldException(version = "2.34",
              ended = "2023-01-03", announced = "2022-06-29", announcedEnd = "2022-08-01",
              msg = "", gitPath = cmdLine)
          }
        case v: String if v.startsWith("git version 2.35.") => // do nothing (2022-01-24) (tag: v2.35.0)
          throw new YourGitInstallationIsToOldException(version = "2.35",
            ended = "2023-01-03", announced = "2022-06-29", announcedEnd = "2022-08-01",
            msg = "", gitPath = cmdLine)
        case v: String if v.startsWith("git version 2.36.") =>
          // do nothing (2022-04-17) (tag: v2.36.0)
          err.println("W: please update your git version, \"" + v + "\" support ends at 2023-03-01")
        case v: String if v.startsWith("git version 2.37.") =>
          // do nothing (2022-07-04) (tag: v2.37.1)
          err.println("W: please update your git version, \"" + v + "\" support ends at 2023-09-01")
        case v: String if v.startsWith("git version 2.38.") =>
          // do nothing (2022-10-02) (tag: v2.38.0)
          if (ReleaseConfig.isDocker()) {
            List.tabulate(3)(_ =>
              err.println("W: please update your git version, \"" + v + "\" support ends at 2023-09-01")
            )
          } else {
            err.println("W: please update your git version, \"" + v + "\" support ends at 2023-09-01")
          }

        case v: String if v.startsWith("git version 2.39.") =>
        // do nothing (2022-12-13) (tag: v2.39.1)
        case v: String if v.startsWith("git version 2.40.") => // do nothing (2023-04-17) (tag: v2.40.1)
        case v: String if v.startsWith("git version 2.41.") => // do nothing (2023-06-01) (tag: v2.41.0)
        case v: String if v.startsWith("git version 2.42.") => // do nothing (2023-08-21) (tag: v2.42.0)
        case v: String if v.startsWith("git version 2.43.") => // do nothing (2023-11-20) (tag: v2.43.0)
        case v: String if v.startsWith("git version 2.44.") => // do nothing (2024-02-22) (tag: v2.44.0)
        case v: String => out.println("W: unknown/untested git version: \"" + v + "\". Please create a ticket at ISBO.");
        //  if (!ReleaseConfig.isTravisCi()) {
        //    if (Sgit.getOs == Os.Darwin) {
        // git fetch --tags && git lg --tags --date=short --simplify-by-decoration --pretty=format:'(%cd)%d'
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

  private[release] def fetchFilter(): String => Option[String] = {
    var foundError = false
    line =>
      line match {
        case l: String if l.matches("Total [0-9]+ \\(delta [0-9]+\\), reused [0-9]+ \\(delta [0-9]+\\)") => None
        case l: String if l.startsWith("ssh: ") => Some(l)
        case l: String if l.startsWith(" ! ") => Some(l)
        case l: String if l.startsWith("error: ") => Some(l)
        case l: String if l.startsWith("fatal: ") => {
          foundError = true
          Some(l)
        }
        case l: String if foundError => Some(l)
        case _ => None
      }
  }

  private[release] def gerritPushFilter(line: String): Option[String] = {
    line match {
      case l if l == "remote: " => None
      case l if l.startsWith("remote: Processing change") => None
      case l if l.startsWith("remote: error:") => None
      case l if l.startsWith("To ssh:") => None
      case l if l.trim == "remote: New Changes:" => None
      case l if l.trim == "remote: SUCCESS" => None
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

  def parseIsoDate(in: String): ZonedDateTime = {
    ZonedDateTime.parse(in)
  }

  def parseIsoDateOpt(in: String): Option[ZonedDateTime] = {
    try {
      Some(parseIsoDate(in))
    } catch {
      case e: Exception => None
    }
  }

  def parseTagLog(in: Seq[String]): Seq[GitTagWithDate] = {
    object TagLogParser extends RegexParsers with LazyLogging {
      override val skipWhitespace = false

      def doParse(s: String): Seq[GitTagWithDate] = {
        def parts = " (" ~> "[^\\)]+".r <~ ")" ^^ (term => term.split(", ").toSeq)

        def isoDate = "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[+-][0-9]{2}:[0-9]{2}".r ^^ (term => parseIsoDateOpt(term))

        val p = isoDate ~ parts

        parseAll(p, s) match {
          case Success(vp, v) => {
            val isoDate = vp._1

            val tagNames = vp._2
              .filter(_.startsWith("tag: "))
              .map(_.replaceFirst("^tag: ", ""))
            if (isoDate.isDefined) {
              tagNames.map(name => GitTagWithDate(name, isoDate.get))
            } else {
              Nil
            }
          }
          case f: Failure => {
            logger.warn("", new IllegalStateException("failure: " + f.toString() + " >" + in + "<"))
            Nil

          }
          case Error(msg, next) => {
            logger.warn("", new IllegalStateException("Syntax error - " + msg + " >" + in + "<"))
            Nil

          }
        }
      }
    }
    in.flatMap(line => {
      TagLogParser.doParse(line)
    })

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

  class YourGitInstallationIsToOldException(version: String, ended: String, announced: String, msg: String = "", gitPath: String, announcedEnd: String = "") extends
    RuntimeException(s"Your git version ${version} support ended at ${ended} announced at ${announced}${msg}; please update. " + gitPath)

  private[release] def findGit(start: File, checkExisting: Boolean): File = {
    findGitInner(start, start, checkExisting)
  }

  private def findGitInner(start: File, in: File, checkExisting: Boolean): File = {
    if (new File(in, ".git").exists()) {
      in
    } else if (checkExisting) {
      throw new MissingGitDirException("no .git dir in " + start.getAbsolutePath + " was found. " +
        "Please change dir to the project folder.")
    } else {
      in
    }
  }

  private[release] def doCloneRemote(src: String, dest: File, verify: Boolean = true, depth: Int = 0): Sgit = {
    val o = Sgit(dest, doVerify = verify, out = System.out, err = System.err, checkExisting = false, gitBin = None, opts = Opts())
    o.cloneRemote(src, dest, depth)
    o.copy(doVerify = false, checkExisting = true)
  }

  private[release] def doClone(src: File, dest: File, verify: Boolean = true): Sgit = {
    val o = Sgit(dest, doVerify = verify, out = System.out, err = System.err, checkExisting = false, gitBin = None, opts = Opts())
    o.clone(src, dest)
    o.copy(doVerify = false, checkExisting = true)
  }

  private[release] def init(f: File, verify: Boolean = true): Sgit = {
    val sgit = Sgit(f, doVerify = verify, out = System.out, err = System.err, checkExisting = false, gitBin = None, opts = Opts())
    sgit.init()
    sgit
  }
}
