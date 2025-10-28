package release

import com.google.common.base.Strings
import com.typesafe.scalalogging.LazyLogging
import release.Conf.Tracer
import release.Sgit.*
import release.Starter.PreconditionsException

import java.io.{File, PrintStream}
import java.net.URI
import java.time.ZonedDateTime
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, Executors, TimeUnit}
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.sys.process.ProcessLogger
import scala.util.{Failure, Success, Try}

trait SgitVersion {
  def version(): String
}

trait SgitDiff {
  def diffSafe(): Seq[String]
}

trait SgitUpstream {
  def findUpstreamBranch(): Option[String]
}

trait SgitDetached {
  def isNotDetached: Boolean = !isDetached

  def isDetached: Boolean
}

case class SlogLine(branchNames: Seq[String], tagNames: Seq[String], sha1: String, date: ZonedDateTime)

case class Sgit(file: File, doVerify: Boolean, out: PrintStream, err: PrintStream,
                checkExisting: Boolean = true, checkGitRoot: Boolean = true, findGitRoot: Boolean = false,
                gitBin: Option[String], opts: Opts) extends LazyLogging with SgitVersion with SgitDiff with SgitDetached
  with SgitUpstream {

  private val gitRoot: File = if (checkGitRoot) {
    if (findGitRoot) {
      Sgit.findGit(file.getAbsoluteFile)
    } else {
      Sgit.selectGit(file.getAbsoluteFile, checkExisting)
    }

  } else {
    null
  }

  private val dotGit = new File(gitRoot, ".git")

  def version(): String = {
    gitNative(Seq("--version"), useWorkdir = false).get.mkString("")
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
    Util.only(gitNative(Seq("rev-parse", "--is-shallow-repository")).get, "only one boolean expected").toBoolean
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

  private[release] def commitAll(message: String, fakeDate: Option[ZonedDateTime] = None): Unit = {
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
    val dateChanger: Seq[String] = fakeDate.map(d => Seq("--date", d.toString)).getOrElse(Nil)

    def doCommit(changeId: String): Seq[String] = {

      gitNative(Seq("commit", "--no-verify", "-m",
        ms.map(_.replaceFirst("^Change-Id:", "Change-Id: I" + changeId)).mkString("\n").trim) ++ dateChanger).get
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

  def currentBranch: String = Util.only(gitNative(Seq("rev-parse", "--abbrev-ref", "HEAD"), showErrorsOnStdErr = false).get, "only one branch expected")

  def currentBranchOpt: Option[String] = {
    try {
      Some(currentBranch)
    } catch {
      case _: Throwable => None
    }
  }

  def currentTagsAnnotated: Option[Seq[String]] = {
    currentTags.map(c => c.diff(currentTagsWithoutAnnotated.getOrElse(Nil)))
  }

  def currentTagsWithoutAnnotated: Option[Seq[String]] = {
    val strings = currentTags.getOrElse(Nil).map(tName => {
      try {
        val asdf = gitNative(Seq("cat-file", "-t", tName), showErrorsOnStdErr = false).get
        (tName, asdf)
      } catch {
        case _: Throwable => (tName, Nil)
      }
    }).flatMap(e => {
      if (e._2.contains("tag")) {
        None
      } else {
        Some(e._1)
      }
    })
    if (strings.isEmpty) {
      None
    } else {
      Some(strings)
    }

  }

  def currentTags: Option[Seq[String]] = {
    if (isDetached) {
      try {
        val value = gitNative(Seq("tag", "--points-at", "HEAD"), showErrorsOnStdErr = false).get
        if (value.isEmpty) {
          None
        } else {
          Some(value)
        }
      } catch {
        case _: Throwable => None
      }
    } else {
      None
    }
  }

  def lsFiles(): Seq[String] = gitNative(Seq("ls-files")).get.map(_.trim).map(in => if (in.startsWith("\"") && in.endsWith("\"")) {
    SgitParsers.unescape(in.substring(1).dropRight(1))
  } else {
    in
  })

  def lsFilesAbsolute(): Seq[File] = {
    lsFiles().map(entry => new File(gitRoot, entry).getAbsoluteFile)
  }

  def fetchAll(): Unit = {
    gitNative(Seq("fetch", "--all", "--tags"), errMapper = Sgit.fetchFilter()).get
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

  def lsRemote(): Seq[GitShaBranch] = {
    val nativeResult = gitNative(Seq("ls-remote"), showErrorsOnStdErr = false)
    nativeResult match {
      case Success(out) => {
        out.map(in => {
          try {
            val splitted = Sgit.splitLineOnWhitespace(in)
            GitShaBranch(commitId = splitted(0), branchName = splitted(1), dateSupplier = () => None)
          } catch {
            case e: Exception => throw new IllegalStateException("invalid line: \"" + in + "\"", e)
          }
        })
      }
      case _ => Nil
    }
  }

  def listRemotes(): Seq[GitRemote] = {
    val out = gitNative(Seq("remote", "-v")).get
    out.map(in => {
      try {
        val splitted = Sgit.splitLineOnWhitespace(in)
        GitRemote.of(splitted(0), splitted(1), splitted(2))
      } catch {
        case e: Exception => throw new IllegalStateException("invalid line: \"" + in + "\"", e)
      }
    })
  }

  def listBranchNamesLocal(): Seq[String] = {
    listBranchesLocal().map(_.branchName.replaceFirst("refs/heads/", "")).sorted
  }

  def listBranchesLocal(): Seq[GitShaBranch] = {
    gitNative(Seq("branch", "--list", "--verbose", "--no-abbrev")).get
      .flatMap(Sgit.splitLineOnBranchlistErr(err))
      .flatMap(in => if (in._1 == in._2) {
        None
      } else {
        Some(in)
      })
      .map(parts => {
        GitShaBranch(parts._2, "refs/heads/" + parts._1, () => Some(ZonedDateTime.now()))
      }).toList
  }

  def listBranchNamesRemoteShort(): Seq[String] = {
    listBranchNamesRemote().map(_.replaceFirst("origin/", "")).sorted
  }

  def remoteHead(timeout: Duration = Duration(10, TimeUnit.SECONDS), debugFn: () => Unit = () => ()): Try[Option[String]] = {
    val executor = Executors.newSingleThreadExecutor() // TODO reuse executor later
    val call = new Callable[Try[Seq[String]]] {
      override def call(): Try[Seq[String]] = {
        try {
          debugFn.apply()
          val value = gitNative(Seq("remote", "show", "origin"), showErrorsOnStdErr = false)
            .flatMap(in => Success(in.filter(_.startsWith("  HEAD branch")).map(_.trim)))
          value
        } catch {
          case t: Throwable => {
            Failure(t)
          }
        }
      }
    }
    val future = executor.submit(call)
    val result: Try[Seq[String]] = try {
      future.get(timeout.length, timeout.unit)
    } catch {
      case te: java.util.concurrent.TimeoutException => {
        future.cancel(true)
        return Failure(new java.util.concurrent.TimeoutException(
          (Strings.nullToEmpty(te.getMessage) + " after " + timeout.toString + " to " + listRemotes().map(_.position).map(Util.stripUserinfo).distinct.mkString(", ")).trim)
        )
      }
    } finally {
      executor.shutdownNow();
    }
    result.map(_.headOption)
  }

  def remoteHeadRaw(): Try[Option[String]] = {
    Sgit.toRawRemoteHead(remoteHead())
  }

  private[release] def listContributorMailboxes(): Seq[String] = {
    val value = "--since=14.days"
    val anCn = gitNative(Seq("log", "--all", "--pretty=%an <%ae>᎒%cn <%ce>", value)).get

    anCn.flatMap(l => l.split('᎒').toSeq).distinct.sorted
  }

  def listBranchNamesRemote(): Seq[String] = listBranchRemoteRaw().map(_.branchName).sorted

  def listBranchNamesAll(): Seq[String] = (listBranchNamesRemoteShort() ++ listBranchNamesLocal()).distinct.sorted

  def listBranchNamesAllFull(): Seq[String] = (listBranchNamesRemote() ++ listBranchNamesLocal()).distinct.sorted

  def listBranchRemoteRefRemotes(): Seq[GitShaBranch] = {
    listBranchRemoteRaw().map(in => in.copy(branchName = "refs/remotes/" + in.branchName))
  }

  private[release] def listBranchRemoteRaw(): Seq[GitShaBranch] = {
    val value = gitNative(Seq("log", "--branches", "--remotes", "--simplify-by-decoration", "--pretty=format:'%d %H %cI'")).get
    val slogs: Seq[SlogLine] = value.flatMap(line => SgitParsers.LogParser.doParse(line, value))
    val branchT = slogs.flatMap(s => s.branchNames.map((_, s.sha1, s.date)))

    if (branchT.nonEmpty) {
      branchT.flatMap(in => in match {
        case (name: String, _: String, _: ZonedDateTime) if name.startsWith("origin/HEAD") => None
        case (name: String, sha1: String, date: ZonedDateTime) if name.startsWith("origin/") => {
          val remoteName = name.replaceFirst("^\\*", "").trim
          Some(GitShaBranch(sha1, remoteName, () => Some(date)))
        }
        case _ => None
      }).toList
    } else {
      Nil
    }
  }

  def listRefNames(): Seq[String] = listRefs().map(_.refName)

  case class CdAd(commitDate: ZonedDateTime, authorDate: ZonedDateTime)

  def listRefs(fn: CdAd => ZonedDateTime = e => e.authorDate): Seq[GitShaRefTime] = {
    val value = gitNativeOpt(Seq("show-ref")).getOrElse(Nil)
    if (value.nonEmpty) {
      val temp = value
        .map(in => {
          val parts = in.replaceFirst("^\\*", "").trim.split("[ \t]+")
          (parts(0), parts(1))
        }).sortBy(_._2).toList
        .map(e => {
          if (e._2.startsWith("refs/heads/")) {
            val nR = gitNative(Seq("merge-base", "HEAD", e._2))
            e.copy(_1 = nR.getOrElse(Nil).headOption.getOrElse(e._1))
          } else {
            e
          }
        })
      val keyRefs = temp.map(_._1)
      val step = 100
      val nD: Try[Seq[String]] = keyRefs.sliding(step, step).map(r =>
        gitNative(Seq("show", "-s", "--format=%H|%cd|%ad", "--date=iso-strict") ++ r)).foldLeft(Try(Seq.empty[String]))((a, b) => {
        if (b.isSuccess) {
          Success(a.get ++ b.get)
        } else {
          b
        }
      })

      val shaToDates: Map[String, CdAd] = nD.getOrElse(Nil)
        .filterNot(_.isBlank)
        .flatMap(line => {
          val oar = line.split("\\|").toList
          if (oar.size < 2) {
            None
          } else {
            try {
              Some((oar.head, CdAd(SgitParsers.parseIsoDate(oar(1)), SgitParsers.parseIsoDate(oar(2)))))
            } catch {
              case e: Exception => {
                None
              }
            }
          }
        }).toMap
      temp.map(t => {
        val time: () => Option[ZonedDateTime] = () => {
          shaToDates.get(t._1).map(fn.apply)
        }
        GitShaBranch(commitId = t._1, branchName = t._2, time)
      })
    } else {
      Nil
    }

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
    Util.only(gitNative(Seq("log", "-n1", "--pretty=%H", ref)).get, "only one commitid expected")
  }

  def commitIds(fromRef: String, toRef: String): Seq[String] = {
    gitNative(Seq("log", "--pretty=%H", fromRef + "..." + toRef)).get
  }

  def hasChangesToPush: Boolean = {
    val status = gitNative(Seq("status", "--branch", "--porcelain", currentBranch)).get.mkString(" ")
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
    gitNative(Seq("push", "--tags", "--dry-run", "--porcelain")).get
  }

  def isDetached: Boolean = {
    val status = gitNative(Seq("status", "--branch", "--porcelain")).get.mkString(" ")
    status.startsWith("## HEAD (no branch)")
  }

  def localChanges(): Seq[String] = {
    val status = gitNative(Seq("status", "--porcelain")).get
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
    gitNative(Seq("diff") ++ opts).get
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
      Success(gitNative(value, showErrorsOnStdErr = false).get.map(_.trim))
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
        "-3", "--oneline", "--decorate", "--")).get
        .map(_.trim).mkString("\n")
    } catch {
      case e: Exception => e.printStackTrace(); "no - graph"
    }
  }

  def listAllTags(): Seq[String] = gitNative(Seq("tag", "--list")).get

  def listTagsWithDate(): Seq[GitTagWithDate] = {
    Sgit.parseTagLog(gitNative(Seq("log", "--tags", "--simplify-by-decoration", "--pretty=%aI%d")).get)
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
    val allTags = listAllTags()
    if (allTags.contains(deleteTag)) {
      gitNative(Seq("tag", "-d", deleteTag))
    } else {
      throw new IllegalStateException("tag " + deleteTag + " could not found")
    }
  }

  def doTag(tagName: String, msg: String = ""): Unit = {
    val newTag = checkedTagName(tagName)
    val allTags = listAllTags()
    if (allTags.contains(newTag)) {
      throw new IllegalStateException("tag " + newTag + " already exists")
    }
    // TODO if % git config --local tag.gpgsign true
    // TODO only annotated tags are allowed % git tag -a v1.4 -m "my version 1.4"
    val cmd = if (!msg.isBlank) {
      Seq("tag", "-a", newTag, "-m", msg)
    } else {
      Seq("tag", newTag)
    }
    gitNative(cmd).get
  }

  def deleteBranch(branchName: String): Unit = {
    if (listBranchNamesLocal().contains(branchName)) {
      gitNative(Seq("branch", "-D", branchName))
    } else {
      throw new IllegalStateException("branch '" + branchName + "' not found.")
    }
  }

  def checkout(branchOrTag: String): Unit = {
    val branch = currentBranch
    if (branch != branchOrTag) {
      val result = gitNative(Seq("checkout", "-q", branchOrTag))
      result.get
    }
  }

  def createBranch(branchName: String): Unit = {
    try {
      gitNative(Seq("branch", branchName)).get
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
    gitNative(Seq("push", "-q", "-u", "origin", src), errMapper = Sgit.gerritPushFilter).get
  }

  private[release] def revertHead(): Seq[String] = {
    gitNative(Seq("revert", "--no-edit", "HEAD")).get
  }

  private[release] def commitMessageBody(commitId: String): Seq[String] = {
    gitNative(Seq("log", "-n1", "--pretty=%b", commitId)).get
  }

  private[release] def resetHard(commitId: String): Seq[String] = {
    gitNative(Seq("reset", "--hard", commitId)).get
  }

  private[release] def gitNativeOpt(args: Seq[String]): Option[Seq[String]] = {
    try {
      Some(gitNative(args, showErrorsOnStdErr = false).get)
    } catch {
      case t: Throwable => None
    }
  }

  private[release] def gitNative(args: Seq[String], showErrorsOnStdErr: Boolean = true, useWorkdir: Boolean = true,
                                 cmdFilter: String => Boolean = _ => false,
                                 errMapper: String => Option[String] = errLine => Some(errLine)): Try[Seq[String]] = {
    if (checkExisting && !gitRoot.isDirectory) {
      throw new IllegalStateException("invalid git dir: " + gitRoot.getAbsolutePath)
    }
    val workdir = if (useWorkdir) {
      Seq("-C", gitRoot.getAbsolutePath)
    } else {
      Nil
    }
    val cmd: Seq[String] = checkVersionGitSmd.updateAndGet {
      case None => Some(selectedGitCmd(err, gitBin).get)
      case s => s
    }.get
    val gitCmdCall: Seq[String] = cmd ++ workdir ++ Seq("--no-pager") ++ args

    val ff = Tracer.msgAround[Try[String]](gitCmdCall.mkString(" "), logger,
      () => Sgit.native(gitCmdCall, errOnStdout = showErrorsOnStdErr, cmdFilter, err, errMapper))
    ff.map(_.linesIterator.toList.dropWhile(in => in.trim.isEmpty))
  }
}

object Sgit {

  lazy val selfFileChecksum: String = {
    val hash = try {
      val codeSource = Sgit.getClass.getProtectionDomain.getCodeSource
      val path = codeSource.getLocation.getPath
      Util.hashMurmur3_32_fixed(path)
    } catch {
      case _: Exception => "N/A"
    }
    val gitHEAD = try {
      FileUtils.read(new File("/root/git.HEAD")).trim
    } catch {
      case _: Exception => "NA"
    }
    hash + "@" + gitHEAD
  }

  def stripVersionPrefix(in: Seq[String]): Seq[String] = in.map(_.replaceFirst("^v(.*)", "$1"))

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
      Some((result.head, result(1)))
    } else {
      err.println("W: Unknown branch definition (check commit messages for second line empty, first line char limit):" +
        " \"" + in + "\". See: git branch --list --verbose --no-abbrev")
      None
    }
  }

  sealed trait GitShaRefTime {
    val sha1: String
    val refName: String
    lazy val dateOpt: Option[ZonedDateTime] = None
    lazy val date: ZonedDateTime = dateOpt.get
  }

  sealed case class GitShaTag(sha1: String, tagName: String, dateSupplier: () => Option[ZonedDateTime]) extends GitShaRefTime {
    override lazy val dateOpt: Option[ZonedDateTime] = dateSupplier.apply()
    val refName = tagName
  }

  sealed case class GitShaBranch(commitId: String, branchName: String, dateSupplier: () => Option[ZonedDateTime]) extends GitShaRefTime {
    override lazy val dateOpt: Option[ZonedDateTime] = dateSupplier.apply()
    if (commitId.length != 40) {
      throw new IllegalStateException("invalid commit id length: " + commitId.length + " (" + commitId + ")")
    }

    if (!GitShaBranch.matchesGitSha(commitId)) {
      throw new IllegalStateException("invalid commit id: " + commitId + "")
    }

    val refName: String = branchName
    val sha1: String = commitId
  }

  object GitShaBranch {
    def matchesGitSha(in: String): Boolean = in.matches("[0-9a-f]{40}")
  }

  object GitRemote {
    def of(name: String, position: String, remoteType: String): GitRemote = {
      GitRemoteImp(name, Util.stripUserinfo(position), remoteType)
    }
  }

  trait GitRemote {
    val name: String
    val position: String
    val remoteType: String
  }

  private case class GitRemoteImp(name: String, position: String, remoteType: String) extends GitRemote {
    override def toString: String = s"$name  $position $remoteType"
  }

  case class GitTagWithDate(name: String, date: ZonedDateTime)

  private var gits = Map.empty[Seq[String], Unit]
  private val checkVersionGitSmd = new AtomicReference[Option[Seq[String]]](None)

  private[release] def checkVersion(sgitVersion: SgitVersion, out: PrintStream, err: PrintStream, gitBin: Option[String]): Unit = synchronized {
    val cmd: Seq[String] = checkVersionGitSmd.updateAndGet {
      case None => Some(selectedGitCmd(err, gitBin).get)
      case s => s
    }.get
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
        case v: String if v.startsWith("git version 2.45.") => // do nothing (2024-05-30) (tag: v2.45.2)
        case v: String if v.startsWith("git version 2.46.") => // do nothing (2024-11-26) (tag: v2.46.3)
        case v: String if v.startsWith("git version 2.47.") => // do nothing (2024-11-26) (tag: v2.47.2)
        case v: String if v.startsWith("git version 2.48.") => // do nothing (2025-01-13) (tag: v2.48.1)
        case v: String if v.startsWith("git version 2.49.") => // do nothing (2025-03-14) (tag: v2.49.0)
        case v: String if v.startsWith("git version 2.50.") => // do nothing (2025-06-15) (tag: v2.50.0)
        case v: String => out.println("W: unknown/untested git version: \"" + v + "\". Please create a ticket at ISBO.");
        //  if (!ReleaseConfig.isTravisCi()) {
        //    if (Sgit.getOs == Os.Darwin) {
        // git fetch --tags && git lg --tags --date=short --simplify-by-decoration --pretty=format:'(%cd)%d'
      }
      gits = gits ++ Map(cmd -> result)
    }
  }

  private[release] def outLogger(errOut: String => Unit, outLog: String => Unit,
                                 errorLineMapper: String => Option[String]): ProcessLogger = {
    new ProcessLogger {
      override def out(s: => String): Unit = {
        outLog.apply(s)
      }

      override def err(s: => String): Unit = {
        if (s.nonEmpty) {
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

  private[release] def native(cmd: Seq[String], errOnStdout: Boolean,
                              cmdFilter: String => Boolean, err: PrintStream,
                              errLineMapper: String => Option[String]): Try[String] = {
    import sys.process._

    var errors: String = ""
    var stdout = ""

    def logError(in: String): Unit = {
      if (errOnStdout) {
        err.println(in)
      }
      errors = errors.concat(" ").concat(in).trim
    }

    def logOut(in: String): Unit = {
      stdout = stdout.concat(" ").concat(in).trim
    }

    try {
      val result: String = cmd !! outLogger(logError, logOut, errLineMapper)
      Success(result.trim)
    } catch {
      case e: RuntimeException if e.getMessage != null &&
        e.getMessage.startsWith("Nonzero exit value:") && cmd.head.contains("git") =>
        val msg = e.getMessage + "; git " + cmd.drop(3).mkString(" ") +
          "; " + Seq(errors, stdout).filterNot(_.isEmpty).mkString("; ")
        if (e.getMessage.startsWith("Nonzero exit value: 130")) {
          Failure(new TerminatedByCtrlCException(msg.trim))
        } else {
          Failure(new RuntimeException(msg.trim))
        }
      case e: Throwable => {
        Failure(e)
      }
    }

  }

  private[release] def selectedGitCmd(err: PrintStream, gitBin: Option[String]): Try[Seq[String]] = {
    if (gitBin.isDefined) {
      Success(gitBin.toSeq)
    } else {
      val gitCygCmd: Try[String] = try {
        native(Seq("cygpath", "-daw", "/usr/bin/git"), errOnStdout = false, _ => false, err, s => Some(s))
      } catch {
        case e: Exception => Failure(e)
      }
      val gitCmd: Seq[String] = if (gitCygCmd.isSuccess) {
        Seq(gitCygCmd.get)
      } else {
        val winGit = new File(new URI("file:///C:/Programme/Git/bin/git.exe"))
        if (winGit.canExecute) {
          Seq(winGit.getAbsolutePath)
        } else {
          Seq("git")
        }
      }
      Success(gitCmd)
    }
  }

  def parseTagLog(in: Seq[String]): Seq[GitTagWithDate] = {
    in.flatMap(line => SgitParsers.TagLogParser.doParse(line, in))
  }

  class MissingCommitHookException(msg: String) extends RuntimeException(msg)

  class MissingGitDirException(msg: String) extends RuntimeException(msg)

  class TerminatedByCtrlCException(msg: String) extends RuntimeException(msg)

  class BranchAlreadyExistsException(msg: String) extends RuntimeException(msg)

  class YourGitInstallationIsToOldException(version: String, ended: String, announced: String, msg: String = "", gitPath: String, announcedEnd: String = "") extends
    RuntimeException(s"Your git version ${version} support ended at ${ended} announced at ${announced}${msg}; please update. " + gitPath)


  private[release] def findGit(start: File): File = {
    findGitInner(start, start)
  }

  @tailrec
  private[release] def findGitInner(start: File, current: File): File = {
    if (current == null) {
      throw new MissingGitDirException("no .git dir in " + start.getAbsolutePath + " or in parents was found. " +
        "Please change dir to the project folder.")
    } else if (new File(current, ".git").exists()) {
      current
    } else {
      findGitInner(start, current.getParentFile)
    }
  }

  private[release] def selectGit(start: File, checkExisting: Boolean): File = {
    selectGitInner(start, start, checkExisting)
  }

  private def selectGitInner(start: File, in: File, checkExisting: Boolean): File = {
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
