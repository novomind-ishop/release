package release

import java.io.File
import java.nio.file.Files
import java.util.regex.Pattern
import scala.util.control.NonFatal

case class BackportOpts(commit: Option[String] = None, apply: Boolean = false, fetch: Boolean = false,
    remote: String = "origin", targets: Seq[String] = Nil, branchPrefix: String = "feature",
    descriptionFile: Option[String] = None, showHelp: Boolean = false, invalids: Seq[String] = Nil)

object Backport {
  private case class Plan(target: String, base: String, branch: String, included: Boolean)

  private[release] def formatDescription(description: String): String = {
    val trailer = "^([A-Za-z0-9][A-Za-z0-9-]*)[ \\t]*:[ \\t]*(.*)$".r
    val body = scala.collection.mutable.ArrayBuffer.empty[String]
    val trailers = scala.collection.mutable.ArrayBuffer.empty[String]
    var continuation = false
    var fence: Option[(Char, Int)] = None
    description.replace("\r\n", "\n").replace('\r', '\n').linesIterator.foreach { line =>
      val trimmed = line.trim
      val fenceMarker = trimmed.takeWhile(c => c == '`' || c == '~')
      if (fenceMarker.length >= 3 && fenceMarker.forall(_ == fenceMarker.head)) {
        fence match {
          case None => fence = Some(fenceMarker.head -> fenceMarker.length)
          case Some((char, length)) if fenceMarker.head == char && fenceMarker.length >= length && trimmed == fenceMarker =>
            fence = None
          case _ =>
        }
        body += line
        continuation = false
      } else if (fence.isDefined) {
        body += line
        continuation = false
      } else line match {
        case trailer(key, value) if value.trim.nonEmpty || key.contains("-") =>
          trailers += s"$key: ${value.trim}"
          continuation = true
        case value if continuation && value.nonEmpty && value.head.isWhitespace && trimmed.nonEmpty =>
          trailers(trailers.size - 1) += "\n" + line
        case _ =>
          if (trimmed.nonEmpty || body.lastOption.exists(_.trim.nonEmpty)) body += line
          continuation = false
      }
    }
    val text = body.dropWhile(_.trim.isEmpty).reverse.dropWhile(_.trim.isEmpty).reverse.mkString("\n")
    Seq(text, trailers.mkString("\n")).filter(_.nonEmpty).mkString("\n\n")
  }

  // fnmatch-style globs: '*' also matches '/', unlike filesystem glob matchers.
  private[release] def matches(value: String, glob: String): Boolean = {
    val regex = new StringBuilder
    var i = 0
    while (i < glob.length) {
      glob.charAt(i) match {
        case '*' => regex.append(".*")
        case '?' => regex.append('.')
        case '[' =>
          var end = i + 1
          if (end < glob.length && glob.charAt(end) == '!') end += 1
          if (end < glob.length && glob.charAt(end) == ']') end += 1
          while (end < glob.length && glob.charAt(end) != ']') end += 1
          if (end == glob.length) regex.append("\\[")
          else {
            val content = glob.substring(i + 1, end)
            val negated = content.startsWith("!")
            val chars = (if (negated) content.drop(1) else content)
              .replace("\\", "\\\\").replace("&", "\\&").replace("[", "\\[").replace("]", "\\]").replace("^", "\\^")
            regex.append('[').append(if (negated) "^" else "").append(chars).append(']')
            i = end
          }
        case c => regex.append(Pattern.quote(c.toString))
      }
      i += 1
    }
    Pattern.compile(regex.toString, Pattern.DOTALL).matcher(value).matches()
  }

  def run(directory: File, sys: Term.Sys, gitBin: Option[String], opts: Opts): Int = {
    val bp = opts.backport.get
    if (bp.showHelp || bp.commit.isEmpty || bp.invalids.nonEmpty) {
      if (bp.invalids.nonEmpty) sys.err.println("Invalid options: " + bp.invalids.mkString(", "))
      sys.out.println("Usage: release backport <commit> [OPTION]")
      sys.out.println("Prepare local backport branches and print GitLab MR push commands.")
      sys.out.println("--apply                 Create missing local branches (default: preview)")
      sys.out.println("--fetch                 Fetch the selected remote with --prune first")
      sys.out.println("--remote name           Remote (default: origin)")
      sys.out.println("--target branch-or-glob Repeatable (default: main, qa/*, release/*)")
      sys.out.println("--branch-prefix prefix  Local branch prefix (default: feature)")
      sys.out.println("--description-file path Read MR description instead of commit message")
      sys.out.println("--help, -h              Show this help")
      return if (bp.showHelp && bp.invalids.isEmpty) 0 else 1
    }
    try {
      val git = Sgit(directory, doVerify = false, out = sys.out, err = sys.err,
        findGitRoot = true, gitBin = gitBin, opts = opts)
      def warnWhitespace(plan: Plan, to: String, checkout: Option[File] = None): Unit = {
        git.diffWhitespaceWarnings(plan.base, to, checkout).foreach { warning =>
          sys.err.println(s"WARNING ${plan.target}: $warning")
        }
      }
      def existing(plan: Plan, commit: String): Boolean = {
        val ref = "refs/heads/" + plan.branch
        if (!git.branchExists(plan.branch)) false
        else {
          git.listWorktrees().filter(_.branch.contains(ref)).foreach { worktree =>
            val checkout = worktree.directory
            if (git.hasPendingCherryPick(checkout) || git.hasWorktreeChanges(checkout))
              throw new IllegalStateException(s"${plan.branch}: Worktree has unresolved conflicts or uncommitted changes: $checkout")
          }
          val marker = s"(cherry picked from commit $commit)"
          if (git.commitParents(plan.branch) != Seq(plan.base) || !git.commitMessage(plan.branch).contains(marker))
            throw new IllegalStateException(
              s"${plan.branch} already exists with a different base or commit; review the branch or choose a different --branch-prefix.")
          warnWhitespace(plan, plan.branch)
          true
        }
      }
      def create(plan: Plan, commit: String): Boolean = {
        val checkout = Files.createTempDirectory("git-backport-").toFile
        try git.worktreeAddBranch(checkout, plan.branch, plan.base)
        catch {
          case NonFatal(error) =>
            if (Option(checkout.list()).exists(_.isEmpty)) Files.delete(checkout.toPath)
            throw error
        }
        val pick = git.cherryPickWithSource(commit, checkout)
        if (pick.exitCode != 0) {
          sys.err.println(s"ERROR ${plan.branch}: ${pick.stderr.trim}\n${pick.stdout.trim}")
          sys.err.println(s"Worktree retained: $checkout")
          sys.err.println("After resolving the conflicts manually:")
          git.conflictResolutionCommands(checkout).foreach(sys.err.println)
          sys.err.println("Then rerun this tool. Review any differences in the backported changes.")
          false
        } else {
          try warnWhitespace(plan, "HEAD", Some(checkout))
          catch { case NonFatal(error) => sys.err.println(s"Worktree retained: $checkout"); throw error }
          git.worktreeRemoveCheckout(checkout)
          true
        }
      }

      val commit = git.resolveCommit(bp.commit.get)
      if (git.commitParents(commit).size != 1)
        throw new IllegalStateException("Specify a single commit with exactly one parent; merge and root commits are not supported.")
      if (!git.remoteNames().contains(bp.remote))
        throw new IllegalStateException("Unknown remote: " + bp.remote)
      if (bp.fetch) git.fetchRemote(bp.remote)
      val patterns = if (bp.targets.isEmpty) Seq("main", "qa/*", "release/*") else bp.targets
      val prefix = s"refs/remotes/${bp.remote}/"
      val targets = git.remoteBranchNames(bp.remote)
        .filter(target => patterns.exists(matches(target, _))).toSeq
      if (targets.isEmpty) throw new IllegalStateException("No matching remote branches found.")
      patterns.foreach { pattern =>
        if (!targets.exists(matches(_, pattern))) throw new IllegalStateException(s"No remote branches match '$pattern'.")
      }
      val message = git.commitMessage(commit)
      val subject = git.commitSubject(commit)
      val task = "(?m)^Task:\\s*([\\w.-]+)".r.findFirstMatchIn(message).map(_.group(1))
      val label = task.map(_ + "-").getOrElse("")
      val description = formatDescription(bp.descriptionFile.map(path => Files.readString(directory.toPath.resolve(path)))
          .getOrElse(git.commitMessageBody(commit).mkString("\n")))
      val plans = targets.map { target =>
        val base = git.resolveCommit(prefix + target)
        val branch = s"${bp.branchPrefix}/$label${commit.take(10)}-${target.replace('/', '-')}"
        git.validateBranchName(branch)
        Plan(target, base, branch, git.containsCommit(base, commit))
      }
      if (plans.map(_.branch).distinct.size != plans.size)
        throw new IllegalStateException("Target branch names produce duplicate merge request branch names.")
      sys.out.println("Remote state: local references" + (if (bp.fetch) " (fetched before planning)" else " (not fetched)"))
      var failed = false
      plans.foreach { plan =>
        if (plan.included) sys.out.println(s"INCLUDED ${plan.target}")
        else try {
          val exists = existing(plan, commit)
          val ready = if (bp.apply && !exists) create(plan, commit) else exists
          if (bp.apply && !ready) failed = true
          else {
            sys.out.println(s"${if (ready) "READY" else "MISSING"} ${plan.target} -> ${plan.branch}")
            if (ready) {
              sys.out.println(git.gitlabMergeRequestPushCommand(bp.remote, plan.branch, plan.target, subject, description))
            }
          }
        } catch {
          case NonFatal(error) => sys.err.println(s"ERROR ${plan.target}: ${error.getMessage}"); failed = true
        }
      }
      if (!bp.apply) sys.out.println("Rerun with --apply to create missing branches.")
      if (failed) 1 else 0
    } catch {
      case NonFatal(error) => sys.err.println("ERROR: " + error.getMessage); 1
    }
  }
}
