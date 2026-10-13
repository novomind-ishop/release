package release

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path}
import org.junit.{Assert, Test}
import scala.jdk.CollectionConverters.*
import scala.sys.process.*

class BackportTest {
  private def git(repo: Path, args: String*): String =
    Process(Seq("git", "-C", repo.toString) ++ args).!!.trim

  private def withRepo(test: Path => Unit): Unit = {
    val repo = Files.createTempDirectory("backport-test-")
    try {
      git(repo, "init", "--initial-branch=source")
      git(repo, "config", "user.name", "Backport Test")
      git(repo, "config", "user.email", "backport@example.invalid")
      git(repo, "config", "commit.gpgsign", "false")
      git(repo, "config", "core.autocrlf", "false")
      Files.writeString(repo.resolve("file.txt"), "base\n")
      git(repo, "add", ".")
      git(repo, "commit", "-m", "Base")
      git(repo, "remote", "add", "origin", repo.toString)
      Seq("main", "qa/1x", "release/1x").foreach { name =>
        git(repo, "update-ref", "refs/remotes/origin/" + name, "HEAD")
      }
      Files.writeString(repo.resolve("file.txt"), "fixed\n")
      git(repo, "commit", "-am", "Fix customer's bug\n\nTask: ABC-123")
      test(repo)
    } finally {
      // Worktrees left by conflict tests are outside the repository.
      git(repo, "worktree", "list", "--porcelain").split("\n\n").foreach { record =>
        record.linesIterator.find(_.startsWith("worktree ")).foreach { line =>
          val path = Path.of(line.stripPrefix("worktree "))
          if (path != repo) git(repo, "worktree", "remove", "--force", path.toString)
        }
      }
      val paths = Files.walk(repo)
      try paths.iterator().asScala.toSeq.sortBy(_.getNameCount).reverse.foreach(Files.delete(_))
      finally paths.close()
    }
  }

  private def run(repo: Path, bp: BackportOpts): (Int, String) = {
    val output = new ByteArrayOutputStream()
    val sys = new Term.Sys(new ByteArrayInputStream(Array.emptyByteArray), output, output)
    val code = Backport.run(repo.toFile, sys, Some("git"), Opts(backport = Some(bp)))
    (code, output.toString("UTF-8"))
  }

  @Test def previewApplyAndRepeatPreserveCheckout(): Unit = withRepo { repo =>
    Files.writeString(repo.resolve("untracked.txt"), "local work")
    val before = git(repo, "status", "--porcelain")
    val commit = git(repo, "rev-parse", "HEAD")
    val preview = run(repo, BackportOpts(commit = Some(commit)))
    Assert.assertEquals(preview._2, 0, preview._1)
    Assert.assertTrue(preview._2.contains("MISSING main"))
    Assert.assertFalse(preview._2.contains("git push"))
    Assert.assertEquals("source", git(repo, "branch", "--format=%(refname:short)"))
    val applied = run(repo, BackportOpts(commit = Some(commit), apply = true))
    Assert.assertEquals(applied._2, 0, applied._1)
    Assert.assertTrue(applied._2.contains("merge_request.create"))
    Assert.assertTrue(applied._2.contains("ABC-123"))
    val pushCommands = applied._2.linesIterator.filter(_.startsWith("git push ")).toSeq
    Assert.assertEquals(3, pushCommands.size)
    pushCommands.foreach { command =>
      val title = Process(Seq("sh", "-c", "set -- " + command + "; printf '%s' \"${11}\"")).!!.trim
      val description = Process(Seq("sh", "-c", "set -- " + command + "; printf '%s' \"${13}\"")).!!.trim
      Assert.assertEquals("merge_request.title=Fix customer's bug", title)
      Assert.assertEquals("merge_request.description=Task: ABC-123", description)
    }
    val branch = s"feature/ABC-123-${commit.take(10)}-main"
    Assert.assertEquals(git(repo, "rev-parse", "HEAD^"), git(repo, "rev-parse", branch + "^"))
    Assert.assertTrue(git(repo, "show", "-s", "--format=%B", branch).contains(s"(cherry picked from commit $commit)"))
    Assert.assertEquals("source", git(repo, "branch", "--show-current"))
    Assert.assertEquals(before, git(repo, "status", "--porcelain"))
    val repeated = run(repo, BackportOpts(commit = Some(commit), apply = true))
    Assert.assertEquals(repeated._2, 0, repeated._1)
    Assert.assertEquals(applied._2, repeated._2)
  }

  @Test def whitespaceWarningsDoNotLeaveSuccessfulWorktrees(): Unit = withRepo { repo =>
    Files.writeString(repo.resolve(".gitlab-ci.yml"), "job:\n  script: echo test\n\n")
    git(repo, "add", ".gitlab-ci.yml")
    git(repo, "commit", "--amend", "--no-edit")
    val commit = git(repo, "rev-parse", "HEAD")
    val bp = BackportOpts(commit = Some(commit), apply = true, targets = Seq("main"))
    val applied = run(repo, bp)
    Assert.assertEquals(applied._2, 0, applied._1)
    Assert.assertTrue(applied._2.contains("WARNING main: .gitlab-ci.yml:3: new blank line at EOF."))
    Assert.assertTrue(applied._2.contains("READY main"))
    Assert.assertFalse(applied._2.contains("Worktree retained"))
    Assert.assertEquals(1, git(repo, "worktree", "list", "--porcelain").linesIterator.count(_.startsWith("worktree ")))
    Assert.assertEquals(0, run(repo, bp)._1)
  }

  @Test def detectsOriginalAndCherryPickedCopies(): Unit = withRepo { repo =>
    val commit = git(repo, "rev-parse", "HEAD")
    git(repo, "update-ref", "refs/remotes/origin/main", commit)
    git(repo, "checkout", "-b", "copy", "HEAD^")
    git(repo, "cherry-pick", "-x", commit)
    git(repo, "update-ref", "refs/remotes/origin/qa/1x", "HEAD")
    val result = run(repo, BackportOpts(commit = Some(commit)))
    Assert.assertEquals(result._2, 0, result._1)
    Assert.assertTrue(result._2.contains("INCLUDED main"))
    Assert.assertTrue(result._2.contains("INCLUDED qa/1x"))
    Assert.assertTrue(result._2.contains("MISSING release/1x"))
  }

  @Test def rejectsWrongExistingBranchAndMovedBase(): Unit = withRepo { repo =>
    val commit = git(repo, "rev-parse", "HEAD")
    val bp = BackportOpts(commit = Some(commit), apply = true, targets = Seq("main"))
    val branch = s"feature/ABC-123-${commit.take(10)}-main"
    git(repo, "branch", branch, "HEAD^")
    Assert.assertEquals(1, run(repo, bp)._1)
    git(repo, "branch", "-D", branch)
    Assert.assertEquals(0, run(repo, bp)._1)
    git(repo, "checkout", "-b", "new-base", "HEAD^")
    git(repo, "commit", "--allow-empty", "-m", "New base")
    git(repo, "update-ref", "refs/remotes/origin/main", "HEAD")
    val result = run(repo, bp)
    Assert.assertEquals(1, result._1)
    Assert.assertTrue(result._2.contains("different base"))
  }

  @Test def conflictCanBeResolvedAndResumed(): Unit = withRepo { repo =>
    val commit = git(repo, "rev-parse", "HEAD")
    git(repo, "checkout", "-b", "conflicting", "HEAD^")
    Files.writeString(repo.resolve("file.txt"), "different\n")
    git(repo, "commit", "-am", "Conflicting change")
    git(repo, "update-ref", "refs/remotes/origin/main", "HEAD")
    val bp = BackportOpts(commit = Some(commit), apply = true, targets = Seq("main", "release/*"))
    val failed = run(repo, bp)
    Assert.assertEquals(1, failed._1)
    Assert.assertTrue(failed._2.contains("Worktree retained"))
    Assert.assertTrue(failed._2.contains("READY release/1x"))
    val pending = run(repo, bp)
    Assert.assertTrue(pending._2.contains("Worktree has unresolved conflicts or uncommitted changes"))
    val checkout = git(repo, "worktree", "list", "--porcelain").linesIterator
      .filter(_.startsWith("worktree ")).map(line => Path.of(line.stripPrefix("worktree "))).find(_ != repo).get
    Files.writeString(checkout.resolve("file.txt"), "resolved\n")
    git(checkout, "add", "file.txt")
    git(checkout, "-c", "core.editor=true", "cherry-pick", "--continue")
    val resumed = run(repo, bp)
    Assert.assertEquals(resumed._2, 0, resumed._1)
    Assert.assertTrue(resumed._2.contains("READY main"))
  }

  @Test def validatesTargetsCommitsAndBranchCollisions(): Unit = withRepo { repo =>
    val commit = git(repo, "rev-parse", "HEAD")
    Assert.assertEquals(1, run(repo, BackportOpts(commit = Some("HEAD^")))._1)
    Assert.assertEquals(1, run(repo, BackportOpts(commit = Some("missing")))._1)
    Assert.assertEquals(1, run(repo, BackportOpts(commit = Some(commit), remote = "missing"))._1)
    Assert.assertEquals(1, run(repo, BackportOpts(commit = Some(commit), targets = Seq("main", "missing/*")))._1)
    git(repo, "update-ref", "refs/remotes/origin/qa/a-b", "HEAD^")
    git(repo, "update-ref", "refs/remotes/origin/qa/a/b", "HEAD^")
    val collision = run(repo, BackportOpts(commit = Some(commit), apply = true, targets = Seq("qa/a*")))
    Assert.assertEquals(1, collision._1)
    Assert.assertTrue(collision._2.contains("duplicate merge request branch names"))
    Assert.assertEquals("source", git(repo, "branch", "--format=%(refname:short)"))
  }

  @Test def fetchAndCustomDescription(): Unit = withRepo { repo =>
    val commit = git(repo, "rev-parse", "HEAD")
    git(repo, "branch", "main", "HEAD^")
    Files.writeString(repo.resolve("mr.txt"), "Custom customer's description\r\nSecond line\n")
    val result = run(repo,
      BackportOpts(commit = Some(commit), apply = true, fetch = true,
        targets = Seq("main"), descriptionFile = Some("mr.txt")))
    Assert.assertEquals(result._2, 0, result._1)
    Assert.assertTrue(result._2.contains("fetched before planning"))
    Assert.assertTrue(result._2.contains("Second line"))
    Assert.assertTrue(result._2.contains("merge_request.description=Custom"))
  }

  @Test def rejectsMergeCommit(): Unit = withRepo { repo =>
    git(repo, "checkout", "-b", "other", "HEAD^")
    git(repo, "commit", "--allow-empty", "-m", "Other branch")
    git(repo, "checkout", "source")
    git(repo, "merge", "--no-ff", "other", "-m", "Merge")
    val result = run(repo, BackportOpts(commit = Some("HEAD")))
    Assert.assertEquals(1, result._1)
    Assert.assertTrue(result._2.contains("merge and root commits"))
  }

  @Test def descriptionsGroupTrailersAtTheEnd(): Unit = withRepo { repo =>
    val input = "Task: ABC-123\r\n\r\nExplain customer's fix.\r\n\r\n" +
      "Reviewed-by: Alice <alice@example.invalid>\r\n  and Bob <bob@example.invalid>\r\n\r\n" +
      "Additional context.\r\n\r\nTask: ABC-456\r\n\r\nChange-Id: I123\r\n"
    val expected = "Explain customer's fix.\n\nAdditional context.\n\n" +
      "Task: ABC-123\nReviewed-by: Alice <alice@example.invalid>\n  and Bob <bob@example.invalid>\n" +
      "Task: ABC-456\nChange-Id: I123"
    git(repo, "commit", "--amend", "-m", "Fix customer's bug\n\n" + input)
    val commit = git(repo, "rev-parse", "HEAD")
    Seq(None, Some("mr.txt")).foreach { file =>
      file.foreach(path => Files.writeString(repo.resolve(path), input))
      val result = run(repo, BackportOpts(commit = Some(commit), apply = true, targets = Seq("main"), descriptionFile = file))
      Assert.assertEquals(result._2, 0, result._1)
      val command = result._2.linesIterator.find(_.startsWith("git push ")).get
      val description = Process(Seq("sh", "-c", "set -- " + command + "; printf '%s' \"${13}\"")).!!.trim
      Assert.assertEquals("merge_request.description=" + expected.replace("\n", "\\n"), description)
    }
    val parsed =
      (Process(Seq("git", "-C", repo.toString, "interpret-trailers", "--parse"))
        #< new ByteArrayInputStream(expected.getBytes(java.nio.charset.StandardCharsets.UTF_8))).!!.trim
    Assert.assertEquals("Task: ABC-123\nReviewed-by: Alice <alice@example.invalid> and Bob <bob@example.invalid>\n" +
        "Task: ABC-456\nChange-Id: I123", parsed)
    Assert.assertEquals(expected, Backport.formatDescription(expected))
  }

  @Test def descriptionFormattingPreservesCodeBlocksAndHandlesEmptyBodies(): Unit = {
    val body = "Details:\n```yaml\nTask: example\n\n\nkey: value\n```\n\nMore context."
    Assert.assertEquals(body + "\n\nTask: ABC-123", Backport.formatDescription("Task: ABC-123\n\n" + body))
    Assert.assertEquals("Task: ABC-123\nChange-Id: I123",
      Backport.formatDescription("\nTask: ABC-123\n\nChange-Id: I123\n"))
    Assert.assertEquals("", Backport.formatDescription("\r\n\n"))
    Assert.assertEquals("Plain text.", Backport.formatDescription("\nPlain text.\n"))
  }

  @Test def shellQuotingAndGlobMatching(): Unit = withRepo { repo =>
    org.junit.Assume.assumeFalse(System.getProperty("os.name").toLowerCase.contains("win"))
    val gitExecutable = repo.resolve("custom path").resolve("git")
    Files.createDirectories(gitExecutable.getParent)
    Files.writeString(gitExecutable, "#!/bin/sh\nexec git \"$@\"\n")
    Assert.assertTrue(gitExecutable.toFile.setExecutable(true))
    val git = Sgit(repo.toFile, doVerify = false, out = System.out, err = System.err,
      gitBin = Some(gitExecutable.toString), opts = Opts())
    val command = git.gitlabMergeRequestPushCommand("origin", "backport/test", "release/1x", "Customer's $HOME `id`", "one\r\ntwo\n")
    Assert.assertTrue(command.startsWith("'" + gitExecutable + "' push "))
    Assert.assertFalse(command.contains("\n"))
    Assert.assertTrue(command.contains("one\\ntwo\\n"))
    val parsed = Process(Seq("sh", "-c", "set -- " + command + "; printf '%s' \"${11}\"")).!!.trim
    Assert.assertTrue(command.contains("'\"'\"'"))
    Assert.assertEquals("merge_request.title=Customer's $HOME `id`", parsed)
    val checkout = repo.resolve("checkout with spaces").toFile
    val continueCommand = git.conflictResolutionCommands(checkout).last
    val parsedDirectory = Process(Seq("sh", "-c", "set -- " + continueCommand + "; printf '%s' \"$3\"")).!!.trim
    Assert.assertTrue(continueCommand.startsWith("'" + gitExecutable + "' -C "))
    Assert.assertEquals(checkout.getAbsolutePath, parsedDirectory)
    Assert.assertTrue(Backport.matches("qa/a/b", "qa/*"))
    Assert.assertTrue(Backport.matches("release/12x", "release/[!a-z]?x"))
    Assert.assertFalse(Backport.matches("Main", "main"))
    Assert.assertFalse(Backport.matches("qa/ax", "qa/[0-9]x"))
  }
}
