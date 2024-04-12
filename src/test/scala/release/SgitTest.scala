package release

import org.junit.{Assert, Assume, Ignore, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Sgit.{GitRemote, GitTagWithDate, MissingGitDirException}
import release.SgitTest.hasCommitMsg
import release.Starter.{Opts, PreconditionsException}

import java.io.File
import java.nio.file.{Files, StandardCopyOption}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

class SgitTest extends AssertionsForJUnit {

  @Test
  def testVersionOnly(): Unit = {
    println(Sgit.versionOnly().version())
  }

  @Test
  def testSplitLineOnWhitespace(): Unit = {
    Assert.assertEquals(Seq("a", "b"), Sgit.splitLineOnWhitespace("a b"))
  }

  @Test
  def testSelectGitCmd(): Unit = {
    val git = Sgit.selectedGitCmd(System.err, None).get

    Term.Os.getCurrent match {
      case Term.Os.Windows => {
        Assert.assertEquals(Seq("C:\\Programme\\Git\\bin\\git.exe"), git)
      }
      case Term.Os.Linux => {
        Assert.assertEquals(Seq("git"), git)
      }
      case Term.Os.Darwin => {
        Assert.assertEquals(Seq("git"), git)
      }
      case other => Assert.fail("unknown os: " + other + " => " + git)
    }
  }

  @Test
  def testMissingGitDir(): Unit = {

    TestHelper.assertExceptionWithCheck(message => Assert.assertEquals("no .git dir in sgit-test was found. " +
      "Please change dir to the project folder.",
      message.replaceFirst("[^ ]+sgit-test-[^ ]+", "sgit-test"))
      , classOf[MissingGitDirException], () => {
        val temp = Files.createTempDirectory("sgit-test-").toFile.getAbsoluteFile
        temp.deleteOnExit()
        Sgit(file = temp, doVerify = hasCommitMsg, out = System.out, err = System.err, gitBin = None, opts = Opts())
      })

  }

  @Test
  def testCommitId(): Unit = {
    // GIVEN
    val git = SgitTest.workSgit()
    SgitTest.assumeNoShallowClone(git)
    // WHEN
    val commitId = git.commitId("606411c")

    // THEN
    Assert.assertEquals("606411c1e4d62030144e9351fe0567cf3a5e5046", commitId)
  }

  @Test
  def testFindUpstream(): Unit = {
    // GIVEN
    val testee = SgitTest.workSgit()
    val currentBranch = testee.currentBranch
    Assume.assumeTrue("not master", currentBranch.contains("master"))

    // WHEN
    val branch = testee.findUpstreamBranch().getOrElse("other")

    // THEN
    Assert.assertEquals("master", branch)
    Assert.assertEquals(Some("HEAD branch: master"), testee.remoteHead().get)
    Assert.assertEquals(Some("origin/master"), testee.remoteHeadRaw().get)
  }

  @Test
  def testCurrentBranch(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val branch = status.currentBranch

    // THEN
    Assert.assertTrue("unknown branch " + branch, Seq("master", "HEAD").contains(branch))
  }

  @Test
  def testCommitIdHEAD(): Unit = {
    // GIVEN
    val git = SgitTest.workSgit()
    SgitTest.assumeNoShallowClone(git)
    // WHEN
    val commitId = git.commitId("origin/master")

    // THEN
    Assert.assertEquals(40, commitId.length)
  }

  @Test
  def testCommitIds(): Unit = {
    // GIVEN
    val git = SgitTest.workSgit()
    SgitTest.assumeNoShallowClone(git)

    // WHEN
    val commitIds = git.commitIds("21a1a3f", "606411c").map(_.substring(0, 7))

    // THEN
    Assert.assertTrue(commitIds.isInstanceOf[List[String]])
    Assert.assertEquals(Seq("21a1a3f", "b299ac9", "29c5b35"), commitIds)
  }

  @Test
  def testSelectUpstream_master(): Unit = {

    Assert.assertEquals(Some("master"), SgitTest.workSgit().selectUpstream(Some("origin/master")))

  }

  @Test
  def testSelectUpstream_feature(): Unit = {

    Assert.assertEquals(Some("feature/any"), SgitTest.workSgit().selectUpstream(Some("origin/feature/any")))

  }

  @Test
  def testSelectUpstream_none(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(None))

  }

  @Test
  def testSelectUpstream_other_master(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(Some("other/master")))

  }

  @Test
  def testSelectUpstream_other_feature(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(Some("other/feature/any")))

  }

  @Test
  def testOutLogger(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, in => Some(in))

    // WHEN
    testee.err("any")

    // THEN
    Assert.assertEquals(Seq("any"), err)
  }

  @Test
  def testOutLoggerFetchFilter_fatal(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.fetchFilter())

    // WHEN
    testee.err("blabla bla")

    testee.err("fatal: 'failfail' does not appear to be a git repository fatal: Could not read from remote repository.")
    testee.err("Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu")


    // THEN
    Assert.assertEquals(Seq(
      "fatal: 'failfail' does not appear to be a git repository fatal: Could not read from remote repository.",
      "Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu"), err)
    Assert.assertEquals(Nil, out)
  }

  @Test
  def testOutLoggerFetchFilter_error(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.fetchFilter())

    // WHEN
    testee.err("bla bla")
    testee.err("error: Could not resolve hostname git.example.org: Name or service not known fatal: Could not read from remote repository.")
    testee.err("bla bla")

    // THEN
    Assert.assertEquals(Seq(
      "error: Could not resolve hostname git.example.org: Name or service not known fatal: Could not read from remote repository."), err)
    Assert.assertEquals(Nil, out)
  }

  @Test
  def testOutLoggerFetchFilter_bang(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.fetchFilter())

    // WHEN
    testee.err("bla bla")
    testee.err(" ! [rejected]        v0.0.10    -> v0.0.10  (would clobber existing tag).")
    testee.err("error: Could not fetch origin")
    testee.err("bla bla")
    // THEN
    Assert.assertEquals(Seq(
      " ! [rejected]        v0.0.10    -> v0.0.10  (would clobber existing tag).",
      "error: Could not fetch origin"), err)
    Assert.assertEquals(Nil, out)
  }

  @Test
  def testOutLoggerFetchFilter_ssh(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.fetchFilter())

    // WHEN
    testee.err("bla bla")
    testee.err("ssh: Could not resolve hostname git.example.org: Name or service not known fatal: Could not read from remote repository.")
    testee.err("bla bla")

    // THEN
    Assert.assertEquals(Seq(
      "ssh: Could not resolve hostname git.example.org: Name or service not known fatal: Could not read from remote repository."), err)
    Assert.assertEquals(Nil, out)
  }

  @Test
  def testOutLoggerFetchFilter(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.fetchFilter())

    // WHEN
    testee.err("Total 31 (delta 0), reused 0 (delta 0)")
    testee.err("Total 286 (delta 177), reused 278 (delta 177)")
    testee.err("Fetching origin")
    testee.err("  remote: Counting objects: 1569, done")
    testee.err("remote: Finding sources: 100% (554/554)")
    testee.err("Receiving objects: 100% (554/554), 152.53 KiB | 3.05 MiB/s, done.")
    testee.err("  Resolving deltas: 100% (307/307), completed with 80 local objects.")
    testee.err("  From ssh://git-ishop.novomind.com:19418/ishop/shops/ags")
    testee.err("e404678f34..48e06ab000  master                -> origin/master")
    testee.err("* [new branch]            feature/fb-ags-update -> origin/feature/fb-ags-update")
    testee.err("* [new branch]            release/RC-2019.37    -> origin/release/RC-2019.37")
    testee.err("* [new branch]            release/RC-2019.40    -> origin/release/RC-2019.40")

    // THEN
    Assert.assertEquals(Nil, err)
    Assert.assertEquals(Nil, out)
  }

  @Test
  def testOutLoggerGerrit_push(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.gerritPushFilter)

    // WHEN
    testee.err("remote: ")
    testee.err("remote: Processing changes: new: 2, refs: 3")
    testee.err("remote: Processing changes: new: 2, refs: 3")
    testee.err("remote: Processing changes: new: 2, refs: 3, done")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: SUCCESS        ")
    testee.err("remote: New Changes:")
    testee.err("remote: New Changes:        ")
    testee.err("remote:   https://any-gerrit:8443/72458 snap weg")
    testee.err("remote:   https://any-gerrit:8443/72459 [ishop-release] prepare for next iteration - 29.0.6")
    testee.err("remote: ")

    // THEN
    Assert.assertEquals(List("See https://any-gerrit:8443/72458 snap weg",
      "See https://any-gerrit:8443/72459 [ishop-release] prepare for next iteration - 29.0.6"), err)
  }

  @Test
  def testOutLoggerGerrit_push_fail(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.gerritPushFilter)

    // WHEN
    testee.err("remote: ")
    testee.err("remote: Processing changes: refs: 2")
    testee.err("remote: Processing changes: refs: 2, done")
    testee.err("remote: ")
    testee.err("remote: error: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\"")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: error: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\"")
    testee.err("To ssh://anyone@any-gerrig:29418/ishop/user/anyone/sonar-demo")
    testee.err(" ! [remote rejected] master -> refs/for/master " +
      "(Commit Message Problem: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\")")
    testee.err(" ! [remote rejected] v29.0.6 -> v29.0.6 " +
      "(Commit Message Problem: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\")")
    testee.err("error: failed to push some refs to 'ssh://anyone@any-gerrig:29418/ishop/user/anyone/sonar-demo'")

    // THEN
    Assert.assertEquals(
      List(
        """[remote rejected] master -> refs/for/master
          |(Commit Message Problem: Line is too long. Reduce it to 72 chars please;
          |"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge")""".stripMargin
          .replaceAll("[\r\n]+", " "),
        """[remote rejected] v29.0.6 -> v29.0.6
          |(Commit Message Problem: Line is too long. Reduce it to 72 chars please;
          |"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge")""".stripMargin
          .replaceAll("[\r\n]+", " "),
        """git-err: 'error: failed to push some refs to
          |'ssh://anyone@any-gerrig:29418/ishop/user/anyone/sonar-demo''""".stripMargin
          .replaceAll("[\r\n]+", " ")), err)
  }

  @Test
  @Ignore
  def testOutLoggerPushFilter_error_no_new_changes(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(
      in => err = err :+ in, in => out = out :+ in, Sgit.gerritPushFilter)

    // WHEN
    testee.err("bla bla")
    testee.err(" ! [remote rejected] HEAD -> refs/for/master (no new changes)")
    testee.err("error: failed to push some refs to 'ssh://www.example.org/path/to/project'")
    testee.err("bla bla")

    // THEN
    Assert.assertEquals(Seq(
      "error: Could not resolve hostname git.example.org: Name or service not known fatal: Could not read from remote repository."), err)
    Assert.assertEquals(Nil, out)
  }

  @Test
  def testUnw(): Unit = {
    Assert.assertEquals("a", unw("a"))
    Assert.assertEquals("a\na", unw("a\na"))
    Assert.assertEquals("a\na ", unw("\"a\na \""))
    Assert.assertEquals("a\na ", unw("\"a\na \" "))
    Assert.assertEquals("a\na ", unw("\"a\na \" \n"))
    Assert.assertEquals("a\na ", unw("\"a\na \"\n"))
    Assert.assertEquals("a\na ", unw("\"\"a\na \"\"\n"))
    Assert.assertEquals("a\n\"a ", unw("\"\"a\n\"a \"\"\n"))
  }

  @Test
  def testGitNative(): Unit = {
    TestHelper.assertException("Nonzero exit value: 1; git --no-pager iutghiprjhpeth; " +
      "git: 'iutghiprjhpeth' is not a git command. See 'git --help'.",
      classOf[RuntimeException], () => {
        SgitTest.workSgit().gitNative(Seq("iutghiprjhpeth")).get
      })

  }

  private def testFailIllegal(expectedMsg: String, fn: () => Unit): Unit = {
    TestHelper.assertException(expectedMsg, classOf[IllegalStateException], fn)
  }

  @Test
  def testInitCloneCommitPoms(): Unit = {
    // GIVEN
    val testRepoA = SgitTest.ensureAbsent("a")
    val testRepoB = SgitTest.ensureAbsent("b")
    val testRepoI = SgitTest.ensureAbsent("i")

    // WHEN
    val gitA = Sgit.init(testRepoA, SgitTest.hasCommitMsg)
    Assert.assertEquals(None, gitA.currentTags)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")

    TestHelper.assertException("Nonzero exit value: 1; git --no-pager push -q -u origin master:refs/for/master; " +
      "git-err: 'error: src refspec master does not match any' " +
      "git-err: 'error: failed to push some refs to 'origin''",
      classOf[RuntimeException], () => {
        gitA.pushFor("master", "master")
      })

    gitA.fetchAll()
    Assert.assertEquals(Nil, gitA.listRemotes())
    gitA.addRemote("ubglu", "failfail")

    Term.Os.getCurrent match {
      case Term.Os.Darwin => {
        TestHelper.assertException("Nonzero exit value: 1; " +
          "git --no-pager fetch --all --tags; fatal: 'failfail' does not appear to be a git repository " +
          "fatal: Could not read from remote repository. Please make sure you have the correct access rights " +
          "and the repository exists. error: could not fetch ubglu",
          classOf[RuntimeException], () => {
            gitA.fetchAll()
          })
      }
      case Term.Os.Linux => {
        println(s"git version ${gitA.version()}")
        gitA.version() match {
          case gv: String if gv.startsWith("git version 2.30") => {
            TestHelper.assertException("Nonzero exit value: 1; " +
              "git --no-pager fetch --all --tags; fatal: 'failfail' does not appear to be a git repository " +
              "fatal: Could not read from remote repository. Please make sure you have the correct access rights " +
              "and the repository exists. error: Could not fetch ubglu",
              classOf[RuntimeException], () => {
                gitA.fetchAll()
              })
          }
          case any => {
            TestHelper.assertException("Nonzero exit value: 128; " +
              "git --no-pager fetch --all --tags; fatal: 'failfail' does not appear to be a git repository " +
              "fatal: Could not read from remote repository. Please make sure you have the correct access rights " +
              "and the repository exists.",
              classOf[RuntimeException], () => {
                gitA.fetchAll()
              })
          }
        }
      }
      case Term.Os.Windows => {
        TestHelper.assertException("Nonzero exit value: 128; " +
          "git --no-pager fetch --all --tags; fatal: 'failfail' does not appear to be a git repository " +
          "fatal: Could not read from remote repository. Please make sure you have the correct access rights " +
          "and the repository exists.",
          classOf[RuntimeException], () => {
            gitA.fetchAll()
          })
      }
      case other => Assert.fail("unknown os: " + other)
    }
    TestHelper.assertException("Nonzero exit value: 1; git --no-pager push -q -u origin master:refs/heads/master; " +
      "git-err: 'error: src refspec master does not match any' " +
      "git-err: 'error: failed to push some refs to 'origin''",
      classOf[RuntimeException], () => {
        gitA.pushHeads("master", "master")
      })

    Assert.assertEquals(Seq(GitRemote.of("ubglu", "failfail", "(fetch)"), GitRemote.of("ubglu", "failfail", "(push)")),
      gitA.listRemotes())
    gitA.removeRemote("ubglu")
    gitA.addRemote("ubglu", "ssh://user@git.example.org/ubglu")

    def failFetchAll(expectedMsg: String*): Unit = {
      TestHelper.assertExceptionWithCheck(message => {
        val selected = expectedMsg.find(_ == message)
        if (selected.isDefined) {
          Assert.assertEquals(selected.get, message)
        } else {
          Assert.assertEquals(expectedMsg.head, message)
        }

      },
        classOf[RuntimeException], () => {
          gitA.fetchAll()
        })
    }
    Assert.assertEquals("after 1 millisecond to ssh://git.example.org/ubglu", gitA.remoteHead(timeout = Duration(1, TimeUnit.MILLISECONDS), debugFn = () => {
      Thread.sleep(10_000)
    }).failed.get.getMessage)

    Term.Os.getCurrent match {
      // TODO a git version change in 2.21
      //   failed: expected:<...r does not match any[.]' git-err: 'error: f...> but was:<...r does not match any[]' git-err: 'error: f...>
      case Term.Os.Windows => {
        failFetchAll("Nonzero exit value: 128; git --no-pager fetch --all --tags; " +
          "ssh: Could not resolve hostname git.example.org: Name or service not known fatal: " +
          "Could not read from remote repository. " +
          "Please make sure you have the correct access rights and the repository exists.")
      }
      case Term.Os.Linux => {
        gitA.version() match {
          // TODO change default later
          case gv: String if gv.startsWith("git version 2.29") ||
            gv.startsWith("git version 2.30") ||
            gv.startsWith("git version 2.31") ||
            gv.startsWith("git version 2.32") => {
            val var1 = "Nonzero exit value: 1; git --no-pager fetch --all --tags; " +
              "ssh: Could not resolve hostname git.example.org: Name or service not known fatal: " +
              "Could not read from remote repository. " +
              "Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu"
            val var2 = "Nonzero exit value: 1; git --no-pager fetch --all --tags; " +
              "ssh: Could not resolve hostname git.example.org: Temporary failure in name resolution fatal: " +
              "Could not read from remote repository. " +
              "Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu"
            failFetchAll(var1, var2)
          }
          case gv: String if gv.startsWith("git version 2.31") => {
            failFetchAll("Nonzero exit value: 1; git --no-pager fetch --all --tags; " +
              "ssh: Could not resolve hostname git.example.org: Name or service not known fatal: " +
              "Could not read from remote repository. " +
              "Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu")
          }
          case _ => {
            failFetchAll("Nonzero exit value: 128; git --no-pager fetch --all --tags; " +
              "ssh: Could not resolve hostname git.example.org: Name or service not known fatal: " +
              "Could not read from remote repository. " +
              "Please make sure you have the correct access rights and the repository exists.")
          }
        }

      }
      case Term.Os.Darwin => {
        failFetchAll("Nonzero exit value: 1; git --no-pager fetch --all --tags; " +
          "ssh: Could not resolve hostname git.example.org: nodename nor servname provided, or not known fatal: " +
          "Could not read from remote repository. " +
          "Please make sure you have the correct access rights and the repository exists. error: could not fetch ubglu")
      }
      case other => Assert.fail("unknown os: " + other)
    }

    gitA.removeRemote("ubglu")
    Assert.assertEquals(Nil, gitA.listBranchesLocal())
    Assert.assertEquals(Nil, gitA.lsFiles())
    Assert.assertEquals(Nil, gitA.listBranchRemoteRefRemotes())
    Assert.assertEquals(None, gitA.findUpstreamBranch())
    SgitTest.copyMsgHook(testRepoA)

    Assert.assertEquals(Nil, gitA.localChanges())
    gitA.add(SgitTest.testFile(testRepoA, "test"))

    Assert.assertEquals(Seq("A test"), gitA.localChanges())
    Assert.assertEquals(Seq("diff --git a/test b/test", "new file mode 100644"), gitA.diffSafe())
    Assert.assertEquals("881b90dfca6078fea145fc33cf7f5b7b656739f1", Starter.sign(gitA))
    gitA.commitAll("add test")
    Assert.assertEquals(Seq("Change-Id: I0000000000000000000000000000000000000000"), gitA.commitMessageBody("HEAD"))
    Assert.assertEquals(Nil, gitA.localChanges())
    Assert.assertEquals("master", gitA.currentBranch)
    gitA.worktreeAdd("../" + testRepoI.getName)
    Assert.assertEquals(Seq("refs/heads/" + testRepoI.getName, "refs/heads/master"), gitA.listBranchesLocal().map(_.branchName))
    gitA.worktreeRemove(testRepoI.getName)
    Assert.assertEquals(None, gitA.findUpstreamBranch())
    val gitB = Sgit.doClone(testRepoA, testRepoB, SgitTest.hasCommitMsg)
    gitB.configSetLocal("user.email", "you@example.com")
    gitB.configSetLocal("user.name", "Your Name")
    SgitTest.copyMsgHook(testRepoB)
    Assert.assertEquals(None, gitA.configGetLocalAll("receive.denyCurrentBranch"))
    gitA.configSetLocal("receive.denyCurrentBranch", "warn")
    Assert.assertEquals(Seq("warn"), gitA.configGetLocalAll("receive.denyCurrentBranch").get)
    gitA.configAddLocal("core.bert", "bert")
    Assert.assertEquals(Seq("bert"), gitA.configGetLocalAll("core.bert").get)
    gitA.configSetLocal("core.bert", "blub")
    gitA.configAddLocal("core.bert", "blub-blub")
    Assert.assertEquals(Seq("blub", "blub-blub"), gitA.configGetLocalAll("core.bert").get)
    gitA.configRemoveLocal("core.bert", "blub-blub")
    Assert.assertEquals(Seq("blub"), gitA.configGetLocalAll("core.bert").get)
    gitA.configRemoveLocal("core.bert", "blub")
    Assert.assertEquals(None, gitA.configGetLocalAll("core.bert"))
    gitA.configSetLocal("core.bert", "blub")
    gitA.configAddLocal("core.bert", "blub-blub")
    gitA.configRemoveLocal("core.bert", "blub") // feels like stats with
    Assert.assertEquals(None, gitA.configGetLocalAll("core.bert"))

    gitA.add(SgitTest.testFile(testRepoA, "test2"))
    gitA.commitAll("add test2")
    gitB.fetchAll()
    Assert.assertEquals(Seq("origin/master"), gitB.listBranchRemoteRaw().map(_.branchName))
    Assert.assertEquals(Seq("origin/master"), gitB.listBranchNamesRemote())
    Assert.assertEquals(Seq("refs/remotes/origin/master"), gitB.listBranchRemoteRefRemotes().map(_.branchName))
    Assert.assertEquals(Seq("master"), gitB.listBranchNamesRemoteShort())
    Assert.assertEquals(Seq("master"), gitB.listBranchNamesAll())
    Assert.assertEquals("master", gitB.currentBranch)
    Assert.assertEquals(None, gitB.currentTags)
    gitB.doTag("Work-no-tag")
    Assert.assertEquals(None, gitB.currentTags)
    gitB.deleteTag("Work-no-tag")
    Assert.assertEquals(None, gitB.currentTags)
    Assert.assertEquals("master", gitB.findUpstreamBranch().get)
    Assert.assertFalse(gitB.hasChangesToPush)

    Assert.assertEquals(Some("master"), gitB.findUpstreamBranch())
    val beforeReverts = gitB.commitId("HEAD")
    gitB.revertHead()
    Assert.assertEquals(Seq("This reverts commit ..."),
      gitB.commitMessageBody("HEAD").map(_.replaceFirst("[0-9a-f]{40}.$", "...")))
    gitB.revertHead()
    TestHelper.assertExceptionWithCheck(message =>
      Assert.assertEquals("The commits 000, 000 has no ChangeId lines. Please amend them manually.",
        message.replaceAll("[0-9a-f]{40}", "000"))
      , classOf[PreconditionsException], () => {
        gitB.pushFor("master", "master")
      })
    gitB.resetHard(beforeReverts)
    gitB.pushFor("master", "master")
    Assert.assertEquals(Nil, gitA.listAllTags())
    Assert.assertEquals(Nil, gitB.listAllTags())
    Assert.assertEquals(None, gitB.currentTags)
    gitB.doTag("0.0.10")
    val beforeBranch = gitB.currentBranch
    gitB.checkout("v0.0.10")
    Assert.assertEquals("HEAD", gitB.currentBranch)
    Assert.assertEquals(Some(Seq("v0.0.10")), gitB.currentTags)
    gitB.checkout(beforeBranch)
    Assert.assertEquals(None, gitB.currentTags)
    gitA.doTag("0.0.10")
    Term.Os.getCurrent match {
      case Term.Os.Darwin => {
        TestHelper.assertExceptionWithCheck(message =>
          Assert.assertEquals("Nonzero exit value: 1; git --no-pager fetch --all --tags;" +
            " ! [rejected]        v0.0.10    -> v0.0.10  (would clobber existing tag)" +
            " error: could not fetch origin",
            message.replaceFirst(" From [^ ]+", ""))
          , classOf[RuntimeException], () => {
            gitB.fetchAll()
          })
      }
      case Term.Os.Linux => {
        gitB.version() match {
          case gv: String if gv.startsWith("git version 2.30") => {
            TestHelper.assertExceptionWithCheck(message =>
              Assert.assertEquals("Nonzero exit value: 1; git --no-pager fetch --all --tags;" +
                " ! [rejected]        v0.0.10    -> v0.0.10  (would clobber existing tag)" +
                " error: Could not fetch origin",
                message.replaceFirst(" From [^ ]+", ""))
              , classOf[RuntimeException], () => {
                gitB.fetchAll()
              })
          }
          case any => {
            TestHelper.assertExceptionWithCheck(message =>
              Assert.assertEquals("Nonzero exit value: 1; git --no-pager fetch --all --tags;" +
                " ! [rejected]        v0.0.10    -> v0.0.10  (would clobber existing tag)",
                message.replaceFirst(" From [^ ]+", ""))
              , classOf[RuntimeException], () => {
                gitB.fetchAll()
              })

          }
        }

      }
      case Term.Os.Windows => {
        TestHelper.assertExceptionWithCheck(message =>
          Assert.assertEquals("Nonzero exit value: 1; git --no-pager fetch --all --tags;" +
            " ! [rejected]        v0.0.10    -> v0.0.10  (would clobber existing tag)" +
            "",
            message.replaceFirst(" From [^ ]+", ""))
          , classOf[RuntimeException], () => {
            gitB.fetchAll()
          })
      }
      case other => Assert.fail("unknown os: " + other)
    }

    gitA.deleteTag("0.0.10")
    gitA.createBranch("lulul")
    gitB.fetchAll()
    gitA.deleteBranch("lulul")
    gitB.fetchAll()
    gitB.remotePruneOrigin()
    val v10 = SgitTest.testFile(testRepoB, "v10")
    gitB.add(v10)
    gitB.commitAll("bla")
    gitB.doTag("0.0.9")
    v10.delete()
    gitB.add(v10)
    gitB.commitAll("delete v10")
    gitB.doTag("0.0.8")
    Assert.assertEquals(Seq("v0.0.10", "v0.0.8", "v0.0.9"), gitB.listAllTags())
    Assert.assertEquals(Nil, gitA.listAllTags())
    Assert.assertEquals(Seq("v0.0.8", "v0.0.9", "v0.0.10"),
      gitB.listTagsWithDate().map(_.name))
    Assert.assertTrue(gitB.hasTagsToPush)
    gitB.pushTag("0.0.9")
    Assert.assertTrue(gitB.hasTagsToPush)
    Assert.assertEquals(Seq("v0.0.9"), gitA.listAllTags())
    gitB.pushTag("0.0.10")
    gitB.pushTag("0.0.8")
    Assert.assertFalse(gitB.hasTagsToPush)
    TestHelper.assertException("Nonzero exit value: 128; git --no-pager tag va*; fatal: 'va*' is not a valid tag name.",
      classOf[RuntimeException], () => gitB.doTag("a*"))

    Assert.assertFalse(gitB.hasTagsToPush)
    val pomFile = SgitTest.testFile(testRepoB, "pom.xml")
    val sub = new File(testRepoB, "sub")
    sub.mkdir()
    val subPomFile = SgitTest.testFile(sub, "pom.xml")
    Assert.assertEquals(Seq("?? pom.xml", "?? sub/"), gitB.localChanges())
    gitB.stash()
    Assert.assertEquals(Nil, gitB.localChanges())
    gitB.stashPop()
    Assert.assertEquals(Seq("?? pom.xml", "?? sub/"), gitB.localChanges())
    gitB.add(pomFile)
    Assert.assertEquals(Seq("A pom.xml", "?? sub/"), gitB.localChanges())
    testFailIllegal("only (pom.xml) changes are allowed => A pom.xml, ?? sub/ => sub/ <= pom.xml, sub/", () => {
      gitB.localPomChanges()
    })
    val anyFile = SgitTest.testFile(testRepoB, "any.xml")

    Assert.assertEquals(Seq("A pom.xml", "?? any.xml", "?? sub/"), gitB.localChanges())

    testFailIllegal("only (pom.xml) changes are allowed => A pom.xml, ?? any.xml, ?? sub/ => any.xml, sub/ <= any.xml, pom.xml, sub/", () => {
      gitB.doCommitPomXmls("fail")
    })

    testFailIllegal("only (any.xml, pom.xml) changes are allowed => A pom.xml, ?? any.xml, ?? sub/ => sub/ <= any.xml, pom.xml, sub/", () => {
      gitB.doCommitWithFilter("fail", Seq("pom.xml", "any.xml"))
    })

    gitB.add(anyFile)
    val otherFile = SgitTest.testFile(testRepoB, "schönes Ding")
    gitB.add(otherFile)
    val subject = "add " + Seq(pomFile, anyFile, otherFile).map(_.getName).mkString(", ") + "-"
    gitB.commitAll(subject + "\r\n\r\n test")
    Assert.assertEquals(Seq("test", "Change-Id: I..."),
      gitB.commitMessageBody("HEAD").map(_.replaceFirst("[0-9a-f]{40}$", "...")))
    assertMsg(Seq(subject, "", " test"), gitB)

    Assert.assertTrue(gitB.hasChangesToPush)
    Assert.assertFalse(gitB.hasLocalChanges)
    Util.write(pomFile, Seq("a"))
    Util.write(subPomFile, Seq("a", "b"))
    Assert.assertTrue(gitB.hasLocalChanges)
    Assert.assertEquals(Seq(
      "diff --git a/pom.xml b/pom.xml", "--- a/pom.xml", "+++ b/pom.xml",
      "@@ -0,0 +1 @@",
      "+a",
      "diff --git a/sub/pom.xml b/sub/pom.xml", "--- a/sub/pom.xml", "+++ b/sub/pom.xml",
      "@@ -0,0 +1,2 @@",
      "+a",
      "+b"
    ), gitB.diffSafe())

    Assert.assertEquals(Seq("M pom.xml", "M sub/pom.xml"), gitB.localChanges())
    Assert.assertEquals(Seq("pom.xml", "sub/pom.xml"), gitB.localPomChanges())
    gitB.doCommitPomXmls("update pom.xml\n\nSigned-off-by: Signer <signer@example.org>")
    Assert.assertEquals(Nil, gitB.localChanges())

    Assert.assertEquals(Seq("refs/heads/master", "refs/remotes/origin/HEAD",
      "refs/remotes/origin/master", "refs/tags/v0.0.10", "refs/tags/v0.0.8", "refs/tags/v0.0.9"), gitB.listRefNames())
    gitB.doTag("1.0.0")
    Assert.assertEquals(Seq("refs/heads/master", "refs/remotes/origin/HEAD",
      "refs/remotes/origin/master", "refs/tags/v0.0.10", "refs/tags/v0.0.8", "refs/tags/v0.0.9", "refs/tags/v1.0.0"),
      gitB.listRefNames())
    Assert.assertEquals(Seq("v0.0.10", "v0.0.8", "v0.0.9", "v1.0.0"), gitB.listAllTags())
    gitB.deleteRef("refs/tags/v1.0.0")
    Assert.assertEquals(Seq("v0.0.10", "v0.0.8", "v0.0.9"), gitB.listAllTags())
    Assert.assertEquals(Seq("refs/heads/master", "refs/remotes/origin/HEAD",
      "refs/remotes/origin/master", "refs/tags/v0.0.10", "refs/tags/v0.0.8", "refs/tags/v0.0.9"), gitB.listRefNames())
    gitB.doTag("1.0.0")
    gitB.doTag("1.0.1")
    Assert.assertEquals(
      """* 0000000 (HEAD -> master, tag: v1.0.1, tag: v1.0.0) update pom.xml
        |* 0000000 add pom.xml, any.xml, schönes Ding-
        |* 0000000 (tag: v0.0.8) delete v10""".stripMargin, gitB.logGraph().replaceAll("\\* [0-9a-f]{7} ", "* 0000000 "))
    Assert.assertEquals(Seq("any.xml", "pom.xml", "schönes Ding", "sub/pom.xml", "test"), gitB.lsFiles())
    Assert.assertEquals(Seq("master"), gitB.listBranchNamesLocal())
    gitB.checkout("v1.0.1")
    Assert.assertEquals(Seq("master"), gitB.listBranchNamesLocal())
    gitB.checkout("master")
    Assert.assertEquals(Seq("master"), gitB.listBranchNamesLocal())
    assertMsg(Seq("update pom.xml", "", "Signed-off-by: Signer <signer@example.org>"), gitB)

    Util.write(pomFile, Seq("b"))
    Util.write(subPomFile, Seq("b"))
    gitB.doCommitWithFilter("subset\n\ntest", Seq("pom.xml", "any.xml"))
    assertMsg(Seq("subset", "", "test"), gitB)
    Assert.assertEquals(false, gitB.isDetached)
    Assert.assertEquals(Some("master"), gitB.findUpstreamBranch())
    gitB.checkout("HEAD~1")
    Assert.assertEquals(None, gitB.findUpstreamBranch())
    Assert.assertEquals(true, gitB.isDetached)
    gitB.checkout("master")
    Assert.assertEquals(false, gitB.isDetached)
    Assert.assertEquals(true, gitB.isNotDetached)
    gitB.createBranch("feature/test")
    Assert.assertEquals(Seq("feature/test", "master"), gitB.listBranchNamesLocal())
    gitB.deleteBranch("feature/test")
    testFailIllegal("branch 'test' not found.", () => {
      gitB.deleteBranch("test")
    })
    Assert.assertEquals(Seq("master"), gitB.listBranchNamesLocal())
    Util.write(anyFile, Seq("a"))
    try {
      gitB.doCommitPomXmls("update pom.xml")
      Assert.fail()
    } catch {
      case e: IllegalStateException =>
        Assert.assertEquals("only (pom.xml) changes are allowed => M any.xml => any.xml <= any.xml", e.getMessage)
    }
    Assert.assertTrue(gitB.hasChangesToPush)
    gitB.pushFor("master", "master")
    gitB.add(anyFile)
    Assert.assertTrue(gitB.hasLocalChanges)
    gitB.commitAll("add " + Seq(anyFile).map(_.getName).mkString(", "))
    testFailIllegal("tag v1.0.0 already exists", () => {
      gitB.doTag("1.0.0")
    })
    Assert.assertFalse(gitB.hasLocalChanges)
    Assert.assertEquals(Seq("refs/heads/master"), gitB.listBranchesLocal().map(_.branchName))

    gitB.createBranch("any")
    Assert.assertEquals(Seq("refs/heads/any", "refs/heads/master"), gitB.listBranchesLocal().map(_.branchName))
    gitB.removeRemote("origin")
    gitB.addRemote("origin", "ssh://none@any-gerrit:29418/ishop/user/anyone/sonar-demo")
    val triedUnit = gitB.tryFetchAll()
    if (triedUnit.isFailure && triedUnit.failed.get.getMessage.contains("publickey")) {
      Term.Os.getCurrent match {
        case Term.Os.Darwin => {
          TestHelper.assertException("Nonzero exit value: 128; git --no-pager push -q -u origin master:refs/heads/master; " +
            "git-err: 'none@any-gerrit: Permission denied (publickey).' " +
            "git-err: 'fatal: Could not read from remote repository.' " +
            "git-err: 'Please make sure you have the correct access rights' " +
            "git-err: 'and the repository exists.'",
            classOf[RuntimeException], () => {
              gitB.pushHeads("master", "master")
            })
        }
        case _ => {
          TestHelper.assertException("Nonzero exit value: 128; git --no-pager push -q -u origin master:refs/heads/master; " +
            "git-err: 'Permission denied (publickey).' " +
            "git-err: 'fatal: Could not read from remote repository.' " +
            "git-err: 'Please make sure you have the correct access rights' " +
            "git-err: 'and the repository exists.'",
            classOf[RuntimeException], () => {
              gitB.pushHeads("master", "master")
            })
        }
      }

    }
    val expectedMailboxes = Seq("Your Name <you@example.com>")
    Assert.assertEquals(expectedMailboxes, gitA.listContributorMailboxes())
    Assert.assertEquals(expectedMailboxes, gitB.listContributorMailboxes())
  }

  @Test
  def testUnescape(): Unit = {
    Assert.assertEquals("On Windows try JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8 sbt assembly",
      "UTF-8", System.getProperty("file.encoding"))
    Assert.assertEquals("test", Sgit.unescape("test"))
    Assert.assertEquals("Präsentation", Sgit.unescape("Pr\\303\\244sentation"))
    Assert.assertEquals("\uA64A", Sgit.unescape("\\352\\231\\212"))
    Assert.assertEquals("\uD802\uDD25", Sgit.unescape("\\360\\220\\244\\245"))
  }

  @Test
  def testParseTagLog(): Unit = {
    val o = Seq("2019-09-25T15:47:39+02:00 (HEAD -> master, tag: v0.0.8)",
      "2019-09-25T15:47:30+02:00 (tag: v0.0.9, tag: v0.0.10)",
      "2019-09-25T15:47:31+02:00 (tag: v0.0,7, tag: v0.0.6)",
      "2019-09-99T15:99:31+02:00 (tag: v3)")
    val result = Sgit.parseTagLog(o)
    Assert.assertEquals(Seq(
      GitTagWithDate("v0.0.8", Sgit.parseIsoDate("2019-09-25T15:47:39+02:00")),
      GitTagWithDate("v0.0.9", Sgit.parseIsoDate("2019-09-25T15:47:30+02:00")),
      GitTagWithDate("v0.0.10", Sgit.parseIsoDate("2019-09-25T15:47:30+02:00")),
      GitTagWithDate("v0.0,7", Sgit.parseIsoDate("2019-09-25T15:47:31+02:00")),
      GitTagWithDate("v0.0.6", Sgit.parseIsoDate("2019-09-25T15:47:31+02:00")),
    ), result)
  }

  @Test
  def testSplitLineOnBranchlist(): Unit = {

    val master = Sgit.splitLineOnBranchlist("* master 915c1ce40ee979c3739e640e0a86ba68adea8681 Removed ...")
    Assert.assertEquals(("master", "915c1ce40ee979c3739e640e0a86ba68adea8681"), master.get)

    val any = Sgit.splitLineOnBranchlist("  bla  74036668cf893e4762ab4ff24ab4de8e44b70e33 Hub: SEE ..")
    Assert.assertEquals(("bla", "74036668cf893e4762ab4ff24ab4de8e44b70e33"), any.get)

    val detached = Sgit.splitLineOnBranchlist("* (HEAD detached at 97cbb59ea) 97cbb59ea40aacb4d8acad402bf90890741b0dbe Add ...")
    Assert.assertEquals(("97cbb59ea40aacb4d8acad402bf90890741b0dbe", "97cbb59ea40aacb4d8acad402bf90890741b0dbe"), detached.get)

    val some = TermTest.withOutErr[Option[(String, String)]]()(sys =>
      Sgit.splitLineOnBranchlistErr(sys.err)(" See merge request !380est          a5b54bf93f5a6b84f5f0833d315f9c6c3dfc1875 [gone] " +
        "Merge branch '904_inxmail_api' into 'sprint/2017.07'"))
    Assert.assertEquals(None, some.value)

    val some1 = TermTest.withOutErr[Option[(String, String)]]()(sys =>
      Sgit.splitLineOnBranchlistErr(sys.err)("bla\r\nbl"))

    Assert.assertEquals(None, some1.value)
    Assert.assertEquals("W: Unknown branch definition (check commit messages for second line empty, first line char limit): \"bla\nbl\". " +
      "See: git branch --list --verbose --no-abbrev", some1.err)
  }

  @Test
  def testSplitLineOnBranchlist_worktree(): Unit = {
    val some1 = TermTest.withOutErr[Option[(String, String)]]()(sys =>
      Sgit.splitLineOnBranchlistErr(sys.err)("+ bla                   74036668cf893e4762ab4ff24ab4de8e44b70e33 Some"))

    Assert.assertEquals(("bla", "74036668cf893e4762ab4ff24ab4de8e44b70e33"), some1.value.get)
  }

  private def assertMsg(expected: Seq[String], sgit: Sgit): Unit = {
    val gitRawOut = sgit.gitNative(Seq("log", "-n1", "--pretty=\"%B\"")).get
    val nativeLines = unw(gitRawOut.mkString("\n")).linesIterator.toList
    val body = nativeLines match {
      case lines if lines.last.startsWith("Change-Id:") => lines.dropRight(1)
      case lines => Assert.fail("invalid lines: " + lines.mkString("|"))
    }
    Assert.assertEquals(expected, body)
    val hookLines = nativeLines.takeRight(1)
    Assert.assertEquals("Change-Id:", hookLines.head.replaceFirst(" .*", "").trim)
  }

  private def unw(input: String) = {
    input.trim match {
      case in: String if in.startsWith("\"") && in.endsWith("\"") => in.replaceFirst("^[\"]+", "").replaceFirst("[\"]+$", "")
      case in => in
    }
  }

}

object SgitTest {
  private var testFolders: Set[String] = Set.empty
  private[release] val commitMsg = Sgit.findGit(Util.localWork, checkExisting = true).toPath.resolve(".git/hooks/commit-msg")
  private[release] val hasCommitMsg = Files.exists(commitMsg)

  def copyMsgHook(to: File): Unit = {
    if (hasCommitMsg) {
      Files.copy(commitMsg, to.toPath.resolve(".git/hooks/commit-msg"), StandardCopyOption.REPLACE_EXISTING)
    }
  }

  def workSgit(): Sgit = Sgit(file = Util.localWork, doVerify = hasCommitMsg, out = System.out, err = System.err,
    gitBin = None, opts = Opts())

  def testFile(folder: File, name: String): File = {
    val testFile = new File(folder, name)
    testFile.createNewFile()
    testFile
  }

  def assumeNoShallowClone(git: Sgit): Unit = {
    val isShallowClone = git.isShallowClone
    Assume.assumeFalse(isShallowClone)
  }

  def ensureAbsent(gitTestFolderName: String): File = {
    if (testFolders.contains(gitTestFolderName)) {
      throw new IllegalStateException(gitTestFolderName + " already in use")
    } else {
      testFolders = testFolders + gitTestFolderName
      val repoFolder = new File(Util.localWork, "target/" + gitTestFolderName)
      if (repoFolder.isDirectory) {
        Util.deleteRecursive(repoFolder)
      }
      repoFolder
    }
  }
}

