package release

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.BasicStatusManager
import ch.qos.logback.core.joran.GenericConfigurator
import ch.qos.logback.core.status.Status
import com.typesafe.scalalogging.LazyLogging
import org.junit.rules.Timeout
import org.junit.{Assert, Ignore, Rule, Test}
import org.mockito.Mockito._
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.Gav3
import release.Sgit.GitRemote
import release.Starter.{FutureEither, FutureError, LintOpts, Opts, OptsApidiff, OptsDepUp}

import java.io._
import java.util.concurrent.{TimeUnit, TimeoutException}
import scala.annotation.unused
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class StarterTest extends AssertionsForJUnit with LazyLogging {

  val _globalTimeout = new Timeout(20_000, TimeUnit.MILLISECONDS)

  @Rule def globalTimeout = _globalTimeout

  def doInit(params: Seq[String]): ExecReturn = {
    val in: InputStream = null
    val result = TermTest.withOutErrIn[Int](in)(sys => Starter.init(params, sys))
    ExecReturn(result.out, result.err, result.value)
  }

  case class ExecReturn(out: String, err: String, exit: Int)

  @Test
  def testTransformRemoteToBuildUrl_paypal(): Unit = {
    val result = Starter.transformRemoteToBuildUrl(
      Seq(GitRemote.of("origin", "ssh://someone@local-gerrit:29418/ishop/ext/ext-paypal.git", "remoteType")),
      "https://jenkins-url")
    Assert.assertEquals("https://jenkins-url/job/ishop-ext-ext-paypal-tag/", result.get)
  }

  @Test
  def testTransformRemoteToBuildUrl_core(): Unit = {
    val result = Starter.transformRemoteToBuildUrl(
      Seq(GitRemote.of("origin", "ssh://someone@local-gerrit:29418/ishop/core/ishop-core-projects", "(fetch)")),
      "https://jenkins-url")
    Assert.assertEquals("https://jenkins-url/job/ishop-core-ishop-core-projects-tag/", result.get)
  }

  @Test
  def testTransformRemoteToBuildUrl_core_version(): Unit = {
    val result = Starter.transformRemoteToBuildUrlVersion(
      Seq(GitRemote.of("origin", "ssh://someone@local-gerrit:29418/ishop/core/ishop-core-projects", "(fetch)")),
      "https://jenkins-url", "v4444.43.43")
    Assert.assertEquals("https://jenkins-url/job/ishop-core-ishop-core-projects-tag-4444/", result.get)
  }

  @Test
  def testTransformRemoteToBuildUrl_core_version_None(): Unit = {
    val origin = GitRemote.of("origin", "ssh://someone@local-gerrit:29418/ishop/core/ishop-core-projects", "(fetch)")
    val result = Starter.transformRemoteToBuildUrlVersion(Seq(origin), "https://jenkins-url", "v")
    Assert.assertEquals(None, result)
    Assert.assertEquals("origin  ssh://local-gerrit:29418/ishop/core/ishop-core-projects (fetch)", origin.toString)
  }

  @Test
  def test_nil_args(): Unit = {
    val result = doInit(Nil)
    assertMessageErr("usage: $0 \"$(dirname $0)\" \"$(pwd)\" \"${os}\" \"${TERM}\" \"${terminal_cols}\" \"${HOME}\" ${argLine}", result)
  }

  @Test
  def test_one_arg(): Unit = {
    val result = doInit(Seq("self_dir"))
    assertMessageErr("usage: $0 \"$(dirname $0)\" \"$(pwd)\" \"${os}\" \"${TERM}\" \"${terminal_cols}\" \"${HOME}\" ${argLine}", result)
  }

  private val helpMessage =
    """. done (b)
      |Usage: release [OPTION] [CMD] ...
      |Note: Calling release without any options creates a normal release.
      |All options are non-mandatory.
      |
      |Possible options:
      |--help, -h            => shows this and exits
      |--no-gerrit           => use this toggle for non gerrit projects
      |--no-interactive      => suppresses startup messages
      |--skip-property value => if you get false positives with property definitions
      |--defaults            => do not read ${HOME}/.ishop-release
      |--no-check-overlap    => skip checks for too similar names e.g. "commons" and "commoms" are too similar
      |
      |--simple-chars        => use no drawing chars
      |--no-color            => use no color
      |--no-jline            => if you have problems with terminal inputs, try this to read from Stdin
      |--replace             => replaces release jar / only required for development
      |
      |showDependencyUpdates                => shows dependency updates from nexus option
      |versionSet newVersion                => changes version like maven
      |shopGASet newGroupIdAndArtifactId    => changes GroupId and ArtifactId exclusively for Shops
      |nothing-but-create-feature-branch    => creates a feature branch and changes pom.xmls
      |apidiff                              => compares two versions of this repo
      |lint                                 => check project for release problems (read-only),
      |                                        reads environment variables CI_COMMIT_REF_NAME and CI_COMMIT_TAG
      |showSelf                             => a list of groupId:artifactId of current project
      |suggest-docker-tag                   => use with '--no-interactive', reads environment variables
      |                                        CI_COMMIT_REF_NAME and CI_COMMIT_TAG
      |
      |Possible environment variables:
      |export RELEASE_GIT_BIN=$PATH_TO_GIT_EXECUTABLE
      |export RELEASE_NO_GERRIT=true
      |
      |Your home dir is: test
      |InteractiveShell: false""".stripMargin

  @Test
  def test_help_dash(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "false", "--no-update", "--help"))
    assertMessage(helpMessage, result)
  }

  @Test
  def test_help(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "false", "--no-update", "help"))
    assertMessage(
      """. done (b)
        |Invalid options:
        |help
        |
        |""".stripMargin + helpMessage.linesIterator.drop(1).mkString("\n"), result)
  }

  @Test
  def test_help_length(): Unit = {
    assertLongLines(helpMessage, 105)
  }

  @Test
  def testAssertLongLines(): Unit = {
    val msg: String =
      """found too long lines:
        |12345
        |""".stripMargin.trim
    TestHelper.assertAssertionError(msg, classOf[AssertionError], () => {
      assertLongLines("12345\n12", 4)
    })
    assertLongLines("123", 4)
  }

  def assertLongLines(in: String, limit: Int): Unit = {
    val result = in.linesIterator.filter(l => l.length > limit).toList
    Assert.assertTrue(s"found too long lines:\n${result.mkString("\n")}", result.isEmpty)
  }

  def testRepo(originDir: File, workDir: File): File = {
    val gitOrigin = Sgit.init(originDir, verify = false)
    gitOrigin.configSetLocal("user.email", "you@example.com")
    gitOrigin.configSetLocal("user.name", "Your Name")
    val test = SgitTest.testFile(originDir, "test")
    gitOrigin.add(test)
    gitOrigin.commitAll("init")
    Assert.assertEquals(Seq(test), gitOrigin.lsFilesAbsolute())
    val git = Sgit.doClone(originDir, workDir, verify = false)
    git.configSetLocal("user.email", "you@example.com")
    git.configSetLocal("user.name", "Your Name")
    Assert.assertEquals(Seq(test.getName), git.lsFiles())
    workDir
  }

  @Test
  def testFetchGitAndAskForBranch_fail(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("c"), SgitTest.ensureAbsent("d"))

    // WHEN
    val in = TermTest.willReadFrom("master\n")
    TestHelper.assertExceptionWithCheck(in => Assert.assertEquals("E: please download a commit-message hook and retry",
      in.linesIterator.toSeq(1)),
      classOf[Sgit.MissingCommitHookException], () => {
        TermTest.withOutErrIn[Unit](in)(sys => Starter.fetchGitAndAskForBranch(sys, noVerify = true, None,
          testRepoD, Opts(), skipFetch = true, skipAskForBranchFetch = false))
      })
  }

  @Test
  def testFetchGitAndAskForBranch(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("e"), SgitTest.ensureAbsent("f"))
    SgitTest.copyMsgHook(testRepoD)
    // WHEN
    val in = TermTest.willReadFrom("master\n")
    val result = TermTest.withOutErrIn[Unit](in)(sys => Starter.fetchGitAndAskForBranch(sys,
      noVerify = SgitTest.hasCommitMsg, None, testRepoD, Opts(useJlineInput = false), skipFetch = true, skipAskForBranchFetch = false))

    // THEN
    Assert.assertEquals("Enter branch name where to start from [master]:", result.out)
    Assert.assertEquals("", result.err)
  }

  @Test
  def testFetchGitAndAskForBranch_noVerify(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("g"), SgitTest.ensureAbsent("h"))
    // WHEN
    val in = TermTest.willReadFrom("master\n")
    val result = TermTest.withOutErrIn[Unit](in)(sys => Starter.fetchGitAndAskForBranch(sys, noVerify = false,
      None, testRepoD, Opts(useJlineInput = false), skipFetch = true, skipAskForBranchFetch = false))

    // THEN
    Assert.assertEquals("Enter branch name where to start from [master]:", result.out)
    Assert.assertEquals("", result.err)
  }

  @Test
  def testArgRead_none(): Unit = {
    Assert.assertEquals(Opts(), Starter.argsAndEnvRead(Nil, Opts(), Map.empty))
  }

  @Test
  def testArgRead_invalids(): Unit = {
    Assert.assertEquals(Opts(invalids = Seq("--bert")), Starter.argsAndEnvRead(Seq("--bert"), Opts(), Map.empty))
    Assert.assertEquals(Opts(showHelp = true, invalids = Seq("some")), Starter.argsAndEnvRead(Seq("--help", "some"), Opts(), Map.empty))
    Assert.assertEquals(Opts(showHelp = true, invalids = Seq("some")), Starter.argsAndEnvRead(Seq("some", "--help"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_simpleChars(): Unit = {
    Assert.assertEquals(Opts(simpleChars = true), Starter.argsAndEnvRead(Seq("--simple-chars"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_help(): Unit = {
    Assert.assertEquals(Opts(showHelp = true), Starter.argsAndEnvRead(Seq("-h"), Opts(), Map.empty))
    Assert.assertEquals(Opts(showHelp = true), Starter.argsAndEnvRead(Seq("--help"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_updateCmd(): Unit = {
    Assert.assertEquals(Opts(showUpdateCmd = true, showStartupDone = false), Starter.argsAndEnvRead(Seq("--show-update-cmd"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_noGerrit(): Unit = {
    Assert.assertEquals(Opts(useGerrit = false), Starter.argsAndEnvRead(Seq("--no-gerrit"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_noGerrit_skip_property(): Unit = {
    Assert.assertEquals(Opts(useGerrit = false, skipProperties = Seq("a", "b")),
      Starter.argsAndEnvRead(Seq("--no-gerrit", "--skip-property", "a", "--skip-property", "b"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_skip_env(): Unit = {
    Assert.assertEquals(Opts(lintOpts = LintOpts(skips = Seq("a"))),
      Starter.argsAndEnvRead(Seq(), Opts(), envs = Map(
        "RELEASE_ANY0" -> "a",
        "RELEASE_LINT_SKIP" -> "a",
        "RELEASE_ANY1" -> "a",
      )))
  }

  @Test
  def testArgRead_noGerrit_env(): Unit = {
    val read = Starter.argsAndEnvRead(Seq("--skip-property", "a", "--skip-property", "b"), Opts(), Map("RELEASE_NO_GERRIT" -> "true"))
    val expected = Opts(useGerrit = false, skipProperties = Seq("a", "b"))
    Assert.assertEquals(Util.show(expected), Util.show(read))
    Assert.assertEquals(expected, read)
  }

  @Test
  def testArgRead_noGerrit_env_false(): Unit = {
    Assert.assertEquals(Opts(skipProperties = Seq("a", "b")),
      Starter.argsAndEnvRead(Seq("--skip-property", "a", "--skip-property", "b"), Opts(),
        Map(
          "RELEASE_NO_GERRIT" -> "false",
        )))
  }

  @Test
  def testArgRead_noJline(): Unit = {
    Assert.assertEquals(Opts(useJlineInput = false), Starter.argsAndEnvRead(Seq("--no-jline"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_replace(): Unit = {
    Assert.assertEquals(Opts(), Starter.argsAndEnvRead(Seq("--replace"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_noUpdate(): Unit = {
    Assert.assertEquals(Opts(doUpdate = false), Starter.argsAndEnvRead(Seq("--no-update"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_versionSet(): Unit = {
    Assert.assertEquals(Opts(invalids = Seq("versionSet")), Starter.argsAndEnvRead(Seq("versionSet"), Opts(), Map.empty))
    Assert.assertEquals(Opts(versionSet = Some("3")), Starter.argsAndEnvRead(Seq("versionSet", "3"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_shopGaSet(): Unit = {
    Assert.assertEquals(Opts(invalids = Seq("shopGASet")), Starter.argsAndEnvRead(Seq("shopGASet"), Opts(), Map.empty))
    Assert.assertEquals(Opts(shopGA = Some("some")), Starter.argsAndEnvRead(Seq("shopGASet", "some"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_apidiff(): Unit = {
    assertArgs(Opts(apiDiff = OptsApidiff(showApiDiff = true)),
      Starter.argsAndEnvRead(Seq("apidiff"), Opts(), Map.empty))
    assertArgs(Opts(apiDiff = OptsApidiff(showApiDiff = true, left = "a", right = "b")),
      Starter.argsAndEnvRead(Seq("apidiff", "a", "b"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_lint(): Unit = {
    assertArgs(Opts(lintOpts = LintOpts(doLint = true), showStartupDone = false),
      Starter.argsAndEnvRead(Seq("lint"), Opts(), Map.empty))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, showHelp = true), showStartupDone = false),
      Starter.argsAndEnvRead(Seq("lint", "--help"), Opts(), Map.empty))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, showHelp = true), showStartupDone = false),
      Starter.argsAndEnvRead(Seq("lint", "-h"), Opts(), Map.empty))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, skips = Seq("c", "D", "RL1012-5a4ee54d", "a")), showStartupDone = false),
      Starter.argsAndEnvRead(Seq("lint", "--skip-RL1012-5a4ee54d", "--skip-a"), Opts(), envs = Map(
        "RELEASE_LINT_SKIP" -> "c,D,, ,c,,,",
      )))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, waringsToErrors = true), showStartupDone = false),
      Starter.argsAndEnvRead(Seq("lint", "--strict"), Opts(), envs = Map(
        "RELEASE_LINT_SKIP" -> null,
      )))
  }

  @Test
  def testArgRead_createFeature(): Unit = {
    Assert.assertEquals(Opts(createFeature = true), Starter.argsAndEnvRead(Seq("nothing-but-create-feature-branch"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_showDependencyUpdates(): Unit = {
    Assert.assertEquals(Opts(depUpOpts = OptsDepUp().copy(showDependencyUpdates = true)),
      Starter.argsAndEnvRead(Seq("showDependencyUpdates"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_mixed(): Unit = {
    val opts = Opts(depUpOpts = OptsDepUp().copy(showDependencyUpdates = true, showHelp = true), useGerrit = false)
    Assert.assertEquals(opts, Starter.argsAndEnvRead(Seq("--no-gerrit", "showDependencyUpdates", "--help", "", " ", "\t"), Opts(), Map.empty))
  }

  def assertArgs(expected: Opts, current: Opts): Unit = {
    Assert.assertEquals(Util.show(expected), Util.show(current))
    Assert.assertEquals(expected, current)
  }

  @Test
  def testSuggestRebase_detached(): Unit = {
    val out = mock(classOf[PrintStream])
    val sgit = mock(classOf[Sgit])
    when(sgit.findUpstreamBranch()).thenReturn(None)
    val opts = mock(classOf[Opts])

    Starter.suggestRebase(new Term.Sys(null, out, null), sgit, branch = "test", opts)

    verify(sgit).checkout("test")
  }

  @Test
  def testCompressToGav(): Unit = {

    val dep0 = ProjectModTest.depOfUndef(groupId = "groupId", artifactId = "artifactId", version = Some(""), packaging = "jar")
    val dep1 = ProjectModTest.depOfUndef(groupId = "groupId", artifactId = "artifactId", version = Some("version${a}${b}"), packaging = "jar")
    val dep2 = ProjectModTest.depOfUndef(groupId = "groupId2", artifactId = "artifactId", version = Some("version${a}${b}"), packaging = "jar")
    val dep3 = ProjectModTest.depOfUndef(groupId = "groupId2", artifactId = "artifactId", version = Some(""))
    val dep4 = ProjectModTest.depOfUndef(groupId = "groupId2", artifactId = "artifactId", version = Some(""))
    val dep5 = ProjectModTest.depOfUndef(groupId = "groupId5", artifactId = "artifactId", version = None)
    val dep6 = ProjectModTest.depOfUndef(groupId = "groupId", artifactId = "artifactId-Self", version = Some("version"), packaging = "jar")
    val dep7 = ProjectModTest.depOfUndef(groupId = "groupId", "artifactId-test", Some("version"), scope = "test", packaging = "jar")

    val gavSelf = dep6.gav().simpleGav()
    val prop = Map("a" -> "-1.2.3")
    val out = Starter.compressToGav(Seq(gavSelf), prop)(Seq(dep0, dep1, dep2, dep3, dep4, dep5, dep6, dep7))

    val gav0 = Gav3("groupId", "artifactId", Some("version-1.2.3${b}"))
    val gav1 = Gav3("groupId2", "artifactId", Some("version-1.2.3${b}"))
    val gav3 = Gav3("groupId5", "artifactId", None)
    Assert.assertEquals(Seq(gav0, gav1, gav3), out)
  }

  @Test
  def testConnectLeftRight(): Unit = {
    val gav0 = Gav3("groupId", "artifactIdNoChange", Some("a"))
    val gavUp0 = Gav3("groupId", "artifactIdUp", Some("1"))
    val gavUp1 = Gav3("groupId", "artifactIdUp", Some("2"))
    val gavNew = Gav3("groupId", "artifactIdNew", Some("1"))
    val gavRemove = Gav3("groupId", "artifactIdRemove", Some("1"))

    val gavUpMore0 = Gav3("groupId", "artifactIdUpMore", Some("1"))
    val gavUpMore1 = Gav3("groupId", "artifactIdUpMore", Some("2"))
    val gavUpMore2 = Gav3("groupId", "artifactIdUpMore", Some("3"))
    val gavUpMore3 = Gav3("groupId", "artifactIdUpMore", Some("4"))

    val out = Starter.connectLeftRight((
      Seq(gav0, gavUp0, gavRemove, gavUpMore0, gavUpMore1),
      Seq(gav0, gavUp1, gavNew, gavUpMore2, gavUpMore3)))

    Assert.assertEquals(Seq(
      (Nil, Seq(gavNew)),
      (Seq(gavRemove), Nil),
      (Seq(gavUp0), Seq(gavUp1)),
      (Seq(gavUpMore0, gavUpMore1), Seq(gavUpMore2, gavUpMore3)),
    ), out)

  }

  @Test
  def testLogback(): Unit = {
    import scala.jdk.CollectionConverters._
    doTest(new File("src/main/resources/logback.xml"))
    doTest(new File("src/test/resources/logback-test.xml"))

    def doTest(logbackFile: File): Unit = {
      val context: LoggerContext = new LoggerContext
      val configurator: GenericConfigurator = new JoranConfigurator
      configurator.setContext(context)
      configurator.doConfigure(logbackFile)

      val sm = context.getStatusManager.asInstanceOf[BasicStatusManager]
      if (sm.getLevel != Status.INFO) {
        val statuses: Seq[Status] = sm.getCopyOfStatusList.asScala.toList
        val errorMsgs = statuses.filter(_.getLevel != Status.INFO)
        if (errorMsgs != Nil) {
          val allStatuses: String = errorMsgs.mkString("\n")
          fail(logbackFile.toString + ": has errors/warnings:\n" + allStatuses)
        }
      }
    }

  }

  @Test
  @Ignore
  def testSuggestRebase(): Unit = {
    // TODO later
    val out = mock(classOf[PrintStream])
    val in = TermTest.willReadFrom("bert")
    val sgit = mock(classOf[Sgit])
    when(sgit.isNotDetached).thenReturn(true)
    when(sgit.findUpstreamBranch()).thenReturn(None)
    when(sgit.listBranchNamesRemote()).thenReturn(Seq("blabla"), Seq("bert"))
    val opts = mock(classOf[Opts])

    Starter.suggestRebase(new Term.Sys(in, out, out), sgit, branch = "test", opts)

    verify(sgit).checkout("test")
    verify(sgit).setUpstream("bert")

    //verify(out, times(2)).println("No upstream found, please set")
    //verify(out, times(2)).println("[2] origin/test")
    //verify(out).println("W: unknown upstream branch; known are blabla")
    //verify(out, times(1)).println("Enter option or type [origin/master]: ")
    // verifyNoMoreInteractions(out)
  }

  @Test
  def testEnvRead_empty(): Unit = {
    val result = Starter.envRead(Nil, Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_some(): Unit = {
    val result = Starter.envRead(Seq(("a", "b")), Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_gerrit(): Unit = {
    val result = Starter.envRead(Seq(("RELEASE_NO_GERRIT", "true")), Opts())
    Assert.assertEquals(Opts(useGerrit = false), result)
  }

  @Test
  def testEnvRead_gerrit_false(): Unit = {
    val result = Starter.envRead(Seq(("RELEASE_NO_GERRIT", "false")), Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_gerrit_defect(): Unit = {
    val result = Starter.envRead(Seq(("RELEASE_NO_GERRIT", "fw")), Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_gerrit_others(): Unit = {
    val result = Starter.envRead(Seq(("a", "b"), ("RELEASE_NO_GERRIT", "true")), Opts())
    Assert.assertEquals(Opts(useGerrit = false), result)
  }

  @Test
  def testEnvRead_showTimeStamps(): Unit = {
    val result = Starter.envRead(Seq(("RELEASE_LINT_TIMESTAMPS", "true")), Opts())
    Assert.assertEquals(Opts(lintOpts = LintOpts(showTimeStamps = true)), result)
  }

  @Test
  def testEnvRead_showTimeStamps_fail(): Unit = {
    val result = Starter.envRead(Seq(("RELEASE_LINT_TIMESTAMPS", "some")), Opts())
    Assert.assertEquals(Opts(lintOpts = LintOpts(showTimeStamps = false)), result)
  }

  @Test
  def testEnvRead_checkPackages(): Unit = {
    val inOpts = Opts()
    Assert.assertEquals(Opts(lintOpts = LintOpts(checkPackages = true)), inOpts)
    val result = Starter.envRead(Seq(("RELEASE_LINT_CHECKPACKAGES", "false")), inOpts)
    Assert.assertEquals(Opts(lintOpts = LintOpts(checkPackages = false)), result)
  }

  @Test
  def testEnvRead_release_lint(): Unit = {
    val tuples = Seq(
      ("RELEASE_ANY0", "a"),
      ("RELEASE_LINT_SKIP", "a"),
      ("RELEASE_ANY1", "a"),
    )
    val result = Starter.envRead(tuples, Opts())
    Assert.assertEquals(Opts(lintOpts = LintOpts(skips = Seq("a"))), result)
  }

  @Test
  def testFutures(): Unit = {
    implicit val global = ExecutionContext.global

    def aString(in: String, fail: Boolean): String = {
      if (fail) {
        throw new IllegalStateException("doof")
      } else {
        in
      }
    }
    @unused
    val sF = Starter.futureOf(global, aString("one", fail = false))
    @unused
    val s1F = Starter.futureOf(global, aString("two", fail = true))

    val s: FutureEither[FutureError, Int] = new FutureEither(Future {
      Right(7)
    })

    val s2: FutureEither[FutureError, Boolean] = new FutureEither(Future {
      Right(true)
    })

    def toResult(): FutureEither[FutureError, (Int, Boolean)] = {
      val result: FutureEither[FutureError, (Int, Boolean)] = for {
        a <- s
        b <- s2
      } yield (a, b)
      result
    }

    try {
      val v: Either[FutureError, (Int, Boolean)] = Await.result(toResult().wrapped, Duration.create(10, TimeUnit.MINUTES))
      Assert.assertEquals((7, true), v.getOrElse(null))
    } catch {
      case _: TimeoutException => throw new TimeoutException("git fetch failed")
    }

  }

  def assertMessageErr(expected: String, result: ExecReturn): Unit = {
    Assert.assertEquals(1, result.exit)
    Assert.assertEquals(expected, result.err)
    Assert.assertEquals("", result.out)
  }

  def assertMessage(expected: String, result: ExecReturn): Unit = {
    Assert.assertEquals(0, result.exit)
    Assert.assertEquals("", result.err)
    Assert.assertEquals(expected, result.out.replaceFirst("Your home dir is: .*", "Your home dir is: test"))
  }

}


