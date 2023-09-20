package release

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.BasicStatusManager
import ch.qos.logback.core.joran.GenericConfigurator
import ch.qos.logback.core.status.Status
import com.typesafe.scalalogging.LazyLogging
import org.junit.rules.Timeout
import org.junit.{Assert, Ignore, Rule, Test}
import org.mockito.MockitoSugar
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.Gav3
import release.Sgit.GitRemote
import release.Starter.{FutureEither, FutureError, LintOpts, Opts, OptsDepUp}

import java.io._
import java.util.concurrent.{TimeUnit, TimeoutException}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class StarterTest extends AssertionsForJUnit with MockitoSugar with LazyLogging {

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
      Seq(GitRemote("origin", "ssh://someone@local-gerrit:29418/ishop/ext/ext-paypal.git", "remoteType")),
      "https://jenkins-url")
    Assert.assertEquals("https://jenkins-url/job/ishop-ext-ext-paypal-tag/", result.get)
  }

  @Test
  def testTransformRemoteToBuildUrl_core(): Unit = {
    val result = Starter.transformRemoteToBuildUrl(
      Seq(GitRemote("origin", "ssh://someone@local-gerrit:29418/ishop/core/ishop-core-projects", "(fetch)")),
      "https://jenkins-url")
    Assert.assertEquals("https://jenkins-url/job/ishop-core-ishop-core-projects-tag/", result.get)
  }

  @Test
  def testTransformRemoteToBuildUrl_core_version(): Unit = {
    val result = Starter.transformRemoteToBuildUrlVersion(
      Seq(GitRemote("origin", "ssh://someone@local-gerrit:29418/ishop/core/ishop-core-projects", "(fetch)")),
      "https://jenkins-url", "v4444.43.43")
    Assert.assertEquals("https://jenkins-url/job/ishop-core-ishop-core-projects-tag-4444/", result.get)
  }

  @Test
  def testTransformRemoteToBuildUrl_core_version_None(): Unit = {
    val result = Starter.transformRemoteToBuildUrlVersion(
      Seq(GitRemote("origin", "ssh://someone@local-gerrit:29418/ishop/core/ishop-core-projects", "(fetch)")),
      "https://jenkins-url", "v")
    Assert.assertEquals(None, result)
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
    Assert.assertEquals(Opts(), Starter.argsRead(Nil, Opts()))
  }

  @Test
  def testArgRead_invalids(): Unit = {
    Assert.assertEquals(Opts(invalids = Seq("--bert")), Starter.argsRead(Seq("--bert"), Opts()))
    Assert.assertEquals(Opts(showHelp = true, invalids = Seq("some")), Starter.argsRead(Seq("--help", "some"), Opts()))
    Assert.assertEquals(Opts(showHelp = true, invalids = Seq("some")), Starter.argsRead(Seq("some", "--help"), Opts()))
  }

  @Test
  def testArgRead_simpleChars(): Unit = {
    Assert.assertEquals(Opts(simpleChars = true), Starter.argsRead(Seq("--simple-chars"), Opts()))
  }

  @Test
  def testArgRead_help(): Unit = {
    Assert.assertEquals(Opts(showHelp = true), Starter.argsRead(Seq("-h"), Opts()))
    Assert.assertEquals(Opts(showHelp = true), Starter.argsRead(Seq("--help"), Opts()))
  }

  @Test
  def testArgRead_updateCmd(): Unit = {
    Assert.assertEquals(Opts(showUpdateCmd = true, showStartupDone = false), Starter.argsRead(Seq("--show-update-cmd"), Opts()))
  }

  @Test
  def testArgRead_noGerrit(): Unit = {
    Assert.assertEquals(Opts(useGerrit = false), Starter.argsRead(Seq("--no-gerrit"), Opts()))
  }

  @Test
  def testArgRead_noGerrit_skip_property(): Unit = {
    Assert.assertEquals(Opts(useGerrit = false, skipProperties = Seq("a", "b")),
      Starter.argsRead(Seq("--no-gerrit", "--skip-property", "a", "--skip-property", "b"), Opts()))
  }

  @Test
  def testArgRead_noGerrit_env(): Unit = {
    val read = Starter.argsRead(Seq("--skip-property", "a", "--skip-property", "b"), Opts(), Map("RELEASE_NO_GERRIT" -> "true"))
    val expected = Opts(useGerrit = false, skipProperties = Seq("a", "b"))
    Assert.assertEquals(Util.show(expected), Util.show(read))
    Assert.assertEquals(expected, read)
  }

  @Test
  def testArgRead_noGerrit_env_false(): Unit = {
    Assert.assertEquals(Opts(skipProperties = Seq("a", "b")),
      Starter.argsRead(Seq("--skip-property", "a", "--skip-property", "b"), Opts(),
        Map(
          "RELEASE_NO_GERRIT" -> "false",
        )))
  }

  @Test
  def testArgRead_noJline(): Unit = {
    Assert.assertEquals(Opts(useJlineInput = false), Starter.argsRead(Seq("--no-jline"), Opts()))
  }

  @Test
  def testArgRead_replace(): Unit = {
    Assert.assertEquals(Opts(), Starter.argsRead(Seq("--replace"), Opts()))
  }

  @Test
  def testArgRead_noUpdate(): Unit = {
    Assert.assertEquals(Opts(doUpdate = false), Starter.argsRead(Seq("--no-update"), Opts()))
  }

  @Test
  def testArgRead_versionSet(): Unit = {
    Assert.assertEquals(Opts(invalids = Seq("versionSet")), Starter.argsRead(Seq("versionSet"), Opts()))
    Assert.assertEquals(Opts(versionSet = Some("3")), Starter.argsRead(Seq("versionSet", "3"), Opts()))
  }

  @Test
  def testArgRead_shopGaSet(): Unit = {
    Assert.assertEquals(Opts(invalids = Seq("shopGASet")), Starter.argsRead(Seq("shopGASet"), Opts()))
    Assert.assertEquals(Opts(shopGA = Some("some")), Starter.argsRead(Seq("shopGASet", "some"), Opts()))
  }

  @Test
  def testArgRead_lint(): Unit = {
    assertArgs(Opts(lintOpts = LintOpts(doLint = true), showStartupDone = false),
      Starter.argsRead(Seq("lint"), Opts()))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, showHelp = true), showStartupDone = false),
      Starter.argsRead(Seq("lint", "--help"), Opts()))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, showHelp = true), showStartupDone = false),
      Starter.argsRead(Seq("lint", "-h"), Opts()))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, skips = Seq( "c", "D", "RL1012-5a4ee54d", "a")), showStartupDone = false),
      Starter.argsRead(Seq("lint", "--skip-RL1012-5a4ee54d", "--skip-a"), Opts(), envs = Map(
        "RELEASE_LINT_SKIP" -> "c,D,, ,c,,,",
      )))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, waringsToErrors = true), showStartupDone = false),
      Starter.argsRead(Seq("lint", "--strict"), Opts(), envs = Map(
        "RELEASE_LINT_SKIP" -> null,
      )))
  }

  @Test
  def testArgRead_createFeature(): Unit = {
    Assert.assertEquals(Opts(createFeature = true), Starter.argsRead(Seq("nothing-but-create-feature-branch"), Opts()))
  }

  @Test
  def testArgRead_showDependencyUpdates(): Unit = {
    Assert.assertEquals(Opts(depUpOpts = OptsDepUp().copy(showDependencyUpdates = true)),
      Starter.argsRead(Seq("showDependencyUpdates"), Opts()))
  }

  @Test
  def testArgRead_mixed(): Unit = {
    val opts = Opts(depUpOpts = OptsDepUp().copy(showDependencyUpdates = true, showHelp = true), useGerrit = false)
    Assert.assertEquals(opts, Starter.argsRead(Seq("--no-gerrit", "showDependencyUpdates", "--help", "", " ", "\t"), Opts()))
  }

  def assertArgs(expected: Opts, current: Opts): Unit = {
    Assert.assertEquals(Util.show(expected), Util.show(current))
    Assert.assertEquals(expected, current)
  }

  @Test
  def testSuggestRebase_detached(): Unit = {
    val out = mock[PrintStream]
    val sgit = mock[Sgit]
    when(sgit.findUpstreamBranch()).thenReturn(None)
    val opts = mock[Opts]

    Starter.suggestRebase(new Term.Sys(null, out, null), sgit, branch = "test", opts)

    verify(sgit).checkout("test")
  }

  @Test
  def testCompressToGav(): Unit = {

    val dep0 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId", "artifactId", Some(""), "", "", "jar", "")
    val dep1 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId", "artifactId", Some("version${a}${b}"), "", "", "jar", "")
    val dep2 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId2", "artifactId", Some("version${a}${b}"), "", "", "jar", "")
    val dep3 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId2", "artifactId", Some(""), "", "", "", "")
    val dep4 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId2", "artifactId", Some(""), "", "", "", "")
    val dep5 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId5", "artifactId", None, "", "", "", "")
    val dep6 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId", "artifactId-Self", Some("version"), "", "", "jar", "")
    val dep7 = ProjectMod.Dep(ProjectMod.SelfRef.undef, "groupId", "artifactId-test", Some("version"), "", "test", "jar", "")

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
    doTest(new File("src/main/resources/logback.xml"))
    doTest(new File("src/test/resources/logback-test.xml"))

    def doTest(logbackFile: File): Unit = {
      val context: LoggerContext = new LoggerContext
      val configurator: GenericConfigurator = new JoranConfigurator
      configurator.setContext(context)
      configurator.doConfigure(logbackFile)

      val sm = context.getStatusManager.asInstanceOf[BasicStatusManager]
      if (sm.getLevel != Status.INFO) {
        val statuses: Seq[Status] = Util.toSeq(sm.getCopyOfStatusList)
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
    val out = mock[PrintStream]
    val in = TermTest.willReadFrom("bert")
    val sgit = mock[Sgit]
    when(sgit.isNotDetached).thenReturn(true)
    when(sgit.findUpstreamBranch()).thenReturn(None)
    when(sgit.listBranchNamesRemote()).thenReturn(Seq("blabla"), Seq("bert"))
    val opts = mock[Opts]

    Starter.suggestRebase(new Term.Sys(in, out, out), sgit, branch = "test", opts)

    val oi = inOrder(out)
    oi.verify(out).println("No upstream found, please set")
    oi.verify(out).println("a")
    oi.verify(out).println("[1] origin/master")
    oi.verify(out).println("[2] origin/test")

    verify(sgit).checkout("test")
    verify(sgit).setUpstream("bert")

    //verify(out, times(2)).println("No upstream found, please set")
    //verify(out, times(2)).println("[2] origin/test")
    //verify(out).println("W: unknown upstream branch; known are blabla")
    //verify(out, times(1)).println("Enter option or type [origin/master]: ")
    // verifyNoMoreInteractions(out)
  }

  @Test
  def inOrder(): Unit = {
    val out = mock[PrintStream]
    out.println("a")
    out.println("b")
    out.println("a")
    val inO = inOrder(out)
    inO.verify(out).println("a")
    inO.verify(out).println("b")
    inO.verify(out).println("a")
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
  def testFutures(): Unit = {
    implicit val global = ExecutionContext.global

    def aString(in: String, fail: Boolean): String = {
      if (fail) {
        throw new IllegalStateException("doof")
      } else {
        in
      }
    }

    val sF = Starter.futureOf(global, aString("one", false))
    val s1F = Starter.futureOf(global, aString("two", true))

    val s: FutureEither[FutureError, Int] = new FutureEither(Future {
      Right(7)
    })

    val s2: FutureEither[FutureError, Boolean] = new FutureEither(Future {
      Right(true)
    })

    def toResult(implicit ec: ExecutionContext): FutureEither[FutureError, (Int, Boolean)] = {
      val result: FutureEither[FutureError, (Int, Boolean)] = for {
        a <- s
        b <- s2
      } yield (a, b)
      result
    }

    try {
      val v: Either[FutureError, (Int, Boolean)] = Await.result(toResult(global).wrapped, Duration.create(10, TimeUnit.MINUTES))
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


