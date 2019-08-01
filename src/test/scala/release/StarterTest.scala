package release

import java.io._
import java.nio.charset.StandardCharsets
import java.util.concurrent.{TimeUnit, TimeoutException}

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
import release.Sgit.GitRemote
import release.Starter.{FutureEither, FutureError, Opts, OptsDepUp}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class StarterTest extends AssertionsForJUnit with MockitoSugar with LazyLogging {

  val _globalTimeout = new Timeout(10000)

  @Rule def globalTimeout = _globalTimeout

  def doInit(params: Seq[String]): ExecReturn = {

    val result = StarterTest.withOutErr[Int]((out, err) => Starter.init(params, out, err))
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
  def test_nil_args(): Unit = {
    val result = doInit(Nil)
    assertMessageErr("usage: $0 \"$(dirname $0)\" \"$(pwd)\" \"${os}\" \"${TERM}\" \"${terminal_cols}\" ${argLine}", result)
  }

  @Test
  def test_one_arg(): Unit = {
    val result = doInit(Seq("self_dir"))
    assertMessageErr("usage: $0 \"$(dirname $0)\" \"$(pwd)\" \"${os}\" \"${TERM}\" \"${terminal_cols}\" ${argLine}", result)
  }

  private val helpMessage =
    """. done
      |Usage: release [OPTION] [CMD] ...
      |Note: Calling release without any options creates a normal release.
      |All options are non-mandatory.
      |
      |Possible options:
      |--help, -h            => shows this and exits
      |--no-gerrit           => use this toggle for non gerrit projects
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
      |shopGASet newGroupIdAndArtifactId    => changes GroupId and ArtifactId for Shops
      |nothing-but-create-feature-branch    => creates a feature branch and changes pom.xmls
      |
      |Possible environment variables:
      |export RELEASE_GIT_BIN = $PATH_TO_GIT_BIN
      |
      |Your home dir is: test""".stripMargin

  @Test
  def test_help_dash(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "--no-update", "--help"))
    assertMessage(helpMessage, result)
  }

  @Test
  def test_help(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "--no-update", "help"))
    assertMessage(
      """. done
        |Invalid options:
        |help
        |
        |""".stripMargin + helpMessage.linesIterator.drop(1).mkString("\n"), result)
  }

  def testRepo(originDir: File, workDir: File): File = {
    val gitOrigin = Sgit.init(originDir, verify = false)
    gitOrigin.add(SgitTest.testFile(originDir, "test"))
    gitOrigin.commitAll("init")
    val git = Sgit.clone(originDir, workDir, verify = false)
    Assert.assertEquals(Seq("test"), git.lsFiles())
    workDir
  }

  @Test
  def testFetchGitAndAskForBranch_fail(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("c"), SgitTest.ensureAbsent("d"))

    // WHEN
    val in = StarterTest.willReadFrom("master\n")
    TestHelper.assertExceptionWithCheck(in => Assert.assertEquals("E: please download a commit-message hook and retry",
      in.linesIterator.toSeq(1)),
      classOf[Sgit.MissingCommitHookException], () => {
        StarterTest.withOutErr[Unit]((out, err) => Starter.fetchGitAndAskForBranch(out, err, noVerify = true, None, testRepoD, in, Opts()))
      })
  }

  @Test
  def testFetchGitAndAskForBranch(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("e"), SgitTest.ensureAbsent("f"))
    SgitTest.copyMsgHook(testRepoD)
    // WHEN
    val in = StarterTest.willReadFrom("master\n")
    val result = StarterTest.withOutErr[Unit]((out, err) => Starter.fetchGitAndAskForBranch(out, err,
      noVerify = SgitTest.hasCommitMsg, None, testRepoD, in, Opts(useJlineInput = false)))

    // THEN
    Assert.assertEquals("Enter branch name where to start from [master]:", result.out)
    Assert.assertEquals("", result.err)
  }

  @Test
  def testFetchGitAndAskForBranch_noVerify(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("g"), SgitTest.ensureAbsent("h"))
    // WHEN
    val in = StarterTest.willReadFrom("master\n")
    val result = StarterTest.withOutErr[Unit]((out, err) => Starter.fetchGitAndAskForBranch(out, err, noVerify = false,
      None, testRepoD, in, Opts(useJlineInput = false)))

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
    Assert.assertEquals(Opts(showUpdateCmd = true), Starter.argsRead(Seq("--show-update-cmd"), Opts()))
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

  @Test
  def testSuggestRebase_detached(): Unit = {
    val out = mock[PrintStream]
    val sgit = mock[Sgit]
    when(sgit.findUpstreamBranch()).thenReturn(None)
    val opts = mock[Opts]

    Starter.suggestRebase(out, sgit, branch = "test", opts)

    verify(sgit).checkout("test")
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
          fail(logbackFile + ": has errors/warnings:\n" + allStatuses)
        }
      }
    }

  }

  @Test
  @Ignore
  def testSuggestRebase(): Unit = {
    // TODO later
    val out = mock[PrintStream]
    val in = mock[BufferedReader]
    when(in.readLine()).thenReturn("bert")
    val sgit = mock[Sgit]
    when(sgit.isNotDetached).thenReturn(true)
    when(sgit.findUpstreamBranch()).thenReturn(None)
    when(sgit.listBranchNamesRemote()).thenReturn(Seq("blabla"), Seq("bert"))
    val opts = mock[Opts]

    Starter.suggestRebase(out, sgit, branch = "test", opts, in)

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
  def testFutures(): Unit = {
    implicit val global = ExecutionContext.global

    def aString(in: String, fail: Boolean)(): String = {
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

object StarterTest {

  def willReadFrom(in: String): BufferedReader = {
    val stream = new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8))
    new BufferedReader(new InputStreamReader(stream))
  }

  def normalize(in: ByteArrayOutputStream): String = in.toString.trim.replaceAll("\r\n", "\n")

  def withOutErr[T](fn: (PrintStream, PrintStream) => T): OutErr[T] = {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val x = fn.apply(new PrintStream(out), new PrintStream(err))
    OutErr(normalize(out), normalize(err), x)
  }

  case class OutErr[T](out: String, err: String, value: T)

}
