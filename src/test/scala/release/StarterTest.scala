package release

import java.io._
import java.nio.charset.StandardCharsets
import java.util.concurrent.{TimeUnit, TimeoutException}

import org.junit.rules.Timeout
import org.junit.{Assert, Rule, Test}
import org.scalatest.junit.AssertionsForJUnit
import release.Sgit.GitRemote
import release.Starter.{FutureEither, FutureError, Opts}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class StarterTest extends AssertionsForJUnit {

  val _globalTimeout = new Timeout(10000)

  @Rule def globalTimeout = _globalTimeout

  def doInit(params: Seq[String]): ExecReturn = {

    val result = StarterTest.withOutErr[Int]((out, err) ⇒ Starter.init(params, out, err))
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
      |--help, -h          => shows this and exits
      |--simple-chars      => use no drawing chars
      |--replace           => replaces release jar / only required for development
      |--no-gerrit         => use this toggle for non gerrit projects
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
        |""".stripMargin + helpMessage.lines.drop(1).mkString("\n"), result)
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
    TestHelper.assertExceptionWithCheck(in ⇒ Assert.assertEquals("E: please download a commit-message hook and retry",
      in.lines.toSeq(1)),
      classOf[Sgit.MissingCommitHookException], () ⇒ {
        StarterTest.withOutErr[Unit]((out, err) ⇒ Starter.fetchGitAndAskForBranch(out, err, noVerify = true, None, testRepoD, in, Opts()))
      })
  }

  @Test
  def testFetchGitAndAskForBranch(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("e"), SgitTest.ensureAbsent("f"))
    SgitTest.copyMsgHook(testRepoD)
    // WHEN
    val in = StarterTest.willReadFrom("master\n")
    val result = StarterTest.withOutErr[Unit]((out, err) ⇒ Starter.fetchGitAndAskForBranch(out, err,
      noVerify = SgitTest.hasCommitMsg, None, testRepoD, in, Opts()))

    // THEN
    Assert.assertEquals("Enter branch name where to start from [master]:", result.out)
    Assert.assertEquals("", result.err)
  }

  @Test
  def testFetchGitAndAskForBranch_noVerify(): Unit = {
    val testRepoD = testRepo(SgitTest.ensureAbsent("g"), SgitTest.ensureAbsent("h"))
    // WHEN
    val in = StarterTest.willReadFrom("master\n")
    val result = StarterTest.withOutErr[Unit]((out, err) ⇒ Starter.fetchGitAndAskForBranch(out, err, noVerify = false,
      None, testRepoD, in, Opts()))

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
    Assert.assertEquals(Opts(verifyGerrit = false), Starter.argsRead(Seq("--no-gerrit"), Opts()))
  }

  @Test
  def testArgRead_noGerrit_skip_property(): Unit = {
    Assert.assertEquals(Opts(verifyGerrit = false, skipProperties = Seq("a", "b")),
      Starter.argsRead(Seq("--no-gerrit", "--skip-property", "a", "--skip-property", "b"), Opts()))
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
    Assert.assertEquals(Opts(showDependencyUpdates = true), Starter.argsRead(Seq("showDependencyUpdates"), Opts()))
  }

  @Test
  def testArgRead_mixed(): Unit = {
    val opts = Opts(showDependencyUpdates = true, showHelp = true, verifyGerrit = false)
    Assert.assertEquals(opts, Starter.argsRead(Seq("showDependencyUpdates", "--help", "--no-gerrit", "", " ", "\t"), Opts()))
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
        a ← s
        b ← s2
      } yield (a, b)
      result
    }

    try {
      val v: Either[FutureError, (Int, Boolean)] = Await.result(toResult(global).wrapped, Duration.create(10, TimeUnit.MINUTES))
      Assert.assertEquals((7, true), v.right.get)
    } catch {
      case _: TimeoutException ⇒ throw new TimeoutException("git fetch failed")
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

  def withOutErr[T](fn: (PrintStream, PrintStream) ⇒ T): OutErr[T] = {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val x = fn.apply(new PrintStream(out), new PrintStream(err))
    OutErr(normalize(out), normalize(err), x)
  }

  case class OutErr[T](out: String, err: String, value: T)

}
