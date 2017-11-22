package release

import java.io.{ByteArrayOutputStream, PrintStream}

import org.junit.rules.Timeout
import org.junit.{Assert, Rule, Test}
import org.scalatest.junit.AssertionsForJUnit
import redis.clients.jedis.Jedis
import release.Sgit.GitRemote

class StarterTest extends AssertionsForJUnit {

  val _globalTimeout = new Timeout(10000)

  @Rule def globalTimeout = _globalTimeout

  def doInit(params: Seq[String]): ExecReturn = {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val exit = Starter.init(params, new PrintStream(out), new PrintStream(err))

    def normalize(in: ByteArrayOutputStream) = in.toString.trim.replaceAll("\r\n", "\n")

    ExecReturn(normalize(out), normalize(err), exit)
  }

  case class ExecReturn(out: String, err: String, exit: Int)

  @Test
  def testTransformRemoteToBuildUrl_paypal(): Unit = {
    val result = Starter.transformRemoteToBuildUrl(
      Seq(GitRemote("origin", "ssh://someone@git-ishop.novomind.com:19418/ishop/ext/ext-paypal.git", "remoteType")),
      "https://build-ishop.novomind.com")
    Assert.assertEquals("https://build-ishop.novomind.com/job/ishop-ext-ext-paypal-tag/", result.get)
  }

  @Test
  def testTransformRemoteToBuildUrl_core(): Unit = {
    val result = Starter.transformRemoteToBuildUrl(
      Seq(GitRemote("origin", "ssh://someone@git-ishop.novomind.com:19418/ishop/core/ishop-core-projects", "(fetch)")),
      "https://build-ishop.novomind.com")
    Assert.assertEquals("https://build-ishop.novomind.com/job/ishop-core-ishop-core-projects-tag/", result.get)
  }

  @Test
  def testJedis(): Unit = {
    try {
      val jedis = new Jedis("deko-ishop.novomind.com", 50500, 2000, 2000)
      val pass ="j348u3nhgij2njfu8§$3g§$F"
     // jedis.configSet("requirepass", pass)
      jedis.auth(pass)
      def push(key:String, value:String): Unit = {
        jedis.rpush(key, value)
        jedis.expire(key, 2)
      }

      push("foor", "hallo")
      val long = jedis.llen("foor")
      val value = jedis.lrange("foor", 0, long)

      println(value)
    } catch {
      case e: Throwable ⇒ println("E: " + e.getMessage)
    }
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
    """Possible args:
      |help/--help      => shows this and exits
      |depUp            => shows dependency updates from nexus option
      |simpleChars      => use no drawing chars
      |showGit          => shows all git commands for debug
      |replace          => replaces release jar / only required for development
      |noVerify         => use this toggle for non gerrit projects
      |jenkinsTrigger   => beta: jenkins trigger for builds
      |
      |versionSet newVersion                => changes version like maven
      |shopGASet newGroupIdAndAtifactId     => changes GroupId and ArtifactId for Shops
      |nothing-but-create-feature-branch    => creates a feature branch and changes pom.xmls
      |
      |Possible environment variables:
      |export RELEASE_GIT_BIN = $PATH_TO_GIT_BIN""".stripMargin

  @Test
  def test_help(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "no-update", "help"))
    assertMessage(helpMessage, result)
  }

  @Test
  def test_help_dash(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "no-update", "--help"))
    assertMessage(helpMessage, result)
  }

  def assertMessageErr(expected: String, result: ExecReturn): Unit = {
    Assert.assertEquals(1, result.exit)
    Assert.assertEquals(expected, result.err)
    Assert.assertEquals("", result.out)
  }

  def assertMessage(expected: String, result: ExecReturn): Unit = {
    Assert.assertEquals(0, result.exit)
    Assert.assertEquals("", result.err)
    Assert.assertEquals(expected, result.out)
  }

}
