package release

import java.io.{ByteArrayOutputStream, PrintStream}

import org.junit.rules.Timeout
import org.junit.{Assert, Rule, Test}
import org.scalatest.junit.AssertionsForJUnit
import redis.clients.jedis.Jedis

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
  def testJedis(): Unit = {
    try {
      val jedis = new Jedis("localhost", 6379, 1, 1)
      jedis.lpush("foor", "hello")
      val long = jedis.llen("foor")
      val value = jedis.lrange("foor", 0, long)
      jedis.expire("foor", 1)
      println(value)
    } catch {
      case e: Throwable ⇒ println(e.getMessage)
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
      |nothing-but-create-feature-branch    => creates a feature branch and changes pom.xmls""".stripMargin

  @Test
  def test_help(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "noUpdate", "help"))
    assertMessage(helpMessage, result)
  }

  @Test
  def test_help_dash(): Unit = {
    val result = doInit(Seq("self_dir", "workdir", "Cygwin", "cygwin", "80", "noUpdate", "--help"))
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
