package release

import java.io.{ByteArrayOutputStream, PrintStream}

import org.junit.rules.Timeout
import org.junit.{Assert, Rule, Test}
import org.scalatest.junit.AssertionsForJUnit

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
      |help/--help => shows this and exits
      |depUp       => shows dependency updates from nexus option
      |simpleChars => use no drawing chars
      |showGit     => shows all git commands for debug
      |replace     => replaces release jar / only required for development
      |noVerify    => use this toggle for non gerrit projects
      |
      |nothing-but-create-feature-branch""".stripMargin

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
