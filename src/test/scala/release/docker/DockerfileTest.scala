package release.docker

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.{OneTimeSwitch, Opts, TermTest, TestHelper}
import release.docker.SuggestDockerTag

import scala.util.Success

class DockerfileTest extends AssertionsForJUnit {

  @Test
  def testParseLines_blank(): Unit = {
    TermTest.testSys(Nil, "", "")(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val warn = new OneTimeSwitch()
      val error = new OneTimeSwitch()
      val codes = Dockerfile.parseLines(
        """
          |""".stripMargin.linesIterator.toSeq, allowedFromHosts = "", sys.out, opts, warn, error)
      Assert.assertFalse(warn.isTriggered())
      Assert.assertFalse(error.isTriggered())
      Assert.assertEquals(Nil, codes)
    })
  }

  @Test
  def testParseLines_from(): Unit = {
    val xOut =
      """[INFO]           ✅ FROM  eclipse-temurin:11-jre-ubi9-minimal
        |[INFO]           ✅ FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal
        |[INFO]           ✅ FROM  example.org/eclipse-temurin:11-jre-ubi9-minimal""".stripMargin
    TermTest.testSys(Nil, expectedOut = xOut, "")(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val warn = new OneTimeSwitch()
      val error = new OneTimeSwitch()
      val codes = Dockerfile.parseLines(
        """FROM  eclipse-temurin:11-jre-ubi9-minimal
          |FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal
          |FROM  example.org/eclipse-temurin:11-jre-ubi9-minimal
          |""".stripMargin.linesIterator.toSeq, allowedFromHosts = "", sys.out, opts, warn, error)
      Assert.assertFalse(warn.isTriggered())
      Assert.assertFalse(error.isTriggered())
      Assert.assertEquals(Nil, codes)
    })
  }

  @Test
  def testParseLines_from_wildcard(): Unit = {
    val xOut =
      """[INFO]           ✅ FROM  eclipse-temurin:11-jre-ubi9-minimal
        |[INFO]           ✅ FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal
        |[INFO]           ✅ FROM  example.org/eclipse-temurin:11-jre-ubi9-minimal""".stripMargin
    TermTest.testSys(Nil, expectedOut = xOut, "")(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val warn = new OneTimeSwitch()
      val error = new OneTimeSwitch()
      val codes = Dockerfile.parseLines(
        """FROM  eclipse-temurin:11-jre-ubi9-minimal
          |FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal
          |FROM  example.org/eclipse-temurin:11-jre-ubi9-minimal
          |""".stripMargin.linesIterator.toSeq, allowedFromHosts = ".*", sys.out, opts, warn, error)
      Assert.assertFalse(warn.isTriggered())
      Assert.assertFalse(error.isTriggered())
      Assert.assertEquals(Nil, codes)
    })
  }

  @Test
  def testParseLines_from_docker_io(): Unit = {
    val xOut =
      """[INFO]           ✅ FROM  eclipse-temurin:11-jre-ubi9-minimal
        |[INFO]           ✅ FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal
        |[WARNING]        😬 FROM  example.org/eclipse-temurin:11-jre-ubi9-minimal RL1022-e43dc8fb""".stripMargin
    TermTest.testSys(Nil, expectedOut = xOut, "")(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val warn = new OneTimeSwitch()
      val error = new OneTimeSwitch()
      val codes = Dockerfile.parseLines(
        """FROM  eclipse-temurin:11-jre-ubi9-minimal
          |FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal
          |FROM  example.org/eclipse-temurin:11-jre-ubi9-minimal
          |""".stripMargin.linesIterator.toSeq, allowedFromHosts = "docker.io", sys.out, opts, warn, error)
      Assert.assertTrue(warn.isTriggered())
      Assert.assertFalse(error.isTriggered())
      Assert.assertEquals(Nil, codes)
    })
  }

  @Test
  def testParseLines_from_selective(): Unit = {
    val xOut =
      """[warning]        🤐 FROM  eclipse-temurin:11-jre-ubi9-minimal RL1022-32815956
        |[WARNING]        😬 FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal RL1022-00a83b68
        |[INFO]           ✅ FROM example.org/eclipse-temurin:11-jre-ubi9-minimal
        |[INFO]           ✅ FROM   example.com/eclipse-temurin:11-jre-ubi9-minimal
        |[WARNING]        😬 FROM  example.net/eclipse-temurin:11-jre-ubi9-minimal RL1022-dd45f0a5""".stripMargin
    TermTest.testSys(Nil, expectedOut = xOut, "")(sys => {
      val skipCodes = Seq("RL1022-32815956")
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = skipCodes))

      val warn = new OneTimeSwitch()
      val error = new OneTimeSwitch()
      val codes = Dockerfile.parseLines(
        """FROM  eclipse-temurin:11-jre-ubi9-minimal
          |FROM  docker.io/eclipse-temurin:11-jre-ubi9-minimal
          |FROM example.org/eclipse-temurin:11-jre-ubi9-minimal
          |FROM   example.com/eclipse-temurin:11-jre-ubi9-minimal
          |FROM  example.net/eclipse-temurin:11-jre-ubi9-minimal
          |""".stripMargin.linesIterator.toSeq, allowedFromHosts = "example.org, example.com", sys.out, opts, warn, error)
      Assert.assertTrue(warn.isTriggered())
      Assert.assertFalse(error.isTriggered())
      Assert.assertEquals(skipCodes, codes)
    })
  }
}


