package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

import java.time.ZonedDateTime

class SgitParserTest extends AssertionsForJUnit {

  @Test
  def testUnescape(): Unit = {
    Assert.assertEquals("On Windows try JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8 sbt assembly",
      "UTF-8", System.getProperty("file.encoding"))
    Assert.assertEquals("test", SgitParsers.unescape("test"))
    Assert.assertEquals("Präsentation", SgitParsers.unescape("Pr\\303\\244sentation"))
    Assert.assertEquals("\uA64A", SgitParsers.unescape("\\352\\231\\212"))
    Assert.assertEquals("\uD802\uDD25", SgitParsers.unescape("\\360\\220\\244\\245"))
  }

  @Test
  def testParseSimplyfiedLogLine(): Unit = {
    Assert.assertEquals(Nil, SgitParsers.LogParser.doParseSimple(""))
    val expected = Seq(
      SlogLine(
        branchNames = Seq("HEAD -> develop-1x", "origin/develop-1x", "main-1x"),
        tagNames = Seq("v1.3.0"),
        sha1 = "c042a9e533bf80ae1f8c887189a502a2fd23c2d2",
        date = ZonedDateTime.parse("2024-02-13T08:19:20+01:00")),
      SlogLine(
        branchNames = Nil,
        tagNames = Nil,
        sha1 = "a9fb1a52a869761c5fc61c78fc49fa8b3c8f40c9",
        date = ZonedDateTime.parse("2024-02-13T08:19:20+01:00")),
      SlogLine(
        branchNames = Nil,
        tagNames = Nil,
        sha1 = "545d1870278f60655d4d3ea3a03041e87b6fab82",
        date = ZonedDateTime.parse("2011-07-28T12:07:10Z")),
      SlogLine(
        branchNames = Seq("origin/master", "origin/HEAD"),
        tagNames = Nil,
        sha1 = "486e8e4672c24ea8b5d6e1b922112e783800a6ca",
        date = ZonedDateTime.parse("2024-06-17T15:05:31+02:00")),
    )
    val in =
      """
        | (HEAD -> develop-1x, origin/develop-1x, tag: v1.3.0, main-1x) c042a9e533bf80ae1f8c887189a502a2fd23c2d2 2024-02-13T08:19:20+01:00
        |a9fb1a52a869761c5fc61c78fc49fa8b3c8f40c9 2024-02-13T08:19:20+01:00
        |545d1870278f60655d4d3ea3a03041e87b6fab82 2011-07-28T12:07:10Z
        |' (origin/master, origin/HEAD) 486e8e4672c24ea8b5d6e1b922112e783800a6ca 2024-06-17T15:05:31+02:00'
        |""".stripMargin
    Assert.assertEquals(expected, SgitParsers.LogParser.doParseSimple(in))
  }

  @Test
  def testParseSimplyfiedLogLineNoRefs(): Unit = {
    val expected = Seq(
      SlogLine(
        branchNames = Nil,
        tagNames = Nil,
        sha1 = "a9fb1a52a869761c5fc61c78fc49fa8b3c8f40c9",
        date = ZonedDateTime.parse("2024-02-13T08:19:20+01:00")),
    )
    val in =
      """
        |a9fb1a52a869761c5fc61c78fc49fa8b3c8f40c9 2024-02-13T08:19:20+01:00
        |""".stripMargin
    Assert.assertEquals(expected, SgitParsers.LogParser.doParseSimple(in.trim))
  }

}
