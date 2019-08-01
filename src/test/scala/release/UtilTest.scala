package release

import java.io.File

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

class UtilTest extends AssertionsForJUnit {

  @Test
  def testSymmetricDiff(): Unit = {
    Assert.assertEquals(Nil, Util.symmetricDiff(Seq("a"), Seq("a")))
    Assert.assertEquals(Seq(("a", "b")), Util.symmetricDiff(Seq(("a", "b")), Nil))
    val tuples: Seq[(String, String)] = Seq(("a", "b"), ("a", "b"))
    val distinct = tuples.distinct
    Assert.assertEquals(Seq(("a", "b")), distinct)
    Assert.assertEquals(distinct, Util.symmetricDiff(tuples, distinct))
  }

  @Test
  def testUb(): Unit = {

    Assert.assertEquals(Map.empty, Util.groupedFiltered(Nil))
    Assert.assertEquals(Map(
      "a" -> Seq("b", "c"),
      "b" -> Seq("a", "c"),
      "c" -> Seq("a", "b")),
      Util.groupedFiltered(Seq("a", "b", "c")))
  }

  @Test
  def testReadLines(): Unit = {
    TestHelper.assertExceptionWithCheck(msg =>
      Assert.assertEquals("... is no regular file", msg.replaceFirst("^[^ ]+ ", "... ")), classOf[IllegalStateException],
      () => Util.readLines(new File("fasdf")))

    Assert.assertNotEquals(Nil, Util.readLines(new File(".gitattributes")))
  }

  @Test
  def testEmptyToNone(): Unit = {
    Assert.assertEquals(None, Util.emptyToNone(""))
    Assert.assertEquals(None, Util.emptyToNone(" "))
    Assert.assertEquals(None, Util.emptyToNone("\t"))
    Assert.assertEquals(Some("\ta"), Util.emptyToNone("\ta"))
  }

  @Test
  def testPluralize(): Unit = {
    import Util.pluralize
    Assert.assertEquals("value", "value".pluralize(0))
    Assert.assertEquals("values", "value".pluralize(10))
  }

  @Test
  def testBlank(): Unit = {
    import Util.pluralize
    Assert.assertFalse("value".blank())
    Assert.assertTrue(" ".blank())
    Assert.assertTrue(" \t".blank())
    Assert.assertTrue(" \t\r".blank())
    Assert.assertTrue(" \t\n".blank())
    Assert.assertTrue(" \t\r\n".blank())
  }

  @Test
  def testDistinctOn(): Unit = {
    Assert.assertEquals(Seq("ab", "c", "abc"), Util.distinctOn[String, Int](Seq("ab", "ba", "c", "abc"), in => in.length))
  }

  @Test
  def testOnlyOpt(): Unit = {
    TestHelper.assertException(classOf[IllegalArgumentException], () => Util.onlyOpt(Seq(1, 2)))
    Assert.assertEquals(Some(1), Util.onlyOpt(Seq(1)))
    Assert.assertEquals(None, Util.onlyOpt(Nil))
  }

  @Test
  def testMd5(): Unit = {
    Assert.assertEquals("3de0746a7d2762a87add40dac2bc95a0", Util.hashMd5("bert"))
  }

  @Test
  def testMd5Random(): Unit = {
    Assert.assertEquals(32, Util.hashMd5Random().length)
  }

  @Test
  def testSha1(): Unit = {
    Assert.assertEquals("7b964cd933b2cc9106deabd4641111826cfbc094", Util.hashSha1("bert"))
  }

  @Test
  def testNullToEmpty(): Unit = {
    Assert.assertEquals("bert", Util.nullToEmpty("bert"))
    Assert.assertEquals("", Util.nullToEmpty(""))
    Assert.assertEquals("", Util.nullToEmpty(null))
  }

  @Test
  def testIsNullOrEmpty(): Unit = {
    Assert.assertFalse(Util.isNullOrEmpty("bert"))
    Assert.assertTrue(Util.isNullOrEmpty(""))
    Assert.assertTrue(Util.isNullOrEmpty(null))
  }
}
