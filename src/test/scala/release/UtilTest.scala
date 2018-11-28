package release

import java.io.File

import org.junit.{Assert, Test}
import org.scalatest.junit.AssertionsForJUnit

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
      "a" → Seq("b", "c"),
      "b" → Seq("a", "c"),
      "c" → Seq("a", "b")),
      Util.groupedFiltered(Seq("a", "b", "c")))
  }

  @Test
  def testReadLines(): Unit = {
    TestHelper.assertExceptionWithCheck(msg ⇒
      Assert.assertEquals("... is no regular file", msg.replaceFirst("^[^ ]+ ", "... ")), classOf[IllegalStateException],
      () ⇒ Util.readLines(new File("fasdf")))

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
  def testDistinctOn(): Unit = {
    Assert.assertEquals(Seq("ab", "c", "abc"), Util.distinctOn[String, Int](Seq("ab", "ba", "c", "abc"), in ⇒ in.length))
  }

  @Test
  def testOnlyOpt(): Unit = {
    TestHelper.assertException(classOf[IllegalArgumentException], () ⇒ Util.onlyOpt(Seq(1, 2)))
    Assert.assertEquals(Some(1), Util.onlyOpt(Seq(1)))
    Assert.assertEquals(None, Util.onlyOpt(Nil))
  }
}
