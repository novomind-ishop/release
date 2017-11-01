package release

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
}
