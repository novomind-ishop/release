package release

import java.io.File
import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

import java.util

class UtilTest extends AssertionsForJUnit {

  @Test
  def testDistance(): Unit = {
    Assert.assertEquals(1, Util.levenshtein("A", "B"))
    Assert.assertEquals(4, Util.levenshtein("AssertionsForJUnit", "AssertionsJUnid"))
  }

  @Test
  def testSoundex(): Unit = {

    Assert.assertEquals(4, Util.soundex("A", "A"))
    Assert.assertEquals(4, Util.soundex("All", "All"))
    Assert.assertEquals(3, Util.soundex("All", "Bll"))
    Assert.assertEquals(3, Util.soundex("A", "B"))
    Assert.assertEquals(4, Util.soundex("AssertionsForJUnit", "AssertionsJUnid"))
    Assert.assertEquals(2, Util.soundex("core", "bre"))
    Assert.assertEquals(2, Util.soundex("Yongera", "Remkpu"))
    Assert.assertEquals(2, Util.soundex("Zonger", "Remkpu"))
    Assert.assertEquals(2, Util.soundex("Anger", "Rengeo"))
    Assert.assertEquals(3, Util.soundex("Anger", "Ranger"))
    Assert.assertEquals(0, Util.soundex("Otto", "Ranger"))


    Assert.assertEquals(0, Util.soundexMax(Seq("Otto"), Seq("Ranger")))
    Assert.assertEquals(0, Util.soundexMax(Seq("Otto", "o"), Seq("Otto")))
    Assert.assertEquals(4, Util.soundexMax(Seq("Otto", "o"), Seq("Otto", "u")))

    Assert.assertEquals(0, Util.soundexSplitMax("Otto", "Ranger"))
    Assert.assertEquals(4, Util.soundexSplitMax("Otto Bert", "Otto Wert"))
    Assert.assertEquals(0, Util.soundexSplitMax("ishop-xx-commons", "ishop-commons"))

    Assert.assertEquals(4, Util.soundexSplitMax("core-bom", "core-api"))
    Assert.assertEquals(2, Util.soundexSplitMin("ui", "vue"))
  }


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
    val in: String = null
    Assert.assertTrue(in.blank())
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
  def testIpFromUrl(): Unit = {
    println(Util.ipFromUrl("http://www.example.org/bert"))
    println(Util.ipFromUrl("example.org"))
    println(Util.ipFromUrl("1"))
    println(Util.ipFromUrl("#"))
    println(Util.ipFromUrl(""))
    println(Util.ipFromUrl(" "))
    println(Util.ipFromUrl(null))
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
    Assert.assertEquals("da39a3ee5e6b4b0d3255bfef95601890afd80709", Util.hashSha1(""))
  }

  @Test
  def testHashMurmur3_32_fixed(): Unit = {
    Assert.assertEquals("e397ece2", Util.hashMurmur3_32_fixed("bert"))
    Assert.assertEquals("00000000", Util.hashMurmur3_32_fixed(""))
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

  @Test
  def testToScalaMap_empty(): Unit = {
    val in = new util.HashMap[String, String]()
    Assert.assertEquals(Map.empty[String, String], Util.toScalaMapNonNull(in))
  }

  @Test
  def testToScalaMap_null_value(): Unit = {
    val in = new util.HashMap[String, String]()
    in.put("a", null)
    Assert.assertEquals(Map.empty[String, String], Util.toScalaMapNonNull(in))
    Assert.assertEquals(Map.empty[String, String], Util.toScalaMapNonBlank(in))
  }

  @Test
  def testToScalaMap_null_key(): Unit = {
    val in = new util.HashMap[String, String]()
    in.put(null, "a")
    Assert.assertEquals(Map.empty[String, String], Util.toScalaMapNonNull(in))
    Assert.assertEquals(Map.empty[String, String], Util.toScalaMapNonBlank(in))
  }

  @Test
  def testToScalaMap(): Unit = {
    val in = new util.HashMap[String, String]()
    in.put("b", "a")
    Assert.assertEquals(Map("b" -> "a"), Util.toScalaMapNonNull(in))
    Assert.assertEquals(Map("b" -> "a"), Util.toScalaMapNonBlank(in))
  }

  @Test
  def testToScalaMap_blank_value(): Unit = {
    val in = new util.HashMap[String, String]()
    in.put("b", "")
    Assert.assertEquals(Map("b" -> ""), Util.toScalaMapNonNull(in))
    Assert.assertEquals(Map.empty, Util.toScalaMapNonBlank(in))
  }

  @Test
  def testToScalaMap_blank_key(): Unit = {
    val in = new util.HashMap[String, String]()
    in.put("", "b")
    Assert.assertEquals(Map("" -> "b"), Util.toScalaMapNonNull(in))
    Assert.assertEquals(Map.empty, Util.toScalaMapNonBlank(in))
  }

  @Test
  def testShow(): Unit = {
    case class Bert(name: String, s: Seq[Int], otherNames:List[String], innerBerts: Seq[Bert])
    Assert.assertEquals(
      """Bert(name = "Bert", s = Seq(1, 2), otherNames = Seq("otto", "man"), innerBerts = List(Bert(name = "Treb", s = List(), otherNames = List(), innerBerts = List())))
        |""".stripMargin.trim, Util.show(Bert(name = "Bert", s = Seq(1, 2), otherNames = List("otto", "man"),
        innerBerts = List(Bert(name = "Treb", Nil, Nil, Nil)))))
  }
}
