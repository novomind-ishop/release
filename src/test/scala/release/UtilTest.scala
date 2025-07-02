package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Util.{Mailbox, UrlUserInfo}

import java.net.URI
import java.time.{Duration, Period}
import java.util
import scala.util.Random

object UtilTest {
  def randomSha1(): String = {
    Util.hashSha1(Random.nextLong().toString)
  }
}

class UtilTest extends AssertionsForJUnit {

  @Test
  def testPaseMailbox(): Unit = {
    Assert.assertEquals(Right(Mailbox("Your Name", "you@example.com")), Util.parseMailbox("Your Name <you@example.com>"))
    Assert.assertEquals(Right(Mailbox("Your Real Name", "you@example.com")), Util.parseMailbox("Your Real Name <you@example.com>"))
    Assert.assertEquals(Left("Could not parse mailbox with name from: 'You'"), Util.parseMailbox("You"))
  }

  @Test
  def testIsMailboxWithTldHostname(): Unit = {
    Assert.assertTrue(Util.isMailboxWithTldHostname("Your Real Name <you@example.com>"))
    Assert.assertFalse(Util.isMailboxWithTldHostname("Your Real Name <you@examplecom>"))
    Assert.assertFalse(Util.isMailboxWithTldHostname("Your Real Name <you.name@examplecom>"))
  }

  @Test
  def testRoundDuration(): Unit = {
    Assert.assertEquals(Duration.parse("PT0S"), Util.roundDuration(Duration.parse("PT0S")))
    Assert.assertEquals(Duration.parse("PT18.85S"), Util.roundDuration(Duration.parse("PT18.845438003S")))
    Assert.assertEquals(Duration.parse("PT1H30M18.85S"), Util.roundDuration(Duration.parse("PT1H30M18.845438003S")))
  }

  @Test
  def testDurationPeriods(): Unit = {
    val dDays = Duration.ofDays(39)
    val pDays = Period.ofDays(39)

    Assert.assertEquals(dDays.toDays, pDays.getDays)

    Assert.assertEquals(0, pDays.getMonths)
    val period = Util.toPeriod(dDays)
    Assert.assertEquals(1, period.getMonths)

  }

  @Test
  def testByteToMb(): Unit = {
    Assert.assertEquals(0L, Util.byteToMb(1_000L))
    Assert.assertEquals(1L, Util.byteToMb(1_048_576L))
    Assert.assertEquals(1L, Util.byteToMb(2_000_000L))
  }

  @Test
  def testUserdata(): Unit = {
    Assert.assertEquals("A", Util.stripUserinfo("A"))
    Assert.assertEquals("", Util.stripUserinfo(""))
    Assert.assertEquals("", Util.stripUserinfo(null))
    Assert.assertEquals("https://example.org/a?a=b", Util.stripUserinfo("https://username@example.org/a?a=b"))
    Assert.assertEquals("https://example.org/a?a=b", Util.stripUserinfo("https://username@example.org@example.org/a?a=b"))
    Assert.assertEquals("git.example.com:some/repo.git", Util.stripUserinfo("git@git.example.com:some/repo.git"))
  }

  @Test
  def testCreateURI(): Unit = {
    Assert.assertEquals(URI.create("as"), Util.createUri("as"))
    Assert.assertEquals(URI.create(s"https://${Util.urlEncode("user@name:pw")}@example.org/a?a=b"), Util.createUri("https://user@name:pw@example.org/a?a=b"))
    Assert.assertEquals(URI.create(s"http://${Util.urlEncode("user@na@:;me:pw")}@e"), Util.createUri("http://user@na@:;me:pw@e"))
  }

  @Test
  def testExtractUserdata(): Unit = {
    Assert.assertEquals(UrlUserInfo("A", None, None), Util.extractedUserInfoUrl("A"))
    Assert.assertEquals(UrlUserInfo("", None, None), Util.extractedUserInfoUrl(""))
    Assert.assertEquals(UrlUserInfo("", None, None), Util.extractedUserInfoUrl(null))
    Assert.assertEquals(UrlUserInfo("https://example.org/a?a=b", Some("username"), None), Util.extractedUserInfoUrl("https://username@example.org/a?a=b"))
    Assert.assertEquals(UrlUserInfo("https://example.org/a?a=b", Some("user@name"), Some("pw")), Util.extractedUserInfoUrl("https://user@name:pw@example.org/a?a=b"))
    Assert.assertEquals(UrlUserInfo("git.example.com:some/repo.git", None, None), Util.extractedUserInfoUrl("git@git.example.com:some/repo.git"))
  }

  @Test
  def testDistance(): Unit = {
    Assert.assertEquals(1, Util.Similarity.levenshtein("A", "B"))
    Assert.assertEquals(4, Util.Similarity.levenshtein("AssertionsForJUnit", "AssertionsJUnid"))
  }

  @Test
  def testCaverphone(): Unit = {
    Assert.assertEquals(3, Util.Similarity.caverphone("bre", "core"))
    Assert.assertEquals(3, Util.Similarity.caverphone("core", "bre")) // XXX to less?
    Assert.assertEquals(4, Util.Similarity.caverphone("any44", "any45"))
    Assert.assertEquals(0, Util.Similarity.caverphone("A", "A"))
    Assert.assertEquals(0, Util.Similarity.caverphone("All", "All"))
    Assert.assertEquals(3, Util.Similarity.caverphone("All", "Bll"))
    Assert.assertEquals(8, Util.Similarity.caverphone("A", "B"))
    Assert.assertEquals(3, Util.Similarity.caverphone("ui", "app"))
    Assert.assertEquals(3, Util.Similarity.caverphone("bert", "core"))
    Assert.assertEquals(4, Util.Similarity.caverphone("AssertionsForJUnit", "AssertionsJUnid"))
    Assert.assertEquals(3, Util.Similarity.caverphone("Yongera", "Remkpu"))
    Assert.assertEquals(4, Util.Similarity.caverphone("Zonger", "Remkpu"))
    Assert.assertEquals(1, Util.Similarity.caverphone("Anger", "Rengeo")) // XXX to less?
    Assert.assertEquals(1, Util.Similarity.caverphone("Anger", "Ranger"))
    Assert.assertEquals(4, Util.Similarity.caverphone("Otto", "Ranger"))
    Assert.assertEquals(4, Util.Similarity.caverphone("any8", "any"))
    Assert.assertEquals(4, Util.Similarity.caverphone("any8", "any9"))
    Assert.assertEquals(6, Util.Similarity.caverphone("any88", "any99"))
    Assert.assertEquals(1, Util.Similarity.caverphone("bo-client", "bo-client-ui"))
    Assert.assertEquals(2, Util.Similarity.caverphone("bo-client", "bo-client-roo"))
    List.tabulate(10)(in => Assert.assertEquals(0, Util.Similarity.caverphone(s"any${in}", s"any${in}")))
    val perm = List.tabulate(10)(in => in)
    val cartesian = perm.flatMap(x => perm.map(y => (x, y))).filterNot(x => x._1 == x._2)
    cartesian.foreach(in => {
      val i = Util.Similarity.caverphone(s"any${in._1}", s"any${in._2}")
      Assert.assertTrue(s"${in} .. ${i}", i >= 2)
    }
    )
  }

  @Test
  def testSoundex(): Unit = {

    Assert.assertEquals(4, Util.Similarity.soundex("A", "A"))
    Assert.assertEquals(4, Util.Similarity.soundex("All", "All"))
    Assert.assertEquals(3, Util.Similarity.soundex("All", "Bll"))
    Assert.assertEquals(3, Util.Similarity.soundex("A", "B"))
    Assert.assertEquals(4, Util.Similarity.soundex("AssertionsForJUnit", "AssertionsJUnid"))
    Assert.assertEquals(2, Util.Similarity.soundex("core", "bre"))
    Assert.assertEquals(2, Util.Similarity.soundex("Yongera", "Remkpu"))
    Assert.assertEquals(2, Util.Similarity.soundex("Zonger", "Remkpu"))
    Assert.assertEquals(2, Util.Similarity.soundex("Anger", "Rengeo"))
    Assert.assertEquals(3, Util.Similarity.soundex("Anger", "Ranger"))
    Assert.assertEquals(0, Util.Similarity.soundex("Otto", "Ranger"))
    Assert.assertEquals(2, Util.Similarity.soundex("any8", "any"))
    Assert.assertEquals(3, Util.Similarity.soundex("any8", "any9"))
    Assert.assertEquals(3, Util.Similarity.soundex("any88", "any99"))
    Assert.assertEquals(4, Util.Similarity.soundex("any44", "any45")) // XXX not good
    Assert.assertEquals(4, Util.Similarity.soundex("any1", "any2")) // XXX not good
    List.tabulate(10)(in => Assert.assertEquals(4, Util.Similarity.soundex(s"any${in}", s"any${in}")))
  }

  @Test
  def testSimilar(): Unit = {

    Assert.assertEquals(2, Util.Similarity.similarSplitMax("abc-client", "abc-parent"))
    Assert.assertEquals(4, Util.Similarity.similarMax(Seq("Otto", "o"), Seq("Otto", "u")))
    Assert.assertEquals(8, Util.Similarity.similarSplitMax("bo-client", "bo-client-ui"))
    Assert.assertEquals(4, Util.Similarity.similarMax(Seq("Otto"), Seq("Ranger")))
    Assert.assertEquals(4, Util.Similarity.similarMax(Seq("Otto", "o"), Seq("Otto")))

    Assert.assertEquals(4, Util.Similarity.similarSplitMax("Otto", "Ranger"))
    Assert.assertEquals(1, Util.Similarity.similarSplitMax("Otto Bert", "Otto Wert"))
    Assert.assertEquals(9, Util.Similarity.similarSplitMax("ishop-xx-commons", "ishop-commons"))
    Assert.assertEquals(0, Util.Similarity.similarSplitMax("ishop-kommons", "ishop-commons"))
    Assert.assertEquals(1, Util.Similarity.similarSplitMax("ishop-konnons", "ishop-commons"))

    Assert.assertEquals(9, Util.Similarity.similarSplitMax("core-bom", "core-api"))
    Assert.assertEquals(3, Util.Similarity.similarSplitMin("ui", "vue"))
    Assert.assertEquals(6, Util.Similarity.similarSplitMin("api", "ui"))
    Assert.assertEquals(6, Util.Similarity.similarSplitMin("cli", "api"))
    Assert.assertEquals(6, Util.Similarity.similarSplitMin("web", "api"))
    Assert.assertEquals(3, Util.Similarity.similarSplitMin("gui", "ui"))
    Assert.assertEquals(6, Util.Similarity.similarSplitMin("app", "sba"))
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
  def testExpandDigits(): Unit = {
    Assert.assertEquals("Ocean", Util.Similarity.expandDigits("1"))
    Assert.assertEquals("QuasarOceanOcean", Util.Similarity.expandDigits("911"))
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
    Assert.assertEquals("value", "value".pluralize(1))
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
    case class Bert(name: String, s: Seq[Int], otherNames: List[String], innerBerts: Seq[Bert])
    Assert.assertEquals(
      """Bert(name = "Bert", s = Seq(1, 2), otherNames = Seq("otto", "man"), innerBerts = List(Bert(name = "Treb", s = List(), otherNames = List(), innerBerts = List())))
        |""".stripMargin.trim, Util.show(Bert(name = "Bert", s = Seq(1, 2), otherNames = List("otto", "man"),
        innerBerts = List(Bert(name = "Treb", Nil, Nil, Nil)))))
  }
}
