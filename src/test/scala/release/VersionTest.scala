package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

class VersionTest extends AssertionsForJUnit {

  @Test
  def testIsMajor(): Unit = {
    Assert.assertTrue(Version.parse("21.0.0").isMajor())
    Assert.assertTrue(Version.parse("21.0.0-SNAPSHOT").isMajor())
    Assert.assertTrue(Version.parse("21").isMajor())
    Assert.assertTrue(Version.parse("21.0").isMajor())
    Assert.assertFalse(Version.parse("21.2.3").isMajor())
    Assert.assertFalse(Version.parse("alpha").isMajor())
  }

  @Test
  def testIsOrdinal(): Unit = {
    Assert.assertTrue(Version.parse("1.0.1-SNAPSHOT").isOrdinalOnly)
    Assert.assertTrue(Version.parse("1.1.1-SNAPSHOT").isOrdinalOnly)
    val aa = Version.parse("21.0.0")
    Assert.assertTrue(aa.isOrdinal)
    Assert.assertTrue(aa.isOrdinalOnly)
    Assert.assertEquals("", aa.text)
    val a = Version.parse("21.0.0-SNAPSHOT")
    Assert.assertTrue(a.isOrdinal)
    Assert.assertTrue(a.isOrdinalOnly)
    Assert.assertEquals("", a.text)

    Assert.assertTrue(Version.parseSloppy("21.0.0-SNAPSHOT").isOrdinalOnly)
    Assert.assertFalse(Version.parse("alpha").isOrdinal)
    Assert.assertFalse(Version.parse("alpha").isOrdinalOnly)

    val alpha2 = Version.parseSloppy("210_alpha")
    Assert.assertTrue(alpha2.isOrdinal)
    Assert.assertFalse(alpha2.isOrdinalOnly)
    Assert.assertEquals("alpha", alpha2.text)
    Assert.assertTrue(Version.parse("210.0.0_alpha").isOrdinal)
    Assert.assertFalse(Version.parse("210.0.0_alpha").isOrdinalOnly)
  }

  @Test
  def testLowOrdinal(): Unit = {
    Assert.assertEquals(Int.MaxValue, Version.parseSloppy("21.0.0-SNAPSHOT").lowOrdinalPart)
    Assert.assertEquals(1, Version.parseSloppy("21.0.0_1").lowOrdinalPart)
    Assert.assertEquals(7, Version.parseSloppy("21.0.0_RC7").lowOrdinalPart)
    Assert.assertEquals(8, Version.parseSloppy("21.0.0_M8").lowOrdinalPart)
    Assert.assertEquals(20423, Version.parseSloppy("21.0.0_RC-204-23").lowOrdinalPart)
  }

  @Test
  def testSame(): Unit = {
    Assert.assertTrue(Version.parseSloppy("21x-SNAPSHOT").same(21))
    Assert.assertTrue(Version.parseSloppy("21x-SNAPSHOT").same(Seq(21)))
    Assert.assertTrue(Version.parse("21.0.0").same(21))
    Assert.assertFalse(Version.parse("21.0.0").same(2))

    Assert.assertTrue(Version.parse("21.0.0").same(21, 0))
    Assert.assertFalse(Version.parse("21.0.0").same(21, 1))

    Assert.assertTrue(Version.parse("21.0.0").same(21, 0, 0))
    Assert.assertFalse(Version.parse("21.0.0").same(21, 0, 1))
    Assert.assertTrue(Version.parse("21.0.0").same(Seq(21, 0, 0)))
    Assert.assertFalse(Version.parse("21.0.0").same(Seq(21, 0, 1)))
    Assert.assertFalse(Version.parse("21.0.0").same(Nil))
    Assert.assertFalse(Version.parse("21.0.0").same(Seq(21, 0, 0, 0)))
  }

  @Test
  def testAfterBefore(): Unit = {
    Assert.assertTrue(Version.parse("21.0.0").isGreaterEqualsOpt(Some(Version.parse("20.0.0"))))
    Assert.assertTrue(Version.parse("21.0.0").isGreaterEqualsOpt(Some(Version.parse("21.0.0"))))
    Assert.assertFalse(Version.parse("21.0.0").isGreaterEqualsOpt(Some(Version.parse("21.0.1"))))

    Assert.assertFalse(Version.parse("21.0.0").isLowerEqualsOpt(Some(Version.parse("20.0.0"))))
    Assert.assertTrue(Version.parse("21.0.0").isLowerEqualsOpt(Some(Version.parse("21.0.0"))))
    Assert.assertTrue(Version.parse("21.0.0").isLowerEqualsOpt(Some(Version.parse("21.0.1"))))
  }

  @Test
  def testVersionParse(): Unit = {
    Assert.assertEquals(Version("", 1, 2, 3, "", ""), Version.parse("1.2.3"))
    Assert.assertEquals(Version("", 1, 2, 3, "6", ""), Version.parse("1.2.3_6"))
    Assert.assertEquals(Version("", 1, 2, 3, "final", ""), Version.parse("1.2.3_final"))
    Assert.assertEquals(Version("", 3, 2, 1, "", ""), Version.parse("3.2.1-SNAPSHOT"))
    Assert.assertEquals(Version("", 7, 0, 0, "", ""), Version.parse("7"))
    Assert.assertEquals(Version("", 8, 43, 0, "", ""), Version.parse("8.43"))
  }

  @Test
  def testVersionOrdering(): Unit = {
    val in = Seq(
      Version.parseSloppy("alpha"),
      Version.parseSloppy("1.1.0.1"), // TODO not semver so before 1.0.0
      Version.parseSloppy("1.1.0-M1"),
      Version.parseSloppy("1.1.0-M2"),
      Version.parseSloppy("1.1.0-M3"),
      Version.parseSloppy("1.1.0"),
      Version.parseSloppy("1.1.1_emergency-bugfix"),
      Version.parseSloppy("1.1.1"), // regular 1.1.1
      Version.parse("1.2.3"),
      Version.parse("2.2.3"),
      Version.parse("2.2.4"),
      Version.parse("2.3.0"),
      Version.parseSloppy("4.0.0-SNAPSHOT"),
      Version.parseSloppy("4.0.0"),
      Version.parse("7"),
      Version.parse("9.1"),
      Version.parseSloppy("10-bravo"), // TODO flip with alpha
      Version.parseSloppy("10-alpha"),
      Version.parseSloppy("10"),
      Version.parse("10.2.3"),
      Version.parse("21.2.3"),
    )
    val t = in.sliding(2, 1).map(ts => {
      if (ts.size != 2) {
        throw new IllegalStateException("d")
      } else {
        ((ts.head, ts.last), Seq(Version.ordering.lt(ts.head, ts.last),
          Version.ordering.equiv(ts.head, ts.head),
          Version.ordering.equiv(ts.last, ts.last)))
      }

    }).toList
    t.foreach(l => {
      println(s"${l._1._1.format()} (${l._1._1.rawInput}) <-> ${l._1._2.format()} (${l._1._2.rawInput}) = ${l._2.mkString(", ")}")
    })
    Assert.assertEquals(1, t.flatMap(_._2).distinct.size)
  }

  @Test
  def testRemoveSnapshot(): Unit = {
    Assert.assertEquals(Version.parseSloppy("4.0.0"), Version.parseSloppy("4.0.0-SNAPSHOT").removeSnapshot())
    Assert.assertEquals(Version.parseSloppy("4.0.0"), Version.parseSloppy("4.0.0").removeSnapshot())
    Assert.assertEquals(Version.parseSloppy("main"), Version.parseSloppy("main-SNAPSHOT").removeSnapshot())
  }

  @Test
  def testRemoveSnapshot_0(): Unit = {
    val out = Version.removeTrailingSnapshots("RC-2009.01")

    Assert.assertEquals("RC-2009.01", out)
  }

  @Test
  def testRemoveSnapshot_1(): Unit = {
    val out = Version.removeTrailingSnapshots("0.1.1-SNAPSHOT")

    Assert.assertEquals("0.1.1", out)
  }

  @Test
  def testRemoveSnapshot_2(): Unit = {
    val out = Version.removeTrailingSnapshots("hallo-SNAPSHOT-SNAPSHOT")

    Assert.assertEquals("hallo", out)
  }

  @Test
  def testRemoveSnapshot_3(): Unit = {
    val out = Version.removeTrailingSnapshots("-SNAPSHOT-hallo-SNAPSHOT-SNAPSHOT")

    Assert.assertEquals("-SNAPSHOT-hallo", out)
  }

}
