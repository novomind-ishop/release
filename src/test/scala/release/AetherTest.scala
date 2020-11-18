package release

import java.time.ZonedDateTime

import org.junit.{Assert, Assume, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

class AetherTest extends AssertionsForJUnit {

  // convert to IT

  @Test
  def testExistsGav(): Unit = {
    val aether = new Aether(Opts())

    Assume.assumeTrue(aether.isReachable(false))
    val g = "javax.servlet.jsp"
    val a = "jsp-api"
    val v = "2.2"
    // "com.google.guava", "guava","30.0-jre"
    //println("r: " + aether.existsGav(g, a, v))
    // println("r: " + aether.newerVersionsOf(g, a, v))

    val g1 = "org.scalatest"
    val a1 = "scalatest_2.13"
    val v1 = "3.2.3"
    // ///39.0.0/: Checksum validation failed, no checksums available
    println(aether.depDate(g1, a1, v1))
    // Some(2020-08-21T13:40:36Z)
    // Assert.assertFalse(aether.existsGav("com.novomind.ishop.exi", "ext-b2c", "0.0.1-BERT"))
  }

  @Test
  def testParseNexusDateNoWeekday(): Unit = {
    val input = "May 20 00:48:50 UTC 2010"
    val out = Aether.parseNexusDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2010-05-20T00:48:50+00:00"), out.get)
  }

  @Test
  def testParseNexusDate(): Unit = {
    val input = "Thu May 20 00:48:50 UTC 2010"
    val out = Aether.parseNexusDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2010-05-20T00:48:50+00:00"), out.get)
  }

}
