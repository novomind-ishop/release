package release

import java.time.ZonedDateTime

import org.junit.{Assert, Assume, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

class RepoTest extends AssertionsForJUnit {

  // convert to IT

  @Test
  def testExistsGav(): Unit = {
    val repo = Repo.of(Opts(useDefaults = true))

    Assume.assumeTrue(repo.isReachable(false).online)
    val g = "javax.servlet.jsp"
    val a = "jsp-api"
    val v = "2.2"
    // "com.google.guava", "guava","30.0-jre"
    //println("r: " + repo.existsGav(g, a, v))
    // println("r: " + repo.newerVersionsOf(g, a, v))

    val g1 = "org.scalatest"
    val a1 = "scalatest_2.13"
    val v1 = "3.2.3"
    // ///39.0.0/: Checksum validation failed, no checksums available
    println(repo.depDate(g1, a1, v1))
    // 2020-11-07T10:27:03Z
    // scalatest_2.13-3.2.3-javadoc.jar                  2020-11-07 10:27       189
    // Some(2020-08-21T13:40:36Z)
    // Assert.assertFalse(repo.existsGav("com.novomind.ishop.exi", "ext-b2c", "0.0.1-BERT"))
    println(repo.getRelocationOf("org.scala-lang", "scala-library", "-1"))
    println(repo.newerVersionsOf("org.scala-lang", "scala3-library_3", "-1"))
    println(repo.latestGav("com.google.guava", "guava", "32.1.2-jre"))
    println(repo.latestGav("com.google.guava", "guava", "1.1.2-jre"))
    println(repo.latestGav("com.google.guava", "guava", "0.1.2-jre"))
    println(repo.latestGav("com.google.guava", "guava", "32.1.2-jre-SNAPSHOT"))
    println(repo.latestGav("com.google.guava", "guava", "1.0.0-SNAPSHOT"))
  }

  @Test
  def testParseNexusDateNoWeekday(): Unit = {
    val input = "May 20 00:48:50 UTC 2010"
    val out = Repo.parseNexusDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2010-05-20T00:48:50+00:00"), out.get)
  }

  @Test
  def testParseNexusDate(): Unit = {
    val input = "Thu May 20 00:48:50 UTC 2010"
    val out = Repo.parseNexusDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2010-05-20T00:48:50+00:00"), out.get)
  }

  @Test
  def testParseCentralDate(): Unit = {
    val input = "scalatest_2.13-3.2.3-javadoc.jar                  2020-11-07 10:27       189"
    val out = Repo.parseCentralDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2020-11-07T10:27:00+00:00"), out.get)
  }

  @Test
  def testParseCentralDate2(): Unit = {
    val input = "scalatest_2.13-3.2.3-javadoc.jar                  2021-10-02 11:17:01       189"
    val out = Repo.parseCentralDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2021-10-02T11:17:01+00:00"), out.get)
  }

}
