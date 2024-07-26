package release

import java.time.{Duration, ZonedDateTime}
import org.junit.{Assert, Assume, Test}
import org.mockito.Mockito
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

import scala.annotation.nowarn

class RepoTest extends AssertionsForJUnit {

  // convert to IT

  @Test
  def testExistsGav(): Unit = {
    val repo = Opts(useDefaults = true).newRepo

    Assume.assumeTrue(repo.isReachable(false).online)
    @nowarn
    val g = "javax.servlet.jsp"
    @nowarn
    val a = "jsp-api"
    @nowarn
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
    println(repo.newerAndPrevVersionsOf("com.google.guava", "guava", "32.1.2-jre"))
    return
    println(repo.newerAndPrevVersionsOf("org.scala-lang", "scala3-library_3", "3.0.1-RC1"))
    println(repo.getRelocationOf("org.scala-lang", "scala-library", "-1"))
    println(repo.newerAndPrevVersionsOf("org.scala-lang", "scala3-library_3", "-1"))
    println(repo.latestGav("com.google.guava", "guava", "32.1.2-jre"))
    println(repo.latestGav("com.google.guava", "guava", "1.1.2-jre"))
    println(repo.latestGav("com.google.guava", "guava", "0.1.2-jre"))
    println(repo.latestGav("com.google.guava", "guava", "32.1.2-jre-SNAPSHOT"))
    println(repo.latestGav("com.google.guava", "guava", "1.0.0-SNAPSHOT"))
  }


  @Test
  def testNewerAndPrevVersionsOf_trival(): Unit = {
    val result = Repo.convertNewerAndPrefVersions("g", "a", "v", in => in.split(' ').toSeq)
    Assert.assertEquals(Seq("g:a:[-2,)"), result)
  }

  @Test
  def testNewerAndPrevVersionsOf_SNAPSHOT(): Unit = {
    Assert.assertEquals(Seq("32.1.2-android", "33-SNAPSHOT"),
      Repo.convertNewerAndPrefVersions("com.google.guava", "guava", "33-SNAPSHOT",
        k => Seq("31.0-android", "31.0-jre", "31.0.1-android", "31.0.1-jre", "31.1-android", "31.1-jre",
          "32.0.0-android", "32.0.0-jre", "32.0.1-android", "32.0.1-jre", "32.1.0-android", "32.1.0-jre",
          "32.1.1-android", "32.1.1-jre", "32.1.2-android", "33-SNAPSHOT")))
  }

  @Test
  def testNewerAndPrevVersionsOf(): Unit = {
    Assert.assertEquals(Seq("32.1.1-jre", "32.1.1-android", "32.1.2-jre", "32.1.2-android"),
      Repo.convertNewerAndPrefVersions("com.google.guava", "guava", "32.1.2-jre",
        k => Seq("31.0-android", "31.0-jre", "31.0.1-android", "31.0.1-jre", "31.1-android", "31.1-jre",
          "32.0.0-android", "32.0.0-jre", "32.0.1-android", "32.0.1-jre", "32.1.0-android", "32.1.0-jre",
          "32.1.1-android", "32.1.1-jre", "32.1.2-android", "32.1.2-jre")))
  }

  @Test
  def testNewerAndPrevVersionsOf_latest(): Unit = {
    Assert.assertEquals(Seq("32.1.2-jre", "32.1.2-android"),
      Repo.convertNewerAndPrefVersions("com.google.guava", "guava", "LATEST",
        k => Seq("31.0-android", "31.0-jre", "31.0.1-android", "31.0.1-jre", "31.1-android", "31.1-jre",
          "32.0.0-android", "32.0.0-jre", "32.0.1-android", "32.0.1-jre", "32.1.0-android", "32.1.0-jre",
          "32.1.1-android", "32.1.1-jre", "32.1.2-android", "32.1.2-jre")))
  }

  @Test
  def testNewerAndPrevVersionsOf_simple(): Unit = {
    Assert.assertEquals(Seq("0.9.9", "1.0.0-M1", "1.0.0"),
      Repo.convertNewerAndPrefVersions("org.example", "example", "1.0.0-M1",
        k => Seq("1.0.0-M1", "0.9.9", "1.0.0")))
  }

  @Test
  def testNewerAndPrevVersionsOf_empty(): Unit = {
    Assert.assertEquals(Nil,
      Repo.convertNewerAndPrefVersions("com.google.guava", "guava", "LATEST", k => Nil))
  }

  @Test
  def testParseNexusDateNoWeekday(): Unit = {
    val input = "May 20 00:48:50 UTC 2010"
    val out = Repo.extractDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2010-05-20T00:48:50+00:00"), out.get)
  }

  @Test
  def testParseNexusDate(): Unit = {
    val input = "Thu May 20 00:48:50 UTC 2010"
    val out = Repo.extractDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2010-05-20T00:48:50+00:00"), out.get)
  }

  @Test
  def testParseCentralDate(): Unit = {
    val input = "scalatest_2.13-3.2.3-javadoc.jar                  2020-11-07 10:27       189"
    val out = Repo.extractDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2020-11-07T10:27:00+00:00"), out.get)
  }

  @Test
  def testParseCentralDate2(): Unit = {
    val input = "scalatest_2.13-3.2.3-javadoc.jar                  2021-10-02 11:17:01       189"
    val out = Repo.extractDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2021-10-02T11:17:01+00:00"), out.get)
  }

  @Test
  def testParseArtifactoryDate(): Unit = {
    val input = "<a href=\"jfrog-20240515.115211-1.data\">jfrog-20240515.115211-1.data</a>                         15-May-2024 11:59  268 bytes"
    val out = Repo.extractDate(input)
    Assert.assertEquals(ZonedDateTime.parse("2024-05-15T11:59:00+00:00"), out.get)
  }

  @Test
  def testRepoMetrics(): Unit = {
    val mock1 = Mockito.mock(classOf[RepoZ])
    Mockito.when(mock1.getMetrics).thenReturn(
      RepoMetrics(dateCollection = Duration.parse("PT18.845438003S"), dateCollectionCount = 1,
        versionCollection = Duration.parse("PT1.135438003S"), versionCollectionCount = 2))
    val out = new RepoProxy(Seq(mock1))

    Assert.assertEquals(Duration.parse("PT1.14S"), out.getMetrics.versionCollection)
    Assert.assertEquals(Duration.parse("PT18.85S"), out.getMetrics.dateCollection)
  }

}
