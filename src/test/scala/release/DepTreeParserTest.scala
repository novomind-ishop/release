package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.Gav3

class DepTreeParserTest extends AssertionsForJUnit {

  @Test
  def test_single(): Unit = {

    val in =
      """com:all:pom:44
        |""".stripMargin
    Assert.assertEquals(Seq(
      Gav3(groupId = "com", artifactId = "all", version = Some("44"))
    ), DepTreeParsers.parseGavsOnly(in.trim))
  }

  @Test
  def testError(): Unit = {

    val in =
      """com
        ?""".stripMargin('?')
    Assert.assertEquals(Nil, DepTreeParsers.parseGavsOnly(in.trim))
  }
  @Test
  def test_2(): Unit = {

    val in =
      """com.core:all:pom:4
        ?+- com:bre:jar:4
        ?|  +- org.mozilla:rhino:jar:1.7.12:compile
        ?|  +- org.springframework:spring-tx:jar:5.3.32:compile
        ?|  +- org.slf4j:slf4j-api:jar:android:1.7.30:compile
        ?|  \- joda-time:joda-time:jar:2.10.13:compile
        ?""".stripMargin('?')
    Assert.assertEquals(Seq(
      Gav3(groupId = "com.core", artifactId = "all", version = Some("4")),
        Gav3(groupId = "com", artifactId = "bre", version = Some("4")),
        Gav3(groupId = "org.mozilla", artifactId = "rhino", version = Some("1.7.12")),
        Gav3(groupId = "org.springframework", artifactId = "spring-tx", version = Some("5.3.32")),
        Gav3(groupId = "org.slf4j", artifactId = "slf4j-api", version = Some("1.7.30")),
        Gav3(groupId = "joda-time", artifactId = "joda-time", version = Some("2.10.13"))
    ), DepTreeParsers.parseGavsOnly(in.trim))
  }

}
