package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Dep, SelfRef}

class SbtModTest extends AssertionsForJUnit {

  @Test
  def testDoParse(): Unit = {


    val value = SbtMod.SimpleParser.doParse(
      """
        |version := "1.0"
        |
        |scalaVersion := "2.13.0"
        |
        |libraryDependencies += "redis.clients" % "jedis" % "3"
        |libraryDependencies += "redis.clients" % "jedis" % "3.3.0" // 540 k
        |libraryDependencies += "org.scalatestplus" %% "junit-4-12" % "3.1.2.0"
        |libraryDependencies += "org.scalatestplus" %% "junit-4-12" % "3.1.2.1" % Test
        |
        |""".stripMargin.trim)

    def d(g: String, a: String, v: String, scope: String = "") =
      Dep(SelfRef.undef, g, a, v, "", scope, "", "")

    Assert.assertEquals(Seq(
      d("org.scala-lang", "scala-library", "2.13.0"),
      d("redis.clients", "jedis", "3"),
      d("redis.clients", "jedis", "3.3.0"),
      d("org.scalatestplus", "junit-4-12_2.13", "3.1.2.0"),
      d("org.scalatestplus", "junit-4-12_2.13", "3.1.2.1"),

    ), value)
  }

}
