package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Dep, Gav3, SelfRef}

class SbtModTest extends AssertionsForJUnit {

  @Test
  def scalaDeps(): Unit = {
    val scalaLib = Gav3("org.scala-lang", "scala-library", "2.13.6")
    val catsEffectM3 = Gav3("org.typelevel", "cats-effect_2.13.0-M5", "1.4.0")
    val catsCore = Gav3("org.typelevel", "cats-core_2.12", "1.4.0")
    val scalaTest = Gav3("org.scalatest", "scalatest_2.13", "3.2.9")
    val guava = Gav3("com.google.guava", "guava", "30.1.1-jre")
    val gavs: Seq[Gav3] = Seq(scalaLib, catsEffectM3, catsCore, scalaTest, guava)

    val result = gavs.map(gav => ProjectMod.scalaDeps(gavs)(gav))
    val extra = Seq(
      catsCore.copy(artifactId = "cats-core_2.13"),
      catsEffectM3.copy(artifactId = "cats-effect_2.13"),
    )
    Assert.assertEquals((gavs ++ extra).sortBy(_.toString), result.flatten.sortBy(_.toString))
  }

  @Test
  def scalaDeps_not(): Unit = {
    val guava = Gav3("com.google.guava", "guava", "30.1.1-jre")
    val gavs: Seq[Gav3] = Seq(guava)

    val result = gavs.map(gav => ProjectMod.scalaDeps(gavs)(gav))

    Assert.assertEquals(gavs, result.flatten)
  }

  @Test
  def testDoParse(): Unit = {
    val value = SbtMod.SimpleParser.doParse(
      """
        |version := "1.0"
        |
        |scalaVersion := "2.13.0"
        |
        |libraryDependencies += "redis.clients" % "jedis" % "3"
        |
        |libraryDependencies += "redis.clients" % "jedis" % "3.3.0" // 540 k
        |
        |libraryDependencies += "org.scalatestplus" %% "junit-4-12" % "3.1.2.0"
        |
        |libraryDependencies += "org.scalatestplus" %% "junit-4-12" % "3.1.2.1" % Test
        |
        |
        |libraryDependencies += "org.typelevel" % "cats-effect_2.13.0-M5" % "1.4.0"
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
      d("org.typelevel", "cats-effect_2.13.0-M5", "1.4.0"),

    ), value)
  }

}
