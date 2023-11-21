package release

import org.junit.{Assert, Test}
import org.mockito.ArgumentMatchers.anyString
import org.mockito.MockitoSugar
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Dep, Gav3, SelfRef}
import release.SbtModTest.d

object SbtModTest {
  def d(g: String, a: String, v: String, scope: String = "") =
    ProjectModTest.depOf(g,a,v,scope)
}

class SbtModTest extends AssertionsForJUnit with MockitoSugar {

  @Test
  def scalaDeps(): Unit = {
    val scalaLib = Gav3("org.scala-lang", "scala-library", Some("2.13.6"))
    val catsEffectM3 = Gav3("org.typelevel", "cats-effect_2.13.0-M5", Some("1.4.0"))
    val catsCore = Gav3("org.typelevel", "cats-core_2.12", Some("1.4.0"))
    val scalaTest = Gav3("org.scalatest", "scalatest_2.13", Some("3.2.9"))
    val guava = Gav3("com.google.guava", "guava", Some("30.1.1-jre"))
    val gavs: Seq[Gav3] = Seq(scalaLib, catsEffectM3, catsCore, scalaTest, guava)

    val repo = mock[Repo]
    when(repo.getRelocationOf(anyString(), anyString(), anyString())).thenReturn(None)
    val result = gavs.map(gav => ProjectMod.relocateGavs(gavs, repo)(gav))
    val scalaLib3 = Gav3("org.scala-lang", "scala3-library_3", Some("-1"))
    val extra = Seq(
      catsCore.copy(artifactId = "cats-core_2.13"),
      catsEffectM3.copy(artifactId = "cats-effect_2.13"),
      scalaLib3,
    )
    Assert.assertEquals((gavs ++ extra).sortBy(_.toString), result.flatten.sortBy(_.toString))
  }

  @Test
  def scalaDeps3(): Unit = {
    val scalaLib = Gav3("org.scala-lang", "scala3-library_3", Some("3.0.1"))
    val catsEffectM3 = Gav3("org.typelevel", "cats-effect_2.13.0-M5", Some("1.4.0"))
    val catsCore = Gav3("org.typelevel", "cats-core_2.12", Some("1.4.0"))
    val gavs: Seq[Gav3] = Seq(scalaLib, catsEffectM3, catsCore)

    val repo = mock[Repo]
    when(repo.getRelocationOf(anyString(), anyString(), anyString())).thenReturn(None)
    val result = gavs.map(gav => {
      ProjectMod.relocateGavs(gavs, repo)(gav)
    })
    val extra = Seq(
      catsCore.copy(artifactId = "cats-core_3"),
      catsEffectM3.copy(artifactId = "cats-effect_3"),
    )
    Assert.assertEquals((gavs ++ extra).sortBy(_.toString), result.flatten.sortBy(_.toString))
  }

  @Test
  def scalaDeps_not(): Unit = {
    val guava = Gav3("com.google.guava", "guava", Some("30.1.1-jre"))
    val gavs: Seq[Gav3] = Seq(guava)

    val result = gavs.map(gav => ProjectMod.relocateGavs(gavs, mock[Repo])(gav))

    Assert.assertEquals(gavs, result.flatten)
  }

  @Test
  def testDoParse(): Unit = {
    val value = SbtMod.SloppyParser.doParse(strict = true)(
      """
        |version := "1.0-SNAPSHOT"
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

    Assert.assertEquals(Seq(
      d("org.scala-lang", "scala-library", "2.13.0"),
      d("redis.clients", "jedis", "3"),
      d("redis.clients", "jedis", "3.3.0"),
      d("org.scalatestplus", "junit-4-12_2.13", "3.1.2.0"),
      d("org.scalatestplus", "junit-4-12_2.13", "3.1.2.1"),
      d("org.typelevel", "cats-effect_2.13.0-M5", "1.4.0"),

    ), value.deps)
    Assert.assertEquals(Some("1.0-SNAPSHOT"), value.selfVersion)
  }

  @Test
  def testDoParse3(): Unit = {
    val value = SbtMod.SloppyParser.doParse(strict = true)(
      """
        |version := "1.0"
        |
        |scalaVersion := "3.0.1"
        |
        |libraryDependencies += "org.scalatestplus" %% "junit-4-12" % "3.1.2.1" % Test
        |
        |""".stripMargin.trim)

    Assert.assertEquals(Seq(
      d("org.scala-lang", "scala3-library_3", "3.0.1"),
      d("org.scalatestplus", "junit-4-12_3", "3.1.2.1"),

    ), value.deps)
  }

  @Test
  def testDoParse_val(): Unit = {
    val value = SbtMod.SloppyParser.doParse(strict = true)(
      """
        |version := "1.0"
        |
        |name := "dnslookup"
        |
        |logLevel := Level.Warn
        |
        |scalaVersion := "3.0.1"
        |
        |val vers = "a.b.c"
        |
        |assembly / logLevel := Level.Warn
        |
        | // upsi
        |
        |scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
        |
        |publish / skip := true
        |
        |libraryDependencies += "org.scalatestplus" %% "junit-4-12" % vers % Test
        |
        |""".stripMargin.trim)

    Assert.assertEquals(Seq(
      d("org.scala-lang", "scala3-library_3", "3.0.1"),
      d("org.scalatestplus", "junit-4-12_3", "a.b.c"),

    ), value.deps)
  }

  @Test
  def testDoParseBuildProperties(): Unit = {
    val value = SbtMod.SloppyParser.doParse(strict = true)(
      """
        |sbt.version=1.5.5
        |
        |""".stripMargin.trim)

    Assert.assertEquals(Seq(
      d("org.scala-sbt", "sbt", "1.5.5"),
    ), value.deps)
  }

  @Test
  def testDoParsePlugins(): Unit = {
    val value = SbtMod.SloppyParser.doParse(strict = true)(
      """
        |addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.0.0")
        |
        |addSbtPlugin(  "org.scoverage"    %  "sbt-scoverage" % "1.6.1" ) // helo
        |
        |""".stripMargin.trim)

    Assert.assertEquals(Seq(
      d("com.eed3si9n", "sbt-assembly", "1.0.0"),
      d("org.scoverage", "sbt-scoverage", "1.6.1"),
    ), value.deps)
  }

}
