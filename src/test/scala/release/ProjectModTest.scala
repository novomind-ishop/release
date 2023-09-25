package release

import org.junit.{Assert, Test}
import org.mockito.ArgumentMatchers.anyString
import org.mockito.MockitoSugar._
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Dep, Gav, Gav3, GavWithRef, SelfRef}
import release.ProjectModTest.MockMod
import release.SbtModTest.d
import release.Starter.OptsDepUp

import java.io.File
import java.time.{Duration, ZonedDateTime}
import scala.collection.immutable.ListMap

object ProjectModTest {
  class MockMod extends ProjectMod {

    override val file: File = new File("")
    override val opts: Starter.Opts = Starter.Opts()
    override lazy val repo: Repo = throw new IllegalStateException("use a mock please")
    override val selfVersion: String = "???"
    override val listDependencies: Seq[ProjectMod.Dep] = Nil
    override val listPluginDependencies: Seq[ProjectMod.PluginDep] = Nil
    override val listProperties: Map[String, String] = Map("undef" -> "undef")

    override def isShop: Boolean = false

    override val skipPropertyReplacement: Boolean = false

    override val selfDepsMod: Seq[ProjectMod.Dep] = Nil

    override def suggestReleaseVersion(branchNames: Seq[String], tagNames: Seq[String], increment: Option[Increment]): Seq[String] = Nil

    override def suggestNextRelease(releaseVersion: String): String = "???"

    override def listSnapshotDependenciesDistinct: Seq[ProjectMod.Dep] = Nil

    override def writeTo(targetFolder: File): Unit = {

    }

    override def changeVersion(newVersion: String): Unit = {

    }

    override def changeDependecyVersion(patch: Seq[(ProjectMod.Gav3, String)]): Unit = {

    }

    override def depTreeFilenameList(): Seq[String] = Nil
  }

  def depOf(g: String, a: String, v: String, scope: String = ""): ProjectMod.Dep = {
    Dep(SelfRef.undef, g, a, Some(v), "", scope, "", "")
  }
}

class ProjectModTest extends AssertionsForJUnit {
  implicit def toOpt(in: String) = Option(in)

  @Test
  def testScalaDeps(): Unit = {
    val now = ZonedDateTime.now()
    val scala = d("org.scala-lang", "scala-library", "2.13.0")

    val term = Term.select("xterm", "os", simpleChars = false, isInteractice = false)
    val rootDeps: Seq[ProjectMod.Dep] = Seq(scala)
    val selfDepsModX: Seq[ProjectMod.Dep] = Nil
    val repoMock = mock[Repo]
    when(repoMock.getRelocationOf(anyString(), anyString(), anyString())).thenReturn(None)
    when(repoMock.newerVersionsOf(scala.groupId, scala.artifactId, scala.version.get)).thenReturn(Seq(scala.version.get, "2.13.1"))
    when(repoMock.depDate(scala.groupId, scala.artifactId, scala.version.get)).thenReturn(Some(now))
    when(repoMock.depDate(scala.groupId, scala.artifactId, "2.13.1")).thenReturn(Some(now))
    when(repoMock.newerVersionsOf(scala.groupId, "scala3-library_3", "-1")).thenReturn(Seq("-1", "3.0.1"))
    when(repoMock.depDate(scala.groupId, "scala3-library_3", "-1")).thenReturn(None)
    val result = TermTest.withOutErr[Unit]()(sys => {
      val opts = OptsDepUp().copy(showLibYears = true)
      val innerResult: Seq[(GavWithRef, (Seq[String], Duration))] = ProjectMod.showDependencyUpdates(100, term,
        opts, rootDeps, selfDepsModX, Seq(repoMock), sys,
        printProgress = true, checkOnline = false)

      val sca1 = GavWithRef(SelfRef.undef, Gav("org.scala-lang", "scala-library", "2.13.0"))
      val scala2 = (sca1, (Seq("2.13.1"), Duration.ZERO))
      val sca2 = GavWithRef(SelfRef.undef, Gav("org.scala-lang", "scala3-library_3", "-1"))
      val scala3 = (sca2, (Seq("3.0.1"), Duration.ofDays(-1)))
      Assert.assertEquals(Seq(scala2, scala3), innerResult)
      val resultMod = new MockMod {

        override lazy val repo: Repo = repoMock

        override private[release] def listGavsForCheck() = rootDeps

        override val selfDepsMod: Seq[Dep] = selfDepsModX
      }.showDependencyUpdates(100, term, opts, sys, printProgress = true, checkOn = false)
      Assert.assertEquals(Seq(scala2, scala3), resultMod)
    })
    Assert.assertEquals("", result.err)
  }

  @Test
  def testListDependeciesForCheck_empty(): Unit = {
    val testee = new ProjectModTest.MockMod()
    Assert.assertEquals(Nil, testee.listGavsForCheck())
  }

  @Test
  def testListDependenciesForCheck(): Unit = {
    val testee = new ProjectModTest.MockMod() {

      override val selfDepsMod: Seq[ProjectMod.Dep] = Seq(
        Dep(SelfRef.parse("ou:x:x"), "gg", "a", "v", "", "", "", ""),
      )
      override val listDependencies: Seq[ProjectMod.Dep] = Seq(
        Dep(SelfRef.undef, "g", "a", "v", "", "", "", ""),
        Dep(SelfRef.undef, "g", "a", "v", "", "runtime", "", ""),
        Dep(SelfRef.undef, "g", "a", "v", "", "john", "", ""),
        Dep(SelfRef.undef, "g", "a", None, "", "john", "", ""),
        Dep(SelfRef.undef, "g", "a", "v", "", "", "", ""),
        Dep(SelfRef.parse("bert:x:x"), "g", "a", "v", "", "", "", ""),
        Dep(SelfRef.parse("se:x:x"), "gg", "a", "v", "", "", "", ""),
      )
    }
    Assert.assertEquals(Seq(
      Dep(SelfRef.undef, "g", "a", "v", "", "", "", "")
    ), testee.listGavsForCheck())

    Assert.assertEquals(Seq(
      Gav("g", "a", "v", scope = "john"),
      Gav("g", "a", None, scope = "john"),
    ), testee.listGavsWithUnusualScope())
  }

  @Test
  def testGav(): Unit = {
    Assert.assertTrue(Gav("g", "a", "v", scope = "john").feelsUnusual())
    Assert.assertFalse(Gav("g", "a", "v").feelsUnusual())
    Assert.assertTrue(Gav("g", "a", "()").feelsUnusual())
    Assert.assertTrue(Gav("g", "a", "RELEASE").feelsUnusual())
    Assert.assertTrue(Gav("g", "a", "LATEST").feelsUnusual())
  }

  @Test
  def testGroupSortReleases(): Unit = {

    val result = ProjectMod.groupSortReleases(Seq(
      "a",
      "1.1", "1-alpha",
      "2", "2.0",
      "5.10.0.202012080955-r", "5.9.0.202009080501-r",
      "10.0.0",
    ))

    Assert.assertEquals(Seq(
      ("10", Seq("10.0.0")),
      ("5", Seq("5.9.0.202009080501-r", "5.10.0.202012080955-r")),
      ("2", Seq("2", "2.0")),
      ("1", Seq("1-alpha", "1.1")),
      ("-1", Seq("a")),
    ), result)
  }

  @Test
  def testFilter(): Unit = {
    val in: Map[Gav3, (Seq[String], Duration)] = ListMap(
      Gav3(groupId = "g", artifactId = "a", version = "1.0.0") -> (Nil, Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a1", version = "1.0.0") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a2", version = "1.0.0-SNAPSHOT") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a3", version = "1.0.0-M1") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a4", version = "1.0.1-SNAPSHOT") -> (Seq("1.0.0", "1.0.1", "2.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a5", version = "1.0.1-M2") -> (Seq("2.0.0", "1.0.1", "1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a6", version = "ok") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a7", version = "ok") -> (Seq("1.0.0", "ok"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a8", version = "1.0.0-SNAPSHOT") -> (Seq("1.0.1"), Duration.ZERO),
    )
    val result = ProjectMod.reduceIn(in)
    val expected = ListMap(
      Gav3(groupId = "g", artifactId = "a", version = "1.0.0") -> (Nil, Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a1", version = "1.0.0") -> (Nil, Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a2", version = "1.0.0-SNAPSHOT") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a3", version = "1.0.0-M1") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a4", version = "1.0.1-SNAPSHOT") -> (Seq("1.0.1", "2.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a5", version = "1.0.1-M2") -> (Seq("1.0.1", "2.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a6", version = "ok") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a7", version = "ok") -> (Seq("1.0.0"), Duration.ZERO),
      Gav3(groupId = "g", artifactId = "a8", version = "1.0.0-SNAPSHOT") -> (Seq("1.0.1"), Duration.ZERO),
    )
    Assert.assertEquals(expected, result)
  }

  @Test
  def testShowDependencyUpdates_empty(): Unit = {
    val term = Term.select("xterm", "os", simpleChars = false, isInteractice = false)
    val rootDeps: Seq[ProjectMod.Dep] = Nil
    val selfDepsMod: Seq[ProjectMod.Dep] = Nil
    val repoMock = mock[Repo]
    val result = TermTest.withOutErr[Unit]()(sys => {
      val innerResult = ProjectMod.showDependencyUpdates(100, term,
        OptsDepUp(), rootDeps, selfDepsMod, Seq(repoMock), sys, printProgress = true, checkOnline = false)

      Assert.assertEquals(Nil, innerResult)
      val resultMod = new MockMod() {
        override lazy val repo: Repo = repoMock
      }.showDependencyUpdates(100, term, OptsDepUp(), sys, printProgress = true, checkOn = false)
      Assert.assertEquals(Nil, resultMod)
    })
    Assert.assertEquals("", result.err)
    val filteredOut = result.out
      .linesIterator
      .map(line => {
        if (line.matches(".* dependecies in [0-9]+ms \\(20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)$")) {
          line.replaceFirst(" in.*", " in ...")
        } else {
          line
        }
      })
      .mkString("\n")
    Assert.assertEquals(
      """
        |I: checking dependecies against nexus - please wait
        |I: checked 0 dependecies in ...
        |term: Term(xterm,os,false,false)
        |I: checking dependecies against nexus - please wait
        |I: checked 0 dependecies in ...
        |term: Term(xterm,os,false,false)
        |""".stripMargin.trim, filteredOut)

  }

}
