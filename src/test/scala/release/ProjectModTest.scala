package release

import org.junit.{Assert, Test}
import org.mockito.ArgumentMatchers.anyString
import org.mockito.MockitoSugar._
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Gav, GavWithRef, SelfRef}
import release.SbtModTest.d
import release.Starter.OptsDepUp

import java.time.{Duration, ZonedDateTime}

class ProjectModTest extends AssertionsForJUnit {

  @Test
  def testScalaDeps(): Unit = {
    val now = ZonedDateTime.now()
    val scala = d("org.scala-lang", "scala-library", "2.13.0")

    val term = Term.select("xterm", "os", simpleChars = false)
    val rootDeps: Seq[ProjectMod.Dep] = Seq(scala)
    val selfDepsMod: Seq[ProjectMod.Dep] = Nil
    val repo = mock[Repo]
    when(repo.getRelocationOf(anyString(), anyString(), anyString())).thenReturn(None)
    when(repo.newerVersionsOf(scala.groupId, scala.artifactId, scala.version)).thenReturn(Seq(scala.version, "2.13.1"))
    when(repo.depDate(scala.groupId, scala.artifactId, scala.version)).thenReturn(Some(now))
    when(repo.depDate(scala.groupId, scala.artifactId, "2.13.1")).thenReturn(Some(now))
    when(repo.newerVersionsOf(scala.groupId, "scala3-library_3", "-1")).thenReturn(Seq("-1", "3.0.1"))
    when(repo.depDate(scala.groupId, "scala3-library_3", "-1")).thenReturn(None)
    val result = TermTest.withOutErr[Unit]()(sys => {
      val innerResult: Seq[(GavWithRef, (Seq[String], Duration))] = ProjectMod.showDependencyUpdates(100, term,
        OptsDepUp().copy(showLibYears = true), () => "workingNexusUrl", rootDeps, selfDepsMod, repo, sys,
        printProgress = true, checkOnline = false)

      val sca1 = GavWithRef(SelfRef.undef, Gav("org.scala-lang", "scala-library", "2.13.0"))
      val scala2 = (sca1, (Seq("2.13.1"), Duration.ZERO))
      val sca2 = GavWithRef(SelfRef.undef, Gav("org.scala-lang", "scala3-library_3", "-1"))
      val scala3 = (sca2, (Seq("3.0.1"), Duration.ofDays(-1)))
      Assert.assertEquals(Seq(scala2, scala3), innerResult)
    })
    Assert.assertEquals("", result.err)
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
  def testShowDependencyUpdates_empty(): Unit = {
    val term = Term.select("xterm", "os", simpleChars = false)
    val rootDeps: Seq[ProjectMod.Dep] = Nil
    val selfDepsMod: Seq[ProjectMod.Dep] = Nil
    val repo = mock[Repo]
    val result = TermTest.withOutErr[Unit]()(sys => {
      val innerResult = ProjectMod.showDependencyUpdates(100, term,
        OptsDepUp(),  () => "workingNExusUrl", rootDeps, selfDepsMod, repo, sys, printProgress = true, checkOnline = false)
      Assert.assertEquals(Nil, innerResult)
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
        |term: Term(xterm,os,false)
        |""".stripMargin.trim, filteredOut)

  }

}
