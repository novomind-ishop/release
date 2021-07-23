package release

import org.junit.{Assert, Test}
import org.mockito.MockitoSugar.mock
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.OptsDepUp

class ProjectModTest extends AssertionsForJUnit {

  @Test
  def testScalaDeps(): Unit = {

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
    val aether = mock[Repo]
    val result = StarterTest.withOutErr[Unit]((out, err) => {
      ProjectMod.showDependencyUpdates(100, term,
        OptsDepUp(), "workingNExusUrl", rootDeps, selfDepsMod, aether, out, err)
    })
    Assert.assertEquals("", result.err)
    val filteredOut = result.out
      .linesIterator
      .map(line => {
        if (line.matches(".* dependecies in [0-9]+ms \\(20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)$")){
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
