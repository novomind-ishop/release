package release

import org.junit.Test
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.Gav3
import release.Starter.Opts
import release.TreeGavTest.replaceVarLiterals

import java.io.File
object TreeGavTest {
  def replaceVarLiterals(in: String): String = {
    in.replaceAll("- $", "-")
      .replaceAll("/junit[0-9]+/", "/junit-REPLACED/")
      .replaceAll("-Xms: [0-9]+m -Xmx: [0-9]+m", "-Xms: 123m -Xmx: 321m")
      .replaceAll("\\.scala:[0-9]+", ".scala:???")
      .replaceAll("\t", "  ")
      .replaceAll("( package name(?:s|) in) PT[0-9]+S", "$1 PT4S")
      .replaceAll("^\\[..:..:..\\...Z\\] ", "[00:00:00.00Z] ")
      .replaceAll(": git version 2\\.[0-9]+\\.[0-9]+", ": git version 2.999.999")
      .replaceAll("[a-f0-9]{40}$", "affe4533042ef887a5477d73d958814317675be1")
      .replaceAll("dependencies in [0-9\\.]+ [mμs]+ \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "dependencies in 999ms (2000-01-01)")
  }
}
class TreeGavTest extends AssertionsForJUnit {

  @Test
  def testRunEmpty(): Unit = {

    val value: Map[File, Seq[Gav3]] = Map(new File("a/dep.tree") -> Seq(
      Gav3(groupId = "g", artifactId = "a", version = Some("1.2.0"))
      
    ))
    val expected =
      """""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts().copy(lintOpts = Opts().lintOpts.copy(showTimer = true, showTimeStamps = true))
      TreeGav.format(value, sys.out, opts)
    })

  }

  @Test
  def testRunSingle(): Unit = {

    val value: Map[File, Seq[Gav3]] = Map(new File("a/dep.tree") -> Seq(
      Gav3(groupId = "g", artifactId = "a", version = Some("1.2.0")),
      Gav3(groupId = "g", artifactId = "a", version = Some("1.2.0")),
      Gav3(groupId = "g", artifactId = "a", version = Some("1.2.3")),

    ))
    val expected =
      """[warning]  g:a {1.2.0, 1.2.3}""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts().copy(colors = false)
      TreeGav.format(value, sys.out, opts)
    })

  }
}
