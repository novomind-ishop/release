package release

import org.junit.{Assert, Rule, Test}
import org.junit.rules.TemporaryFolder
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

import java.io.File

class LintTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  def outT(in: String): String = {
    in
      .replaceAll("/junit[0-9]+/", "/junit-REPLACED/")
      .replaceAll(": git version 2\\.[0-9]+\\.[0-9]+", ": git version 2.999.999")
  }

  @Test
  def testRunEmpty(): Unit = {
    val file = temp.newFolder("release-lint-empty")
    val expected =
      """
        |[[34mINFO[0m] --------------------------------[ lint ]--------------------------------
        |[[31mERROR[0m] E: NO FILES FOUND in /tmp/junit-REPLACED/release-lint-empty
        |[[31mERROR[0m] ----------------------------[ end of lint ]---------------------------- """.stripMargin
    TermTest.testSys(Nil, expected, Nil, outFn = outT)(sys => {
      Assert.assertEquals(1, Lint.run(sys.out, sys.err, Opts(), file))
    })

  }

  @Test
  def testRunMvnSimpleShallow(): Unit = {
    val fileA = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(fileA, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val any = new File(fileA, "any.xml")
    Util.write(any,
      """some
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(any)
    gitA.commitAll("bla")
    Util.write(any,
      """emos
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(any)
    gitA.commitAll("blub")
    val gitB = Sgit.doCloneRemote(fileA.toURI.toString.replaceFirst("file:/", "file:///"), fileB, depth = 1)

    val expected =
      """
        |[[34mINFO[0m] --------------------------------[ lint ]--------------------------------
        |[[34mINFO[0m]     ✅ git version: git version 2.999.999
        |[[34mINFO[0m] --- check clone config / no shallow clone @ git ---
        |[[33mWARNING[0m]  shallow clone detected 😬
        |[[33mWARNING[0m]  we do not want shallow clones because the commit id used in runtime
        |[[33mWARNING[0m]  info will not point to a known commit
        |[[33mWARNING[0m]  on Gitlab, change 'Settings' -> 'CI/CD' -> 'General pipelines' ->
        |[[33mWARNING[0m]    'Git shallow clone' to 0 or blank
        |[[34mINFO[0m] --- .gitattributes @ git ---
        |[[34mINFO[0m] --- .gitignore @ git ---
        |[[34mINFO[0m] --- gitlabci.yml @ gitlab ---
        |[[34mINFO[0m]     WIP ci path: null
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/any.xml
        |[[34mINFO[0m] ----------------------------[ end of lint ]---------------------------- 
        |[[31mERROR[0m] exit 42 - because lint found warnings, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, Nil, outFn = outT)(sys => {
      Assert.assertEquals(42, Lint.run(sys.out, sys.err, Opts(lintOpts = Opts().lintOpts.copy(showTimer = false)), fileB))
    })

  }

  @Test
  def testRunMvnSimple(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    Util.write(new File(file, "pom.xml"),
      """
        |""".stripMargin.linesIterator.toSeq)
    val expected =
      """
        |[[34mINFO[0m] --------------------------------[ lint ]--------------------------------
        |[[34mINFO[0m]     ✅ git version: git version 2.999.999
        |[[34mINFO[0m] --- check clone config / no shallow clone @ git ---
        |[[34mINFO[0m]     ✅ NO shallow clone
        |[[34mINFO[0m] --- .gitattributes @ git ---
        |[[34mINFO[0m] --- .gitignore @ git ---
        |[[34mINFO[0m] --- gitlabci.yml @ gitlab ---
        |[[34mINFO[0m]     WIP ci path: null
        |[[34mINFO[0m] --- .mvn @ maven ---
        |[[34mINFO[0m]     WIP
        |[[34mINFO[0m] --- check for snapshots @ maven ---
        |[[34mINFO[0m]     WIP
        |[[34mINFO[0m] --- suggest dependency updates / configurable @ maven ---
        |[[34mINFO[0m]     WIP
        |[[34mINFO[0m] --- dep.tree @ maven ---
        |[[34mINFO[0m]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[[34mINFO[0m] ----------------------------[ end of lint ]---------------------------- """.stripMargin
    TermTest.testSys(Nil, expected, Nil, outFn = outT)(sys => {
      Assert.assertEquals(0, Lint.run(sys.out, sys.err, Opts(lintOpts = Opts().lintOpts.copy(showTimer = false)), file))
    })

  }
}
