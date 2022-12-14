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
    in.replaceAll("- $", "-")
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
        |[[31mERROR[0m] ----------------------------[ end of lint ]----------------------------""".stripMargin
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
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[WARNING]  shallow clone detected 😬
        |[WARNING]  % git rev-parse --is-shallow-repository # returns true
        |[WARNING]    We do not want shallow clones because the commit id used in runtime
        |[WARNING]    info will not point to a known commit
        |[WARNING]    on Gitlab, change 'Settings' -> 'CI/CD' -> 'General pipelines' ->
        |[WARNING]      'Git shallow clone' to 0 or blank
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[INFO]     WIP ci path: null
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/any.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[ERROR] exit 42 - because lint found warnings, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, Nil, outFn = outT)(sys => {
      Assert.assertEquals(42, Lint.run(sys.out, sys.err, Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false)), fileB))
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
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[INFO]     WIP ci path: null
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- check for snapshots @ maven ---
        |[INFO]     WIP
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[ERROR] exit 42 - because lint found warnings, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, Nil, outFn = outT)(sys => {
      Assert.assertEquals(42, Lint.run(sys.out, sys.err, Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false)), file))
    })

  }
}
