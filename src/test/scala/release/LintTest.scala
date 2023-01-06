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
      .replaceAll("[a-f0-9]{40}$", "a79849c3042ef887a5477d73d958814317675be1")
      .replaceAll("dependecies in [1-9][0-9]*ms \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "dependecies in 999ms (2000-01-01)")
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
        |[WARNING]    % git rev-parse --is-shallow-repository # returns true
        |[WARNING]    % git log -n1 --pretty=%H # returns a79849c3042ef887a5477d73d958814317675be1
        |[WARNING]    We do not want shallow clones because the commit id used in runtime
        |[WARNING]    info will not point to a known commit
        |[WARNING]    on Gitlab, change 'Settings' -> 'CI/CD' -> 'General pipelines' ->
        |[WARNING]      'Git shallow clone' to 0 or blank.
        |[WARNING]      If this does not fix this warning, toggle
        |[WARNING]      the .. -> 'Git strategy' to 'git clone' for maybe a
        |[WARNING]      single build to wipe out gitlab caches.
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
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
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.11-SNAPSHOT</version>
        |  <dependencies>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context</artifactId>
        |      <version>1</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- check for snapshots @ maven ---
        |[INFO]     WIP
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9
        |║ ╠═══ (2) 2.0, .., 2.5.5, 2.5.6
        |║ ╠═══ (3) 3.0.0.RELEASE, .., 3.2.17.RELEASE, 3.2.18.RELEASE
        |║ ╠═══ (4) 4.0.0.RELEASE, .., 4.3.29.RELEASE, 4.3.30.RELEASE
        |║ ╠═══ (5) 5.0.0.RELEASE, .., 5.3.23, 5.3.24
        |║ ╚═══ (6) 6.0.0, .., 6.0.2, 6.0.3
        |║
        |term: Term(dumb,lint,false)
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
