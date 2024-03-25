package release

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

import java.io.File
import java.time.ZonedDateTime

class LintSbtTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  def outT(in: String): String = {
    in.replaceAll("- $", "-")
      .replaceAll("/junit[0-9]+/", "/junit-REPLACED/")
      .replaceAll(": git version 2\\.[0-9]+\\.[0-9]+", ": git version 2.999.999")
      .replaceAll("[a-f0-9]{40}$", "affe4533042ef887a5477d73d958814317675be1")
      .replaceAll("dependencies in [0-9]+ms \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "dependencies in 999ms (2000-01-01)")
  }

  @Test
  def testWorkUrl(): Unit = {

    val remote = temp.newFolder("release-lint-sbt-simple-init")
    val fileB = temp.newFolder("release-lint-sbt-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val sbtFile = new File(remote, "build.sbt")
    Util.write(sbtFile,
      """scalaVersion := "2.13.10"
        |
        |version := "1.0-SNAPSHOT"
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(sbtFile)
    gitA.commitAll("bla")
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[INFO]     HEAD branch: master - affe4533042ef887a5477d73d958814317675be1
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 1
        |[INFO]       Your Name <you@example.com>
        |[INFO]     active branch count: 1 - master
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-sbt-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-sbt-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- project version @ maven ---
        |[INFO]     1.0-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |I: checking dependencies against nexus - please wait
        |
        |I: checked 2 dependencies in 999ms (2000-01-01)
        |║ Project GAV: X
        |╠═╦═ org.scala-lang:scala3-library_3:-1
        |║ ╚═══ 1.0.0 (libyears: 0.0 [0 days])
        |║
        |
        |libyears: 0.0 (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |[INFO] --- ??? @ sbt ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-sbt-simple/.git
        |/tmp/junit-REPLACED/release-lint-sbt-simple/build.sbt
        |[INFO] ----------------------------[ end of lint ]----------------------------""".stripMargin

    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Seq("1.0.0"))

      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, fileB))
    })

  }

}
