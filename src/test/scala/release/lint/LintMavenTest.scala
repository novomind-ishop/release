package release.lint

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Ignore, Rule, Test}
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.lint.Lint.BranchTagMerge
import release.lint.{Lint, LintMaven}
import release.*

import java.io.File
import java.nio.file.Paths
import java.time.{YearMonth, ZonedDateTime}
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.{nowarn, unused}
import scala.util.{Failure, Success, Try}

object LintMavenTest {
  def replaceVarLiterals(in: String): String = {
    in.replaceAll("- $", "-")
      .replaceAll("/junit[0-9]+/", "/junit-REPLACED/")
      .replaceAll("-Xms: [0-9]+m -Xmx: [0-9]+m", "-Xms: 123m -Xmx: 321m")
      .replaceAll("\\.scala:[0-9]+", ".scala:???")
      .replaceAll("\t", "  ")
      .replaceAll("    used memory: [1-9][0-9]*m", "    used memory: ∞m")
      .replaceAll("( package name(?:s|) in) PT[0-9]+S", "$1 PT4S")
      .replaceAll("^\\[..:..:..\\...Z\\] ", "[00:00:00.00Z] ")
      .replaceAll(": git version 2\\.[0-9]+\\.[0-9]+", ": git version 2.999.999")
      .replaceAll("self version: [0-9a-f]+@[^ ]+$", "self version: 2222ffff")
      .replaceAll("[a-f0-9]{40}$", "affe4533042ef887a5477d73d958814317675be1")
      .replaceAll("dependencies in [0-9\\.]+ [mμs]+ \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "dependencies in 999ms (2000-01-01)")
      .replaceAll("^Σ libyears: (.*) \\[[0-9]{4}-[0-9]{2}-[0-9]{2}\\]$", "Σ libyears: $1 [2000-01-01]")
  }
}

class LintMavenTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  def replaceVarLiterals(in: String): String = LintMavenTest.replaceVarLiterals(in)

  @Test
  def testRunEmpty(): Unit = {
    val file = temp.newFolder("release-lint-empty")
    val expected =
      """
        |[00:00:00.00Z] [[34mINFO[0m] --------------------------------[ lint ]--------------------------------
        |[00:00:00.00Z] [[31mERROR[0m] E: NO FILES FOUND in /tmp/junit-REPLACED/release-lint-empty
        |[00:00:00.00Z] [[31mERROR[0m] ----------------------------[ end of lint ]----------------------------""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 1)(sys => {
      val opts = Opts().copy(lintOpts = Opts().lintOpts.copy(showTimer = true, showTimeStamps = true))
      sys.exit(Lint.run(sys.out, sys.err, opts, Map.empty, file))
    })

  }

  @Test
  def testRunMvnSimpleShallow(): Unit = {
    val remote = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val any = new File(remote, "any.xml")
    FileUtils.write(any,
      """some
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(any)
    gitA.commitAll("bla")
    FileUtils.write(any,
      """emos
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(any)
    gitA.commitAll("blub")
    @unused
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB, depth = 1)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[INFO]     HEAD branch: master - affe4533042ef887a5477d73d958814317675be1
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 1
        |[INFO]       Y‌o‌u‌r N‌a‌m‌e <‌y‌o‌u‌@‌e‌x‌a‌m‌p‌l‌e‌.‌c‌o‌m‌>
        |[INFO]     active branch count: 1 - master
        |[INFO]     approx. a new branch each: P0D, approx. a new tag each: P-1D
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[WARNING]  shallow clone detected 😬
        |[WARNING]    % git rev-parse --is-shallow-repository # returns true
        |[WARNING]    % git log -n1 --pretty=%H # returns
        |[WARNING]      affe4533042ef887a5477d73d958814317675be1
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
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (fetch)
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (push)
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/any.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      sys.exit(Lint.run(sys.out, sys.err, opts, Map.empty, fileB))
    })

  }

  @Test
  def testWorkUrl(): Unit = {

    val remote = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@hostname-without-tld")
    gitA.configSetLocal("user.name", "Your Name")
    val pom = new File(remote, "pom.xml")
    FileUtils.write(pom,
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
        |      <version>1.0.0</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(pom)

    {
      val tree = new File(remote, "dep.tree")
      FileUtils.write(tree,
        """some
          |""".stripMargin.linesIterator.toSeq)
      gitA.add(tree)  
    }
    
    val sub1 = new File(remote, "subfolder")
    sub1.mkdir()

    {
      val tree = new File(sub1, "dep.tree")
      FileUtils.write(tree,
        """some
          |""".stripMargin.linesIterator.toSeq)
      gitA.add(tree)
    }
    
    gitA.commitAll("bla")
    @unused
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     skips: RL1023-82a96f52
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[INFO]     HEAD branch: master - affe4533042ef887a5477d73d958814317675be1
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 1
        |[WARNING]       Y‌o‌u‌r N‌a‌m‌e <‌y‌o‌u‌@‌h‌o‌s‌t‌n‌a‌m‌e‌-‌w‌i‌t‌h‌o‌u‌t‌-‌t‌l‌d‌> // not valid mailbox TLD
        |[INFO]     active branch count: 1 - master
        |[INFO]     approx. a new branch each: P0D, approx. a new tag each: P-1D
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (fetch)
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (push)
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org # (no ip)
        |[WARNING]  nexus work url must end with a '/' - https://repo.example.org 😬 RL1001
        |... https://repo.example.org/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 1 dependencies in 999ms (2000-01-01)
        |Non existing dependencies for:
        |»org.springframework:spring-context:1.0.0« -> Nil
        |  RepoProxy: https://repo.example.orgorg/springframework/spring-context/maven-metadata.xml
        |
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |[WARNING]  found 2 orphan trees 😬
        |[WARNING]  orphan dep.tree RL1023-96763194 😬
        |[warning]  orphan subfolder/dep.tree RL1023-82a96f52 🤐
        |
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/dep.tree
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |/tmp/junit-REPLACED/release-lint-mvn-simple/subfolder
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1023-82a96f52")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/", "https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Nil)

      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, fileB))
    })

  }

  @Test
  def testCiVars(): Unit = {

    val remote = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    @unused
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB)
    val dockerile = new File(fileB, "Dockerfile")
    FileUtils.write(dockerile,
      """
        |FROM busybox:unstable
        |""".stripMargin.linesIterator.toSeq)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 unknown remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 HEAD branch: (unknown) - n/a
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-914782d6
        |[WARNING]  ?? Dockerfile 😬 RL1003-8faf89d2
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (fetch)
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (push)
        |[INFO] --- Dockerfile @ docker ---
        |[INFO]       Allowed docker hostnames are: .*
        |[INFO]       - : Dockerfile
        |[INFO]           ✅ FROM busybox:unstable
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[WARNING]    ci path: a
        |[WARNING]    use .gitlab-ci.yml 😬 RL1005
        |[INFO]       CI_COMMIT_TAG : vU
        |[INFO]       CI_COMMIT_REF_NAME : vU
        |[INFO]       CI_COMMIT_BRANCH :
        |[WARNING]    an INVALID branch/tag: ciRef: vU, ciTag: vU, ciBranch: , gitTags: , gitBranch:
        |[ERROR]      docker tag : auto suggested docker tag » vU « is no valid docker tag
        |[ERROR]        name. This could lead to build problems later. A git tag must match
        |[ERROR]        the pattern » ^v[0-9]+\.[0-9]+\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$ « to
        |[ERROR]        suggest valid docker tags. It is also possible to export an
        |[ERROR]        environment variable e.g. HARBOR_TAG ❌ RL1006
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/Dockerfile
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[ERROR] exit 43 - because lint found errors, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 43)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      @unused
      val mockRepo = Mockito.mock(classOf[Repo])

      val value = Map(
        "CI_CONFIG_PATH" -> "a",
        "CI_COMMIT_REF_NAME" -> "vU",
        "CI_COMMIT_TAG" -> "vU",
      )
      sys.exit(Lint.run(sys.out, sys.err, opts, value, fileB))
    })

  }

  @Test
  def testValid(): Unit = {

    val remote = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val pom = new File(remote, "pom.xml")
    FileUtils.write(pom,
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
        |      <version>1.0.0</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(pom)
    gitA.commitAll("bla")
    @unused
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[INFO]     HEAD branch: master - affe4533042ef887a5477d73d958814317675be1
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 1
        |[INFO]       Y‌o‌u‌r N‌a‌m‌e <‌y‌o‌u‌@‌e‌x‌a‌m‌p‌l‌e‌.‌c‌o‌m‌>
        |[INFO]     active branch count: 1 - master
        |[INFO]     approx. a new branch each: P0D, approx. a new tag each: P-1D
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (fetch)
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (push)
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |... https://repo.example.org/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 1 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1.0.0
        |║ ╚═══ 1.0.1, .., 1.2.8, 1.2.9 (libyears: ?? no date found for org.springframework:spring-context:1.2.9)
        |║
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts1 = Opts().lintOpts.copy(showTimer = false)
      val opts = Opts(colors = false, lintOpts = opts1)
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.eq("1.2.9"))).
        thenReturn(None)
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "0.9", "1.0.0", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, fileB))
    })

  }

  @Test
  def testFailOldMilestone(): Unit = {

    val remote = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val pom = new File(remote, "pom.xml")
    FileUtils.write(pom,
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
        |      <version>1.0.0-M1</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context</artifactId>
        |      <version>1.0.0-M1</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-vals</artifactId>
        |      <version>1.0.0-SNAPSHOT</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-vars</artifactId>
        |      <version>1.0.0-M1</version>
        |    </dependency>
        |  </dependencies>
        |    <dependencyManagement>
        |    <dependencies>
        |      <dependency>
        |        <groupId>org.springframework</groupId>
        |        <artifactId>spring-vals</artifactId>
        |        <version>1.0.0-SNAPSHOT</version>
        |      </dependency>
        |    </dependencies>
        |  </dependencyManagement>
        |   <build>
        |    <plugins>
        |      <plugin>
        |        <artifactId>example-maven-plugin</artifactId>
        |        <groupId>org.example.maven</groupId>
        |        <version>1.10.3</version>
        |        <dependencies>
        |          <dependency>
        |            <groupId>org.example</groupId>
        |            <artifactId>example</artifactId>
        |            <version>1.2.3</version>
        |          </dependency>
        |        </dependencies>
        |      </plugin>
        |      <plugin>
        |        <artifactId>example2-maven-plugin</artifactId>
        |        <groupId>org.example.maven</groupId>
        |        <version>1.10.3</version>
        |        <dependencies>
        |          <dependency>
        |            <groupId>org.example</groupId>
        |            <artifactId>example</artifactId>
        |            <version>3.2.1</version>
        |          </dependency>
        |        </dependencies>
        |      </plugin>
        |    </plugins>
        |  </build>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(pom)
    gitA.commitAll("bla")
    @unused
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     skips: RL10015-aa71e948, RL1017-ab101a0e, RL1018-ceefe9c6
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[INFO]     HEAD branch: master - affe4533042ef887a5477d73d958814317675be1
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 1
        |[INFO]       Y‌o‌u‌r N‌a‌m‌e <‌y‌o‌u‌@‌e‌x‌a‌m‌p‌l‌e‌.‌c‌o‌m‌>
        |[INFO]     active branch count: 1 - master
        |[INFO]     approx. a new branch each: P0D, approx. a new tag each: P-1D
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (fetch)
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (push)
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[WARNING]  found scopes/copies/overlapping 😬 RL1017-ab101a0e
        |[WARNING]  found copies, use only one dependency in com.novomind.ishop.any:any:0.11-SNAPSHOT
        |  org.springframework:spring-context:1.0.0-M1:compile (times 2). This can also happen if you override an unused dependencyManagement.
        |[WARNING]  found copies, use only one dependency in com.novomind.ishop.any:any:0.11-SNAPSHOT
        |  org.springframework:spring-vals:1.0.0-SNAPSHOT:compile (times 2). This can also happen if you override an unused dependencyManagement.
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[warning]   found snapshot: org.springframework:spring-vals:1.0.0-SNAPSHOT 😬 RL1011-eef87565
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[WARNING]   found preview: org.springframework:spring-context:1.0.0-M1 😬 RL1018-b40c1656
        |[WARNING]        next    : org.springframework:spring-context:1.0.1
        |[WARNING]        latest  : org.springframework:spring-context:1.2.9
        |[WARNING]        previous: org.springframework:spring-context:0.99.99
        |[warning]   found preview: org.springframework:spring-vars:1.0.0-M1 😬 RL1018-ceefe9c6
        |[warning]        previous: org.springframework:spring-vars:0.0.99
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |... https://repo.example.org/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 7 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.example:example:1.2.3
        |║ ╠═══ (3) 3.2.1 (libyears: 0Y 0M [0 days])
        |║ ╚═══ (99) 99.99.99 (libyears: 0Y 0M [0 days])
        |╠═╦═ org.example:example:3.2.1
        |║ ╚═══ 99.99.99 (libyears: 0Y 0M [0 days])
        |╠═╦═ org.example.maven:example-maven-plugin:1.10.3
        |║ ╚═══ 99.99.99 (libyears: 0Y 0M [0 days])
        |╠═╦═ org.example.maven:example2-maven-plugin:1.10.3
        |║ ╚═══ 99.99.99 (libyears: 0Y 0M [0 days])
        |╠═╦═ org.springframework:spring-context:1.0.0-M1
        |║ ╚═══ 1.0.1, .., 1.2.8, 1.2.9 (libyears: 0Y 0M [0 days])
        |╠═╦═ org.springframework:spring-vals:1.0.0-SNAPSHOT
        |║ ╚═══ 1.0.1, .., 1.2.8, 1.2.9 (libyears: 0Y 0M [0 days])
        |║
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL10015-aa71e948", "RL1017-ab101a0e", "RL1018-ceefe9c6")))
      val mockRepo = Mockito.mock(classOf[Repo])
      val mockRepo2 = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo2.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo2.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo, mockRepo2))
      Mockito.when(mockRepo2.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo, mockRepo2))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo, mockRepo2))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo2.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo2.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo2.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Nil)
      Mockito.when(mockRepo2.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Nil)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-vars"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "0.0.99", "1.0.0-M1"
        ))
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-vals"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.0.0-SNAPSHOT", "0.1", "1.0.1", "1.0.2", "1.2.8", "1.2.9"
        ))
      Mockito.when(mockRepo2.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-vals"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "0.1", "1.0.1", "1.0.2", "1.2.8", "1.2.9"
        ))
      Mockito.when(mockRepo2.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-context"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "0.99.99", "1.0.1", "1.0.2", "1.2.8", "1.2.9", "1.0.0-M1"
        ))
      Mockito.when(mockRepo2.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("example-maven-plugin"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.10.3", "99.99.99"
        ))
      Mockito.when(mockRepo2.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("example2-maven-plugin"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.10.3", "99.99.99"
        ))
      Mockito.when(mockRepo2.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("example"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.2.3", "99.99.99", "3.2.1"
        ))
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, fileB))
    })
  }

  @Test
  def testRunMvnMajorVersion(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(file, "pom.xml"),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.11-SNAPSHOT</version>
        |  <dependencies>
        |    <dependency>
        |      <groupId>com.novomind.ishop.core.other</groupId>
        |      <artifactId>other-context</artifactId>
        |      <version>50.2.3</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>com.novomind.ishop.core.some</groupId>
        |      <artifactId>core-some-context</artifactId>
        |      <version>51.2.3</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-3b946499
        |[WARNING]  ?? pom.xml 😬 RL1003-467ad8bc
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[WARNING]  project.version »0.11-SNAPSHOT« has no git. Please add some .git folder. 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[WARNING]     Found multiple core major version: »50, 51«, use only one 😬 RL1013-200c4b3d
        |[WARNING]       - 50 -
        |[WARNING]       com.novomind.ishop.core.other:other-context:50.2.3 😬 RL1013-253fb8cd
        |[WARNING]       - 51 -
        |[WARNING]       com.novomind.ishop.core.some:core-some-context:51.2.3 😬 RL1013-235d0058
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |... https://repo1.maven.org/maven2/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 2 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ com.novomind.ishop.core.other:other-context:50.2.3
        |║ ╠═══ (50) 50.4.0 (libyears: 0Y 0M [0 days])
        |║ ╚═══ (51) 51.0.0, 51.2.5 (libyears: 0Y 0M [0 days])
        |╠═╦═ com.novomind.ishop.core.some:core-some-context:51.2.3
        |║ ╚═══ 51.2.5 (libyears: 0Y 0M [0 days])
        |║
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("other-context"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "51.0.0", "51.2.5",
          "50.4.0", "50.2.3",
          "0.0.1"
        ))
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("core-some-context"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "51.0.0", "51.2.5", "51.2.3",
        ))
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testRunMvnMajorVersionNo(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(file, "pom.xml"),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.11-SNAPSHOT</version>
        |  <dependencies>
        |    <dependency>
        |      <groupId>com.novomind.ishop.core.other</groupId>
        |      <artifactId>other-context</artifactId>
        |      <version>50x-SNAPSHOT</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>com.novomind.ishop.core.some</groupId>
        |      <artifactId>core-some-context</artifactId>
        |      <version>50x-SNAPSHOT</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-3b946499
        |[WARNING]  ?? pom.xml 😬 RL1003-467ad8bc
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[WARNING]  project.version »0.11-SNAPSHOT« has no git. Please add some .git folder. 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[warning]   found snapshot: com.novomind.ishop.core.other:other-context:50x-SNAPSHOT 😬 RL1011-08f0f8bf
        |[warning]   found snapshot: com.novomind.ishop.core.some:core-some-context:50x-SNAPSHOT 😬 RL1011-9f269959
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |... https://repo.example.org/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 2 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ com.novomind.ishop.core.other:other-context:50x-SNAPSHOT
        |║ ╚═══ 50.2.3, 50.4.0 (libyears: ?? libyears are not computed for com.novomind.ishop.core.other:other-context:50.4.0)
        |╠═╦═ com.novomind.ishop.core.some:core-some-context:50x-SNAPSHOT
        |║ ╚═══ 50.2.3, 50.4.0 (libyears: ?? libyears are not computed for com.novomind.ishop.core.some:core-some-context:50.4.0)
        |║
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "49.4.0", "50.4.0", "50.2.3", "50x-SNAPSHOT"
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testRunMvnSimple(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(file, "pom.xml"),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.14-SNAPSHOT</version>
        |  <dependencies>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context</artifactId>
        |      <version>1</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    @unused
    val notes = new File(file, "notes.md")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     skips: RL1003-467ad8bc
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.14-SNAPSHOT
        |[WARNING]  project.version »0.14-SNAPSHOT« has no git. Please add some .git folder. 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |... https://repo1.maven.org/maven2/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 1 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.14-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9 (libyears: 0Y 0M [0 days])
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6 (libyears: 0Y 0M [0 days])
        |║
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1003-467ad8bc")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "0.1",
        "1",
        "1.0.1", "1.0.2", "1.2.8", "1.2.9",
        "2.0", "2.1.1", "2.5.5", "2.5.6",
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testSnapshotTypo(): Unit = {
    val file = temp.newFolder("release-lint-mvn-snapshot")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(file, "pom.xml"),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.11-snapshot</version>
        |</project>
        |""".stripMargin.linesIterator.toSeq)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-3b946499
        |[WARNING]  ?? pom.xml 😬 RL1003-467ad8bc
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[ERROR]     😬 invalid SNAPSHOT definition in »0.11-snapshot«. SNAPSHOT has to be in upper case.
        |[WARNING]     skipped because of previous problems - invalid SNAPSHOT definition in »0.11-snapshot«. SNAPSHOT has to be in upper case. 😬
        |
        |/tmp/junit-REPLACED/release-lint-mvn-snapshot/.git
        |/tmp/junit-REPLACED/release-lint-mvn-snapshot/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[ERROR] exit 43 - because lint found errors, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 43)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Seq("0.0.1", "1.0.1-SNAPSHOT", "1.0.0"))
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testEmptyVersions(): Unit = {
    val file = temp.newFolder("release-lint-mvn-empty")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(file, "pom.xml"),
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
        |      <version>1.0.1-SNAPSHOT</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-raw-context</artifactId>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context-range</artifactId>
        |      <version>(,1.0],[1.2,)</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context-release</artifactId>
        |      <version>RELEASE</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context-latest</artifactId>
        |      <version>LATEST</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context-empty</artifactId>
        |      <version></version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context-blank</artifactId>
        |      <version />
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-3b946499
        |[WARNING]  ?? pom.xml 😬 RL1003-467ad8bc
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[WARNING]  project.version »0.11-SNAPSHOT« has no git. Please add some .git folder. 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[warning]   found snapshot: org.springframework:spring-context:1.0.1-SNAPSHOT 😬 RL1011-ea7ea019
        |[INFO] --- check for GAV format @ maven ---
        |[WARNING] »org.springframework:spring-context-range:(,1.0],[1.2,)« uses version with range »(,1.0],[1.2,)«. Please only use a single version. 😬 RL1010-d924ddfe
        |[WARNING] »org.springframework:spring-context-release:RELEASE« uses version »RELEASE« that is part of unstable markers LATEST and RELEASE. Please use unambiguous versions only. 😬 RL1010-7dd44fdb
        |[WARNING] »org.springframework:spring-context-latest:LATEST« uses version »LATEST« that is part of unstable markers LATEST and RELEASE. Please use unambiguous versions only. 😬 RL1010-c144cf49
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |... https://repo1.maven.org/maven2/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 4 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context-latest:LATEST
        |║ ╠═══ (0) 0.0.1 (libyears: ????)
        |║ ╚═══ (1) 1.0.0 (libyears: ????)
        |╠═╦═ org.springframework:spring-context-range:(,1.0],[1.2,)
        |║ ╠═══ (0) 0.0.1 (libyears: ????)
        |║ ╚═══ (1) 1.0.0 (libyears: ????)
        |╠═╦═ org.springframework:spring-context-release:RELEASE
        |║ ╠═══ (0) 0.0.1 (libyears: ????)
        |║ ╚═══ (1) 1.0.0 (libyears: ????)
        |║
        |[WARNING]  Dependency updated failed
        |[ERROR]    invalid empty versions:
        |org.springframework:spring-context-empty
        |org.springframework:spring-context-blank
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |
        |/tmp/junit-REPLACED/release-lint-mvn-empty/.git
        |/tmp/junit-REPLACED/release-lint-mvn-empty/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[ERROR] exit 43 - because lint found errors, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 43)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Seq("0.0.1", "1.0.1-SNAPSHOT", "1.0.0"))
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })
  }

  @Test
  def testTimeout(): Unit = {
    val file = temp.newFolder("release-lint-mvn-empty")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- project version @ maven ---
        |[INFO]     null
        |[WARNING]  version »null« must be a SNAPSHOT; non snapshots are only allowed in tags 😬
        |[WARNING]  project.version »null« has no git. Please add some .git folder. 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |
        |[WARNING]  Dependency updated failed
        |[WARNING] java.util.concurrent.TimeoutException a
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |
        |/tmp/junit-REPLACED/release-lint-mvn-empty/.git
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 44 - because lint found warnings and wants to retry, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 44)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])

      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      val mod = Mockito.mock(classOf[ProjectMod])
      Mockito.when(mod.repo).thenReturn(mockRepo)
      Mockito.when(mod.listDependencies).thenReturn(Nil)
      Mockito.when(mod.listDependenciesPlugin).thenReturn(Nil)
      Mockito.when(mod.listRawDeps).thenReturn(Nil)
      Mockito.when(mod.listGavsForCheck()).thenReturn(Nil)
      Mockito.when(mod.listGavsWithUnusualScope()).thenReturn(Nil)
      Mockito.when(mod.listRemoteRepoUrls()).thenReturn(Nil)
      Mockito.when(mod.getDepTreeFileContents).thenReturn(Map.empty)
      Mockito.when(mod.listGavsForCheck()).thenReturn(Nil)
      Mockito.when(mod.selfDepsMod).thenReturn(Nil)

      Mockito.when(mod.tryCollectDependencyUpdates(ArgumentMatchers.any(), ArgumentMatchers.anyBoolean(),
        ArgumentMatchers.any(), ArgumentMatchers.anyString(), ArgumentMatchers.any())).thenReturn(Failure(new java.util.concurrent.TimeoutException("a")))

      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file, mockMod = Some(Success(mod))))
    })
  }

  @Test
  def testSnapUpdateWithDates(): Unit = {
    val root = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(root, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(root, "pom.xml"),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.11.0</version>
        |  <dependencies>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-context</artifactId>
        |      <version>1.0.1-SNAPSHOT</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-single</artifactId>
        |      <version>1.0.1</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-raw-context</artifactId>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-other</artifactId>
        |      <version>1.0.0-SNAPSHOT</version>
        |      <scope>bert</scope>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-other2</artifactId>
        |      <version>1.0.0-SNAPSHOT </version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    val notes = new File(root, "notes.md")
    val seq =
      """
        |This is the documentation for 0.11-SNAPSHOT
        |This is the documentation for 0.11-SNAPSHOT
        |""".stripMargin.linesIterator.toSeq
    FileUtils.write(notes, seq)
    val dockerile = new File(root, "Dockerfile")
    FileUtils.write(dockerile,
      """
        |FROM
        |""".stripMargin.linesIterator.toSeq)
    val folder1 = new File(root, "folder1")
    folder1.mkdir()
    val file1 = new File(folder1, "test.txt")
    FileUtils.write(file1, Seq("content"))
    gitA.add(notes)
    gitA.add(dockerile)
    gitA.add(FileUtils.write(new File(folder1, "Demo.java"), // TODO valid folder
      """package some;
        |""".stripMargin.linesIterator.toSeq))
    val unwantedPackages = new File(root, ".unwanted-packages")
    FileUtils.write(unwantedPackages,
      """a.b;
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(unwantedPackages)
    gitA.add(file1)
    gitA.commitAll("some")
    gitA.doTag("0.11.0")
    gitA.checkout("v0.11.0")
    gitA.createBranch("ref")
    FileUtils.write(file1, Seq("content1"))
    val file2 = new File(folder1, "test2.txt")
    FileUtils.write(file2, Seq("content1"))
    Assert.assertTrue(gitA.currentTags.isDefined)
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     skips: RL1012-587ee13e, RL1003-8a73d4ae
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 1
        |[INFO]       Y‌o‌u‌r N‌a‌m‌e <‌y‌o‌u‌@‌e‌x‌a‌m‌p‌l‌e‌.‌c‌o‌m‌>
        |[INFO]     active branch count: 2 - master, ref
        |[INFO]     approx. a new branch each: P0D, approx. a new tag each: P-1D
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO]     current git tags: v0.11.0
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-8acb7584
        |[WARNING]  ?? folder1/ 😬 RL1003-9fe48cca
        |[WARNING]  ?? folder1/test2.txt 😬 RL1003-ac1cfa8d
        |[WARNING]  ?? pom.xml 😬 RL1003-467ad8bc
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- Dockerfile @ docker ---
        |[INFO]       Allowed docker hostnames are: .*
        |[INFO]       - : Dockerfile
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[INFO]       ci path: .gitlab-ci.yml
        |[INFO]       CI_COMMIT_TAG : v0.11.0
        |[INFO]       CI_COMMIT_REF_NAME : v0.11.0
        |[INFO]       CI_COMMIT_BRANCH :
        |[INFO]       a valid tag : v0.11.0
        |[INFO]       a valid semver tag? : v0.11.0
        |[INFO]       docker tag : v0.11.0
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11.0
        |[INFO] --- check for snapshots @ maven ---
        |[WARNING]   found snapshot: org.springframework:spring-context:1.0.1-SNAPSHOT 😬 RL1011-ea7ea019
        |[WARNING]   found snapshot: org.springframework:spring-other:1.0.0-SNAPSHOT:bert 😬 RL1011-bd849fd4
        |[INFO] --- check for GAV format @ maven ---
        |[WARNING] »org.springframework:spring-other:1.0.0-SNAPSHOT:bert« uses unknown scope »bert« please use only known scopes: compile, import, provided, runtime, system, test. 😬 RL1010-12cb6670
        |[WARNING] »org.springframework:spring-other2:1.0.0-SNAPSHOT « uses version with unknown symbol »1.0.0-SNAPSHOT␣«. Please remove unknown symbols. 😬 RL1010-0d88b10c
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |... https://repo1.maven.org/maven2/
        |I: checking dependencies against binary repository - please wait
        |
        |I: checked 4 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11.0
        |╠═╦═ org.springframework:spring-context:1.0.1-SNAPSHOT
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9 (libyears: 0Y 0M [1 days])
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6 (libyears: 0Y 0M [2 days])
        |╠═╦═ org.springframework:spring-other:1.0.0-SNAPSHOT:bert
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9 (libyears: 0Y 0M [7 days])
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6 (libyears: 0Y 0M [14 days])
        |╠═╦═ org.springframework:spring-single:1.0.1
        |║ ╚═══ 1.0.2 (libyears: 2Y 10M [1049 days])
        |║
        |Non existing dependencies for:
        |»org.springframework:spring-other2:1.0.0-SNAPSHOT « -> Nil
        |  RepoProxy: https://repo1.maven.org/maven2/org/springframework/spring-other2/maven-metadata.xml
        |
        |
        |Σ libyears: 2Y 10M (1065 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[WARNING] org.springframework:spring-context:1.0.1-SNAPSHOT is already released, remove '-SNAPSHOT' suffix 😬 RL1009-ea7ea019
        |[WARNING] org.springframework:spring-other:1.0.0-SNAPSHOT is not released, but next release (1.0.1) was found (maybe orphan snapshot) 😬 RL10015-f0a969b5
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |[INFO] --- unwanted-packages @ ishop ---
        |[INFO]     found 1 package name in PT4S
        |[INFO]     ✅ no problematic packages found
        |[INFO]     .unwanted-packages // checksum f466d6ea
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.unwanted-packages
        |/tmp/junit-REPLACED/release-lint-mvn-simple/Dockerfile
        |/tmp/junit-REPLACED/release-lint-mvn-simple/folder1
        |/tmp/junit-REPLACED/release-lint-mvn-simple/notes.md
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1012-587ee13e", "RL1003-8a73d4ae")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "200"))
      val now = ZonedDateTime.parse("2018-05-31T00:10:52+00:00")
      val now2 = ZonedDateTime.parse("2023-05-31T00:10:52+00:00")
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Some(now))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-context"), ArgumentMatchers.eq("1.0.1-SNAPSHOT")))
        .thenReturn(Some(now))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-context"), ArgumentMatchers.eq("1.2.9")))
        .thenReturn(Some(now.plusDays(1)))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-context"), ArgumentMatchers.eq("2.5.6")))
        .thenReturn(Some(now.plusDays(2)))

      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-other"), ArgumentMatchers.eq("1.2.9")))
        .thenReturn(Some(now.plusDays(7)))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-other"), ArgumentMatchers.eq("2.5.6")))
        .thenReturn(Some(now.plusDays(14)))

      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-single"), ArgumentMatchers.eq("1.0.1")))
        .thenReturn(Some(now2))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-single"), ArgumentMatchers.eq("1.0.2")))
        .thenReturn(Some(now2.plusDays(1049)))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Nil)

      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-single"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.0.1",
          "1.0.2",
        ))

      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.eq("org.springframework"), ArgumentMatchers.eq("spring-other2"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.0.0-SNAPSHOT",
        ))
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-other"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.0.0-SNAPSHOT",
          "0.0.2", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
          "2.0", "2.1.1", "2.5.5", "2.5.6",
        ))

      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-context"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.0.1-SNAPSHOT",
          "0.0.2", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
          "2.0", "2.1.1", "2.5.5", "2.5.6",
        ))

      val value = Map(
        "CI_CONFIG_PATH" -> ".gitlab-ci.yml",
        "CI_COMMIT_REF_NAME" -> "v0.11.0",
        "CI_COMMIT_TAG" -> "v0.11.0",
      )
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), value, root))
    })

  }

  @Test
  def testRunMvnParent(): Unit = {
    val file = temp.newFolder("release-lint-mvn-parent")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(file, "pom.xml"),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>ert</artifactId>
        |  <version>0.11-SNAPSHOT</version>
        |  <packaging>pom</packaging>
        |  <modules>
        |    <module>bert</module>
        |  </modules>
        |</project>
        |""".stripMargin.linesIterator.toSeq)

    val bertFolder = new File(file, "bert")
    bertFolder.mkdir()
    val bertPom = new File(bertFolder, "pom.xml")
    FileUtils.write(bertPom,
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <parent>
        |    <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>ert</artifactId>d>
        |    <version>0.11-SNAPSHOT</version>
        |  </parent>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>uert</artifactId>
        |  <version>0.11-SNAPSHOT</version>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(bertPom)

    val unwantedPackages = new File(file, ".unwanted-packages")
    FileUtils.write(unwantedPackages,
      """a.b
        |a.b.c
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(unwantedPackages)
    gitA.add(FileUtils.write(new File(bertFolder, "Demo.java"), // TODO valid folder
      """package a.b;
        |""".stripMargin.linesIterator.toSeq))

    gitA.add(FileUtils.write(new File(bertFolder, "Demo.scala"), // TODO valid folder
      """package c.b
        |""".stripMargin.linesIterator.toSeq))
    gitA.commitAll("some")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     no skips
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 1
        |[INFO]       Y‌o‌u‌r N‌a‌m‌e <‌y‌o‌u‌@‌e‌x‌a‌m‌p‌l‌e‌.‌c‌o‌m‌>
        |[INFO]     active branch count: 1 - master
        |[INFO]     approx. a new branch each: P-1D, approx. a new tag each: P-1D
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-3b946499
        |[WARNING]  ?? pom.xml 😬 RL1003-467ad8bc
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:ert
        |[INFO]     com.novomind.ishop.any:uert
        |[WARNING]    »com.novomind.ishop.any:ert« (in pom.xml) is too similar to
        |[WARNING]      »com.novomind.ishop.any:uert« (in bert/pom.xml). Please choose
        |[WARNING]      distinguishable names. 😬 RL1021-5dc541c2
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |... https://repo.example.org/
        |I: checking dependencies against binary repository - please wait
        |I: checked 0 dependencies in 999ms (2000-01-01)
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |[INFO] --- unwanted-packages @ ishop ---
        |[INFO]     found 2 package names in PT4S
        |[WARNING]  package »a.b;« is in list of unwanted packages, please avoid this package 😬 RL1019-e215bd6f
        |[INFO]     .unwanted-packages // checksum b0af4bf2
        |
        |/tmp/junit-REPLACED/release-lint-mvn-parent/.git
        |/tmp/junit-REPLACED/release-lint-mvn-parent/.unwanted-packages
        |/tmp/junit-REPLACED/release-lint-mvn-parent/bert
        |/tmp/junit-REPLACED/release-lint-mvn-parent/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "202"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))

    })
  }

  @Test
  def testRunMvnSimpleFail(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple-fail")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    @nowarn("msg=possible missing interpolator")
    val content =
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
        |      <artifactId>spring-non</artifactId>
        |      <version>${non-existing}</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq
    FileUtils.write(new File(file, "pom.xml"), content)
    val notes = new File(file, "notes.md")
    FileUtils.write(notes,
      """
        |This is the documentation for 0.11-SNAPSHOT
        |This is the documentation for 0.11-SNAPSHOT
        |""".stripMargin.linesIterator.toSeq)
    val dotMvnFolder = new File(file, ".mvn")
    dotMvnFolder.mkdir()
    val extension = new File(dotMvnFolder, "extensions.xml")
    FileUtils.write(extension,
      """<version>0.11-SNAPSHOT</version>
        |""".stripMargin
    )
    FileUtils.write(new File(file, "README.md"),
      """hello
        |""".stripMargin
    )
    gitA.add(notes)
    gitA.add(extension)
    gitA.commitAll("some")
    @nowarn("msg=possible missing interpolator")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     skips: RL1012-d143f8dc, RL1003-b4b0c08b
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 1
        |[INFO]       Y‌o‌u‌r N‌a‌m‌e <‌y‌o‌u‌@‌e‌x‌a‌m‌p‌l‌e‌.‌c‌o‌m‌>
        |[INFO]     active branch count: 1 - master
        |[INFO]     approx. a new branch each: P-1D, approx. a new tag each: P-1D
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-3b946499
        |[WARNING]  ?? pom.xml 😬 RL1003-467ad8bc
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[warning]   found snapshot in: notes.md line: 3 😬 RL1012-1805c642
        |              This is the documentation for 0.11-SNAPSHOT
        |[warning]   found snapshots: 😬 RL1012-587ee13e -- RL1012-1805c642
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[WARNING]     😬 No property replacement found in pom.xmls for: "${non-existing}" - define properties where they are required and not in parent pom.xml. Input is Nil.
        |[WARNING]     skipped because of previous problems - No property replacement found in pom.xmls for: "${non-existing}" - define properties where they are required and not in parent pom.xml. Input is Nil. 😬
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/.mvn
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/README.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/notes.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1012-d143f8dc", "RL1003-b4b0c08b")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "202"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      sys.exit(Lint.run(sys.out, sys.err, opts, Map.empty, file))

    })
  }

  @Test
  @Ignore
  def testOnLocal(): Unit = {
    val file = new File("/mnt/c/Users/tstock/git/cbr")
    TermTest.testSys(Nil, "", "", expectedExitCode = 42)(sys => {
      Lint.run(sys.out, sys.err, Opts().copy(colors = false), Map.empty, file)
    })
  }


  @Test
  def testFindAllPackagenames(): Unit = {
    val file = temp.newFolder("release-lint-find-packagenames")

    FileUtils.write(new File(file, ".unwanted-packages"),
      """a
        |""".stripMargin.linesIterator.toSeq)
    FileUtils.write(new File(file, "Blank.java"),
      "".linesIterator.toSeq)
    FileUtils.write(new File(file, "ATest.java"),
      """pac
        |""".stripMargin.linesIterator.toSeq)
    FileUtils.write(new File(file, "Test.scala"),
      """package b
        |""".stripMargin.linesIterator.toSeq)
    FileUtils.write(new File(file, "Test.java"),
      """// no
        |package a;
        |""".stripMargin.linesIterator.toSeq)

    FileUtils.write(new File(file, "Some.java"),
      """
        |package a.b;
        |package c.b;
        |
        |import java.io.File;
        |import static java.util.Set;
        |importReport.call();
        |packageList.call();
        |""".stripMargin.linesIterator.toSeq)

    val result = Lint.findAllPackagenames(file, check = true)
    println(result.d)
    Assert.assertEquals(Seq(
      ("package a.b;", Paths.get("Some.java")),
      ("package a;", Paths.get("Test.java")),
      ("package b", Paths.get("Test.scala")),

    ), result.packagesWithSrcPath)
    Assert.assertEquals(Seq("a.b;", "a;"), result.unwantedPackages)
    Assert.assertEquals(Seq("package a.b;", "package a;", "package b"), result.packages)
    Assert.assertEquals(Seq("import java.io.File;", "import static java.util.Set;"), result.imports)
  }

  @Test
  def testDiffPackageAndImports(): Unit = {
    val fileA = new File("/tmp/a")
    val fileB = new File("/tmp/b")
    val file = temp.newFolder("release-lint-diff-packagenames")

    val wf = FileUtils.write(new File(file, ".unwanted-packages"),
      """a
        |""".stripMargin.linesIterator.toSeq)


    val resultA = Lint.findPackagesAndImports(fileA, wf)
    println("packages")
    resultA.packages.foreach(println)
    val result = Lint.findPackagesAndImports(fileB, wf)
    println("imports")
    result.importsNom.filter(_.startsWith("com.novomind.")).foreach(println)
    Assert.assertEquals(Nil, result.unwantedPackages)
  }

  @Test
  def testValidMergeRequest(): Unit = {
    val file = temp.newFolder("release-lint-mvn-valid-branch")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", null, null, null, currentTagsIn = Nil)))
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", "", null, "", currentTagsIn = Nil)))
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val work = new File(file, "pom.xml")
    FileUtils.write(work,
      """<?xml version="1.0" encoding="UTF-8"?>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(work)
    gitA.commitAll("test")
    gitA.createBranch("work")
    gitA.checkout("work")
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", null, null, null, currentTagsIn = Nil)))
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", "", null, "", currentTagsIn = Nil)))

    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", null, gitA.currentBranchOpt, null, currentTagsIn = gitA.currentTags.getOrElse(Nil))))
    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "", currentTagsIn = gitA.currentTags.getOrElse(Nil))))
  }

  @Test
  def testValidBranch(): Unit = {
    val file = temp.newFolder("release-lint-mvn-valid-branch")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", null, gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val work = new File(file, "pom.xml")
    FileUtils.write(work,
      """<?xml version="1.0" encoding="UTF-8"?>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(work)
    gitA.commitAll("test")
    gitA.createBranch("work")
    gitA.checkout("work")
    gitA.checkout("HEAD~0")
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", null, gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))

    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", null, gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))
    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))

    gitA.doTag("work-20315-gcb5491b407")
    Assert.assertEquals(Some(Seq("vwork-20315-gcb5491b407")), gitA.currentTags)

    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))
    gitA.deleteTag("work-20315-gcb5491b407")
    gitA.doTag("20.0.0.some-6439-g800ef8fee7")
    Assert.assertEquals(Some(Seq("v20.0.0.some-6439-g800ef8fee7")), gitA.currentTags)
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work",
      currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))))

    gitA.deleteTag("20.0.0.some-6439-g800ef8fee7")

    gitA.doTag("1.2.3")
    Assert.assertEquals(Some(Seq("v1.2.3")), gitA.currentTags)
    val maybeMerge = Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))
    Assert.assertFalse(Lint.isValidBranch(maybeMerge))
    Assert.assertFalse(Lint.isValidTag(maybeMerge))
    gitA.deleteTag("1.2.3")

    gitA.doTag("1.2.3", msg = "handle annotated tags")
    Assert.assertEquals(Some(Seq("v1.2.3")), gitA.currentTags)
    val olo = Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTagsWithoutAnnotated.getOrElse(Nil))
    Assert.assertTrue(Lint.isValidBranch(olo))
    Assert.assertFalse(Lint.isValidTag(olo))
  }

  @Test
  def testValidDetachedTag(): Unit = {
    val file = temp.newFolder("release-lint-mvn-valid-tag")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val work = new File(file, "pom.xml")
    FileUtils.write(work,
      """<?xml version="1.0" encoding="UTF-8"?>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(work)
    gitA.commitAll("test")
    gitA.doTag("1.0.0")
    gitA.checkout("v1.0.0")
    Assert.assertTrue(Lint.isValidTag(Lint.toBranchTag("v1.0.0", "v1.0.0", gitA.currentBranchOpt, null, currentTagsIn = gitA.currentTags.getOrElse(Nil))))
    val tagDef = Lint.toBranchTag("v1.0.0", "v1.0.0", currentBranchOpt = Some("HEAD"), "HEAD", currentTagsIn = gitA.currentTags.getOrElse(Nil))
    Assert.assertEquals(Some(BranchTagMerge(tagName = Some("v1.0.0"), branchName = Some("HEAD"), info = "")), tagDef)
    Assert.assertTrue(Lint.isValidTag(tagDef))

    Assert.assertFalse(Lint.isValidBranch(Lint.toBranchTag("v1.0.0", "v1.0.0", gitA.currentBranchOpt, null, currentTagsIn = gitA.currentTags.getOrElse(Nil))))
  }

  @Test
  def testNo(): Unit = {
    val file = temp.newFolder("release-lint-mvn-deploy-none")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    FileUtils.write(new File(file, "pom.xml"),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.11-SNAPSHOT</version>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    val dotMvnFolder = new File(file, ".mvn")
    dotMvnFolder.mkdir()
    FileUtils.write(new File(dotMvnFolder, "extensions.xml"),
      """<version>0.11-SNAPSHOT</version>
        |""".stripMargin
    )
    @unused
    val branchName = "feature/bre"
    // gitA.createBranch(branchName)
    //gitA.checkout(branchName)
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_WARN_TO_ERROR ---
        |[INFO]     -Xms: 123m -Xmx: 321m
        |[INFO]     used memory: ∞m
        |[INFO]     skips: RL1003-21ee7891, RL1003-aaaaaaa
        |[INFO] --- version / git ---
        |[INFO]     ✅ git  version: git version 2.999.999
        |[INFO]     ✅ self version: 2222ffff
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active (last 14 days) contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[INFO]       ci path: .gitlab-ci.yml
        |[INFO]       CI_COMMIT_TAG :
        |[INFO]       CI_COMMIT_REF_NAME : feature/bre
        |[INFO]       CI_COMMIT_BRANCH :
        |[INFO]       a valid merge request : feature/bre
        |[warning]    no docker tag : no Dockerfiles
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[INFO]     ✅ successfull created
        |[INFO] --- dependency scopes/copies/overlapping @ maven/sbt ---
        |[INFO]     ✅ no warnings found
        |[INFO] --- artifactnames @ maven ---
        |[INFO]     com.novomind.ishop.any:any
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- version skew ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |... https://repo.example.org/
        |I: checking dependencies against binary repository - please wait
        |I: checked 0 dependencies in 999ms (2000-01-01)
        |
        |Σ libyears: 0Y 0M (0 days) [2000-01-01]
        |RepoMetrics(dateCollection = PT0S, dateCollectionCount = 0, versionCollection = PT0S, versionCollectionCount = 0)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     found 0 trees
        |[INFO] --- skip-conf / self / end ---
        |[WARNING]     found unused skips, please remove from your config: RL1003-aaaaaaa
        |[WARNING]     active skips: RL1003-21ee7891
        |
        |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/.git
        |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/.mvn
        |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[INFO]     used memory: ∞m
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1003-21ee7891", "RL1003-aaaaaaa")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.getMetrics).thenReturn(RepoMetrics.empty())
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.allRepoZ()).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(ArgumentMatchers.eq(false), ArgumentMatchers.any())).thenReturn(Repo.ReachableResult(online = true, "202"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val env = Map(
        "CI_CONFIG_PATH" -> ".gitlab-ci.yml",
        "CI_COMMIT_REF_NAME" -> "feature/bre",
        "CI_COMMIT_TAG" -> "")
      sys.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), env, file))

    })
  }

  @Test
  def testLintProjectVersion_merge(): Unit = {
    val expected =
      """
        |[INFO]     main-SNAPSHOT
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      LintMaven.lintProjectVersion(sys.out, opts, "main-SNAPSHOT", new OneTimeSwitch(), new OneTimeSwitch(),
        BranchTagMerge.merge, Nil, isShop = false, headBranchName = None)
    })
  }

  @Test
  def testLintProjectVersion_main(): Unit = {
    val expected =
      """
        |[INFO]     main-SNAPSHOT
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val boolean = new OneTimeSwitch()
      LintMaven.lintProjectVersion(sys.out, opts, "main-SNAPSHOT", warnExit = boolean, new OneTimeSwitch(),
        Some(BranchTagMerge(tagName = None, branchName = Some("main"))), Nil, isShop = false, headBranchName = None)
      Assert.assertFalse(boolean.isTriggered())
    })
  }

  @Test
  def testLintProjectVersion_main_with_released_version(): Unit = {
    val expected =
      """
        |[INFO]     1.2.3-SNAPSHOT
        |[WARNING]  tag v1.2.3 is already existing. Please increment to next version e.g. 1.2.4-SNAPSHOT 😬 RL1020
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val boolean = new OneTimeSwitch()
      LintMaven.lintProjectVersion(sys.out, opts, "1.2.3-SNAPSHOT", warnExit = boolean, new OneTimeSwitch(),
        Some(BranchTagMerge(tagName = None, branchName = Some("main"))), allGitTags = Seq("v1.2.3"), isShop = false, headBranchName = None)
      Assert.assertTrue(boolean.isTriggered())
    })
  }

  @Test
  def testLintProjectVersion_main_with_released_version_skipped(): Unit = {
    val expected =
      """
        |[INFO]     1.2.3-SNAPSHOT
        |[warning]  tag v1.2.3 is already existing. Please increment to next version e.g. 1.2.4-SNAPSHOT 😬 RL1020
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1020", "ö")))
      val boolean = new OneTimeSwitch()
      val skips = LintMaven.lintProjectVersion(sys.out, opts, "1.2.3-SNAPSHOT", warnExit = boolean, new OneTimeSwitch(),
        Some(BranchTagMerge(tagName = None, branchName = Some("main"))), allGitTags = Seq("v1.2.3"), isShop = false, headBranchName = None)

      Assert.assertFalse(boolean.isTriggered())
      Assert.assertEquals(Seq("RL1020"), skips)
    })
  }

  @Test
  def testLintProjectVersion_tag_gaps(): Unit = {
    val expected =
      """
        |[INFO]     5.0.0
        |[warning]  Invalid calendar week 99 for year 2026 <== »ja2026.99.ooo« 😬
        |[warning]  unexpected version increment: 5.0.0; expected one of: »99.0.1, 99.1.0, 100.0.0«. Because latest numeric version is: »99.0.0«
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val warn = new OneTimeSwitch()

      LintMaven.lintProjectVersion(sys.out, opts, "5.0.0", warnExit = warn, new OneTimeSwitch(),
        Some(BranchTagMerge(tagName = Some("v5.0.0"), branchName = None)),
        allGitTags = Seq("v1.2.3", "bert-preview", "v99.0.0", "99.0.0", "vja2026.99.ooo"), isShop = false, headBranchName = None)
      Assert.assertFalse(warn.isTriggered())
    })
  }

  @Test
  def testLintProjectVersion_tag(): Unit = {
    val expected =
      """
        |[INFO]     1.2.3
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val hasWarnExit = new OneTimeSwitch()
      val btm = Some(BranchTagMerge(tagName = Some("v1.2.3"), branchName = None))
      Assert.assertTrue(Lint.isValidTag(btm))
      LintMaven.lintProjectVersion(sys.out, opts, "1.2.3", warnExit = hasWarnExit, new OneTimeSwitch(),
        btm, allGitTags = Seq("v1.2.2", "v1.2.3"), isShop = false, headBranchName = None)
      Assert.assertFalse(hasWarnExit.isTriggered())
    })
  }

  @Test
  def testLintProjectVersion_tag_problem(): Unit = {
    val expected =
      """
        |[INFO]     otto
        |[WARNING]  version »otto« is not recommended, please use at least a single digit e.g. 1.0.0. This helps us to cleanup older versions. 😬
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val hasWarnExit = new OneTimeSwitch()
      val btm = Some(BranchTagMerge(tagName = Some("otto"), branchName = None))
      Assert.assertTrue(Lint.isValidTag(btm))
      LintMaven.lintProjectVersion(sys.out, opts, "otto", warnExit = hasWarnExit, new OneTimeSwitch(),
        btm, allGitTags = Seq("v1.2.2", "v1.2.3"), isShop = false, headBranchName = None)
      Assert.assertTrue(hasWarnExit.isTriggered())
    })
  }

  @Test
  def testLintProjectVersion_unknown_release(): Unit = {
    val expected =
      """
        |[INFO]     ro-SNAPSHOT
        |[warning]  unknown release/version pattern: ro-SNAPSHOT -
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      LintMaven.lintProjectVersion(sys.out, opts, "ro-SNAPSHOT", new OneTimeSwitch(), new OneTimeSwitch(),
        Some(BranchTagMerge(tagName = None, branchName = Some("release/ro"))), Nil, isShop = false, headBranchName = None)
    })
  }

  @Test
  def testLintProjectVersion_unknown_release_suggestion(): Unit = {
    val expected =
      """
        |[INFO]     RC-2024.34.01-bugfix-SNAPSHOT
        |[warning]  unknown release/version pattern: RC-2024.34.01-bugfix-SNAPSHOT Maybe one of the following versions is what you want: 'RC-2024.34.1'
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      LintMaven.lintProjectVersion(sys.out, opts, "RC-2024.34.01-bugfix-SNAPSHOT", new OneTimeSwitch(), new OneTimeSwitch(),
        Some(BranchTagMerge(tagName = None, branchName = Some("release/RC-2024.34.01-bugfix"))), Nil, isShop = false, headBranchName = None)
    })
  }

  @Test
  def testLintProjectVersion_tag_fail_branch(): Unit = {
    val expected =
      """
        |[INFO]     main
        |[WARNING]  version »main« must be a SNAPSHOT; non snapshots are only allowed in tags 😬
        |[WARNING]  project.version »main« does not relate to git branch: »main«. Please use a plausible version marker and git marker combination like: (project.version: main -> git branch:main), ... 😬 RL1014
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val boolean = new OneTimeSwitch()
      val btm = Some(BranchTagMerge(tagName = None, branchName = Some("main")))
      Assert.assertFalse(Lint.isValidTag(btm))
      Assert.assertTrue(Lint.isValidBranch(btm))
      Assert.assertFalse(Lint.isValidMergeRequest(btm))
      LintMaven.lintProjectVersion(sys.out, opts, "main", warnExit = boolean, new OneTimeSwitch(),
        btm, allGitTags = Seq("v1.2.3"), isShop = false, headBranchName = None)
      Assert.assertTrue(boolean.isTriggered())
    })
  }

  @Test
  def testLintProjectVersion_tag_fail_merge(): Unit = {
    val expected =
      """
        |[INFO]     main
        |[WARNING]  version »main« must be a SNAPSHOT; non snapshots are only allowed in tags 😬
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val boolean = new OneTimeSwitch()
      val btm = BranchTagMerge.merge
      Assert.assertFalse(Lint.isValidTag(btm))
      Assert.assertFalse(Lint.isValidBranch(btm))
      Assert.assertTrue(Lint.isValidMergeRequest(btm))
      val result = LintMaven.lintProjectVersion(sys.out, opts, "main", warnExit = boolean, new OneTimeSwitch(),
        btm, allGitTags = Seq("v1.2.3"), isShop = false, headBranchName = None)
      Assert.assertEquals(Nil, result)
      Assert.assertTrue(boolean.isTriggered())
    })
  }

  @Test
  def testLintProjectVersion_fail(): Unit = {
    val expected =
      """
        |[INFO]     null
        |[WARNING]  version »null« must be a SNAPSHOT; non snapshots are only allowed in tags 😬
        |[WARNING]  project.version »null« has no git. Please add some .git folder. 😬 RL1014
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val boolean = new OneTimeSwitch()
      LintMaven.lintProjectVersion(sys.out, opts, null, warnExit = boolean, new OneTimeSwitch(), None, Nil, isShop = false, headBranchName = None)
      Assert.assertTrue(boolean.isTriggered())
    })
  }

  @Test
  def testSelectYearLikesProblems(): Unit = {
    Assert.assertEquals(Seq(
      "Invalid calendar week 991 for year 2026 <== »any2026-991w« \uD83D\uDE2C",
      "Invalid calendar week 88 for year 2027 <== »any2027-88-tag« \uD83D\uDE2C",
      "Invalid calendar week 91 for year 2025 <== »v2025-91-234« \uD83D\uDE2C",
    ),
      LintMaven.selectYearLikesProblems(Seq(
        "a",
        "any2026-991w",
        "any2027-88-tag",
        "v2025-01-12",
        "v2025-91-234",
        "vBeta2024-91-234"
      ), YearMonth.of(2026, 1)))
  }


}
