package release

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Lint.BranchTagMerge
import release.Starter.Opts

import java.io.File
import java.time.ZonedDateTime
import java.util.concurrent.atomic.AtomicBoolean

class LintMavenTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  val fakeStackElement = "  at release..."

  def replaceAllVarLiterals(in: Seq[String]): Seq[String] = {
    val value1 = in.takeWhile(_ != fakeStackElement)
    in
  }

  def replaceVarLiterals(in: String): String = {
    in.replaceAll("- $", "-")
      .replaceAll("/junit[0-9]+/", "/junit-REPLACED/")
      //.replaceAll("\tat .*", fakeStackElement)
      .replaceAll("( package name(?:s|) in) PT[0-9]+S", "$1 PT4S")
      .replaceAll("^\\[..:..:..\\...Z\\] ", "[00:00:00.00Z] ")
      .replaceAll(": git version 2\\.[0-9]+\\.[0-9]+", ": git version 2.999.999")
      .replaceAll("[a-f0-9]{40}$", "affe4533042ef887a5477d73d958814317675be1")
      .replaceAll("dependencies in [0-9]+ms \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "dependencies in 999ms (2000-01-01)")
  }

  @Test
  def testRunEmpty(): Unit = {
    val file = temp.newFolder("release-lint-empty")
    val expected =
      """
        |[00:00:00.00Z] [[34mINFO[0m] --------------------------------[ lint ]--------------------------------
        |[00:00:00.00Z] [[31mERROR[0m] E: NO FILES FOUND in /tmp/junit-REPLACED/release-lint-empty
        |[00:00:00.00Z] [[31mERROR[0m] ----------------------------[ end of lint ]----------------------------""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts().copy(lintOpts = Opts().lintOpts.copy(showTimer = true, showTimeStamps = true))
      Assert.assertEquals(1, Lint.run(sys.out, sys.err, opts, Map.empty, file))
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
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB, depth = 1)

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
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/any.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      Assert.assertEquals(42, Lint.run(sys.out, sys.err, opts, Map.empty, fileB))
    })

  }

  @Test
  def testWorkUrl(): Unit = {

    val remote = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val pom = new File(remote, "pom.xml")
    Util.write(pom,
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
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org # (no ip)
        |[WARNING]  nexus work url must end with a '/' - https://repo.example.org 😬 RL1001
        |I: checking dependencies against nexus - please wait
        |
        |I: checked 1 dependencies in 999ms (2000-01-01)
        |Non existing dependencies for:
        |org.springframework:spring-context:1.0.0->Nil
        |  RepoProxy: https://repo.example.orgorg/springframework/spring-context/maven-metadata.xml
        |
        |
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/", "https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Nil)

      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, fileB))
    })

  }

  @Test
  def testCiVars(): Unit = {

    val remote = temp.newFolder("release-lint-mvn-simple-init")
    val fileB = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(remote, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 unknown remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 HEAD branch: (unknown) - n/a
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 0
        |[INFO]     active branch count: 0
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (fetch)
        |[INFO]       remote: origin  file:/tmp/junit-REPLACED/release-lint-mvn-simple-init/ (push)
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[WARNING]    ci path: a
        |[WARNING]    use .gitlab-ci.yml 😬 RL1005
        |[INFO]       CI_COMMIT_TAG : vU
        |[INFO]       CI_COMMIT_REF_NAME : vU
        |[INFO]       CI_COMMIT_BRANCH :
        |[WARNING]    an INVALID branch/tag: ciRef: vU, ciTag: vU, ciBranch: , gitTags: , gitBranch:
        |[ERROR]      docker tag : » vU « is no valid git tag name. This could lead to
        |[ERROR]        build problems later. A git tag must match the pattern
        |[ERROR]        » ^v[0-9]+\.[0-9]+\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$ « ❌ RL1006
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[ERROR] exit 43 - because lint found errors, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 43)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])

      val value = Map(
        "CI_CONFIG_PATH" -> "a",
        "CI_COMMIT_REF_NAME" -> "vU",
        "CI_COMMIT_TAG" -> "vU",
      )
      System.exit(Lint.run(sys.out, sys.err, opts, value, fileB))
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
    Util.write(pom,
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
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |I: checking dependencies against nexus - please wait
        |
        |I: checked 1 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1.0.0
        |║ ╚═══ 1.0.1, .., 1.2.8, 1.2.9 (libyears: 0Y 0M [0 days])
        |║
        |
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts1 = Opts().lintOpts.copy(showTimer = false)
      val opts = Opts(colors = false, lintOpts = opts1)
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "0.9", "1.0.0", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, fileB))
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
    Util.write(pom,
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
    val gitB = Sgit.doCloneRemote(remote.toURI.toString.replaceFirst("file:/", "file:///"), fileB)

    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self ---
        |[INFO]     skips: RL10015-aa71e948, RL1017-ab101a0e, RL1018-ceefe9c6
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
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |I: checking dependencies against nexus - please wait
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
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL10015-aa71e948", "RL1017-ab101a0e", "RL1018-ceefe9c6")))
      val mockRepo = Mockito.mock(classOf[Repo])
      val mockRepo2 = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo2.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo, mockRepo2))
      Mockito.when(mockRepo2.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo, mockRepo2))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo2.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
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
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, fileB))
    })
  }

  @Test
  def testRunMvnMajorVersion(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
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
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 0
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
        |[WARNING]  0.11-SNAPSHOT != None() 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[WARNING]     Found core 50, 51 😬 RL1013-28c40a8a
        |[WARNING]       - 50 -
        |[WARNING]       com.novomind.ishop.core.other:other-context:50.2.3 😬 RL1013-253fb8cd
        |[WARNING]       - 51 -
        |[WARNING]       com.novomind.ishop.core.some:core-some-context:51.2.3 😬 RL1013-235d0058
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependencies against nexus - please wait
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
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
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
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testRunMvnMajorVersionNo(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
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
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 0
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
        |[WARNING]  0.11-SNAPSHOT != None() 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[warning]   found snapshot: com.novomind.ishop.core.other:other-context:50x-SNAPSHOT 😬 RL1011-08f0f8bf
        |[warning]   found snapshot: com.novomind.ishop.core.some:core-some-context:50x-SNAPSHOT 😬 RL1011-9f269959
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependencies against nexus - please wait
        |
        |I: checked 2 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ com.novomind.ishop.core.other:other-context:50x-SNAPSHOT
        |║ ╚═══ 50.2.3, 50.4.0 (libyears: 0Y 0M [0 days])
        |╠═╦═ com.novomind.ishop.core.some:core-some-context:50x-SNAPSHOT
        |║ ╚═══ 50.2.3, 50.4.0 (libyears: 0Y 0M [0 days])
        |║
        |
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "49.4.0", "50.4.0", "50.2.3", "50x-SNAPSHOT"
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testRunMvnSimple(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    Util.write(new File(file, "pom.xml"),
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
    val notes = new File(file, "notes.md")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self ---
        |[INFO]     skips: RL1003-467ad8bc
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 0
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
        |[WARNING]  0.14-SNAPSHOT != None() 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependencies against nexus - please wait
        |
        |I: checked 1 dependencies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.14-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9 (libyears: 0Y 0M [0 days])
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6 (libyears: 0Y 0M [0 days])
        |║
        |
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1003-467ad8bc")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
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
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testEmptyVersions(): Unit = {
    val file = temp.newFolder("release-lint-mvn-empty")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
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
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 0
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
        |[WARNING]  0.11-SNAPSHOT != None() 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[warning]   found snapshot: org.springframework:spring-context:1.0.1-SNAPSHOT 😬 RL1011-ea7ea019
        |[INFO] --- check for GAV format @ maven ---
        |[WARNING] org.springframework:spring-context-range:(,1.0],[1.2,) uses unusual format, please repair 😬 RL1010-92f68063
        |[WARNING] org.springframework:spring-context-release:RELEASE uses unusual format, please repair 😬 RL1010-d484eb4f
        |[WARNING] org.springframework:spring-context-latest:LATEST uses unusual format, please repair 😬 RL1010-94d64e99
        |[WARNING] known scopes are: compile, import, provided, runtime, system, test
        |[WARNING] version ranges are not allowed
        |[WARNING] unstable marker like LATEST and RELEASE are not allowed
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependencies against nexus - please wait
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
        |[ERROR] invalid empty versions:
        |org.springframework:spring-context-empty
        |org.springframework:spring-context-blank 😬 RL1008
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-empty/.git
        |/tmp/junit-REPLACED/release-lint-mvn-empty/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[ERROR] exit 43 - because lint found errors, see above ❌""".stripMargin
    val errorOut =
      """java.lang.IllegalArgumentException: invalid empty versions:
        |org.springframework:spring-context-empty
        |org.springframework:spring-context-blank
        |	at release.PomMod$.unmanaged(PomMod.scala:636)
        |	at release.ProjectMod$.collectDependencyUpdates(ProjectMod.scala:606)
        |	at release.ProjectMod.collectDependencyUpdates(ProjectMod.scala:690)
        |	at release.ProjectMod.collectDependencyUpdates$(ProjectMod.scala:682)
        |	at release.PomMod.collectDependencyUpdates(PomMod.scala:27)
        |	at release.ProjectMod.tryCollectDependencyUpdates(ProjectMod.scala:674)
        |	at release.ProjectMod.tryCollectDependencyUpdates$(ProjectMod.scala:671)
        |	at release.PomMod.tryCollectDependencyUpdates(PomMod.scala:27)
        |	at release.Lint$.run(Lint.scala:651)""".stripMargin.stripTrailing()
    TermTest.testSys(Nil, expected, errorOut, outFn = replaceVarLiterals, outAllFn = replaceAllVarLiterals,
      expectedExitCode = 43)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Seq("0.0.1", "1.0.1-SNAPSHOT", "1.0.0"))
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))
    })

  }

  @Test
  def testSnapUpdateWithDates(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    Util.write(new File(file, "pom.xml"),
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
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    val notes = new File(file, "notes.md")
    Util.write(notes,
      """
        |This is the documentation for 0.11-SNAPSHOT
        |This is the documentation for 0.11-SNAPSHOT
        |""".stripMargin.linesIterator.toSeq)
    val folder1 = new File(file, "folder1")
    folder1.mkdir()
    val file1 = new File(folder1, "test.txt")
    Util.write(file1, Seq("content"))
    gitA.add(notes)
    gitA.add(Util.write(new File(folder1, "Demo.java"), // TODO valid folder
      """package some;
        |""".stripMargin.linesIterator.toSeq))
    val unwantedPackages = new File(file, ".unwanted-packages")
    Util.write(unwantedPackages,
      """a.b;
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(unwantedPackages)
    gitA.add(file1)
    gitA.commitAll("some")
    gitA.doTag("0.11.0")
    gitA.checkout("v0.11.0")
    Util.write(file1, Seq("content1"))
    val file2 = new File(folder1, "test2.txt")
    Util.write(file2, Seq("content1"))
    Assert.assertTrue(gitA.currentTags.isDefined)
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self ---
        |[INFO]     skips: RL1012-d3421ec9, RL1003-8a73d4ae
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 1
        |[INFO]       Your Name <you@example.com>
        |[INFO]     active branch count: 1 - master
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
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[INFO]       ci path: .gitlab-ci.yml
        |[INFO]       CI_COMMIT_TAG : v0.11.0
        |[INFO]       CI_COMMIT_REF_NAME : v0.11.0
        |[INFO]       CI_COMMIT_BRANCH :
        |[INFO]       a valid tag : v0.11.0
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
        |[WARNING] org.springframework:spring-other:1.0.0-SNAPSHOT:bert uses unusual format, please repair 😬 RL1010-bd849fd4
        |[WARNING] known scopes are: compile, import, provided, runtime, system, test
        |[WARNING] version ranges are not allowed
        |[WARNING] unstable marker like LATEST and RELEASE are not allowed
        |[INFO] --- check for preview releases @ maven ---
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependencies against nexus - please wait
        |
        |I: checked 3 dependencies in 999ms (2000-01-01)
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
        |
        |Σ libyears: 2Y 10M (1065 days)
        |[WARNING] org.springframework:spring-context:1.0.1-SNAPSHOT is already released, remove '-SNAPSHOT' suffix 😬 RL1009-ea7ea019
        |[WARNING] org.springframework:spring-other:1.0.0-SNAPSHOT is not released, but next release (1.0.1) was found (maybe orphan snapshot) 😬 RL10015-f0a969b5
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |[INFO] --- unwanted-packages @ ishop ---
        |[INFO]     found 1 package name in PT4S
        |[INFO]     ✅ no problematic packages found
        |[INFO]     .unwanted-packages // checksum f466d6ea
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.unwanted-packages
        |/tmp/junit-REPLACED/release-lint-mvn-simple/folder1
        |/tmp/junit-REPLACED/release-lint-mvn-simple/notes.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1012-d3421ec9", "RL1003-8a73d4ae")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq(Repo.centralUrl))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
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

      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-single"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "1.0.1",
          "1.0.2",
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
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), value, file))
    })

  }

  @Test
  def testRunMvnParent(): Unit = {
    val file = temp.newFolder("release-lint-mvn-parent")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    Util.write(new File(file, "pom.xml"),
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
    Util.write(bertPom,
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
    Util.write(unwantedPackages,
      """a.b
        |a.b.c
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(unwantedPackages)
    gitA.add(Util.write(new File(bertFolder, "Demo.java"), // TODO valid folder
      """package a.b;
        |""".stripMargin.linesIterator.toSeq))

    gitA.add(Util.write(new File(bertFolder, "Demo.scala"), // TODO valid folder
      """package c.b
        |""".stripMargin.linesIterator.toSeq))
    gitA.commitAll("some")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 1
        |[INFO]       Your Name <you@example.com>
        |[INFO]     active branch count: 1 - master
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
        |[WARNING]      distinguishable names.
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
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
        |I: checked 0 dependencies in 999ms (2000-01-01)
        |
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |[INFO] --- unwanted-packages @ ishop ---
        |[INFO]     found 2 package names in PT4S
        |[WARNING]  package »a.b;« is in list of unwanted packages, please avoid this package
        |[INFO]     .unwanted-packages // checksum b0af4bf2
        |
        |/tmp/junit-REPLACED/release-lint-mvn-parent/.git
        |/tmp/junit-REPLACED/release-lint-mvn-parent/.unwanted-packages
        |/tmp/junit-REPLACED/release-lint-mvn-parent/bert
        |/tmp/junit-REPLACED/release-lint-mvn-parent/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "202"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), Map.empty, file))

    })
  }

  @Test
  def testRunMvnSimpleFail(): Unit = {
    val file = temp.newFolder("release-lint-mvn-simple-fail")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
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
        |      <artifactId>spring-non</artifactId>
        |      <version>${non-existing}</version>
        |    </dependency>
        |  </dependencies>
        |</project>
        |""".stripMargin.linesIterator.toSeq)
    val notes = new File(file, "notes.md")
    Util.write(notes,
      """
        |This is the documentation for 0.11-SNAPSHOT
        |This is the documentation for 0.11-SNAPSHOT
        |""".stripMargin.linesIterator.toSeq)
    val dotMvnFolder = new File(file, ".mvn")
    dotMvnFolder.mkdir()
    val extension = new File(dotMvnFolder, "extensions.xml")
    Util.write(extension,
      """<version>0.11-SNAPSHOT</version>
        |""".stripMargin
    )
    Util.write(new File(file, "README.md"),
      """hello
        |""".stripMargin
    )
    gitA.add(notes)
    gitA.add(extension)
    gitA.commitAll("some")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self ---
        |[INFO]     skips: RL1012-637a4930, RL1003-b4b0c08b
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 1
        |[INFO]       Your Name <you@example.com>
        |[INFO]     active branch count: 1 - master
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
        |[warning]   found snapshot in: notes.md 😬 RL1012-d143f8dc
        |              This is the documentation for 0.11-SNAPSHOT
        |[warning]   found snapshots: 😬 RL1012-d3421ec9 -- RL1012-d143f8dc
        |[INFO] --- model read @ maven/sbt/gradle ---
        |[WARNING]     😬 No property replacement found in pom.xmls for: "${non-existing}" - define properties where they are required and not in parent pom.xml. Input is Nil.
        |[WARNING]     skipped because of previous problems - No property replacement found in pom.xmls for: "${non-existing}" - define properties where they are required and not in parent pom.xml. Input is Nil. 😬
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/.mvn
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/README.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/notes.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1012-637a4930", "RL1003-b4b0c08b")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "202"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      System.exit(Lint.run(sys.out, sys.err, opts, Map.empty, file))

    })
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
    Util.write(work,
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
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", null, gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val work = new File(file, "pom.xml")
    Util.write(work,
      """<?xml version="1.0" encoding="UTF-8"?>
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(work)
    gitA.commitAll("test")
    gitA.createBranch("work")
    gitA.checkout("work")
    gitA.checkout("HEAD~0")
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", null, gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))

    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", null, gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))
    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))

    gitA.doTag("work-20315-gcb5491b407")
    Assert.assertEquals(Some(Seq("vwork-20315-gcb5491b407")), gitA.currentTags)

    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))

    gitA.doTag("20.0.0.some-6439-g800ef8fee7")
    Assert.assertEquals(Some(Seq("v20.0.0.some-6439-g800ef8fee7")), gitA.currentTags)
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))))

    gitA.deleteTag("work-20315-gcb5491b407")
    gitA.deleteTag("20.0.0.some-6439-g800ef8fee7")

    gitA.doTag("1.2.3")
    Assert.assertEquals(Some(Seq("v1.2.3")), gitA.currentTags)
    val maybeMerge = Lint.toBranchTag("work", "", gitA.currentBranchOpt, "work", currentTagsIn = gitA.currentTags.getOrElse(Nil))
    Assert.assertFalse(Lint.isValidBranch(maybeMerge))
    Assert.assertFalse(Lint.isValidTag(maybeMerge))
  }

  @Test
  def testValidTag(): Unit = {
    val file = temp.newFolder("release-lint-mvn-valid-tag")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    gitA.configSetLocal("user.email", "you@example.com")
    gitA.configSetLocal("user.name", "Your Name")
    val work = new File(file, "pom.xml")
    Util.write(work,
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
    Util.write(new File(file, "pom.xml"),
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
    Util.write(new File(dotMvnFolder, "extensions.xml"),
      """<version>0.11-SNAPSHOT</version>
        |""".stripMargin
    )
    val branchName = "feature/bre"
    // gitA.createBranch(branchName)
    //gitA.checkout(branchName)
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO] --- skip-conf / self ---
        |[INFO]     skips: RL1003-21ee7891, RL1003-aaaaaaa
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[WARNING]  😬 no remote HEAD found, corrupted remote -- repair please
        |[WARNING]  😬 if you use gitlab try to
        |[WARNING]  😬 choose another default branch; save; use the original default branch
        |[WARNING]  😬 remote call exception: java.lang.RuntimeException message: Nonzero exit value: 128; git --no-pager remote show origin; fatal: 'origin' does not appear to be a git repository fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists.
        |[INFO] --- check branches / remote @ git ---
        |[INFO]     active contributor count: 0
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
        |[warning]    no docker tag : no tag
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
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     is shop: false
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |I: checking dependencies against nexus - please wait
        |I: checked 0 dependencies in 999ms (2000-01-01)
        |
        |Σ libyears: 0Y 0M (0 days)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |[INFO] --- skip-conf / self / end ---
        |[WARNING]     found unused skips, please remove from your config: RL1003-aaaaaaa
        |[WARNING]     active skips: RL1003-21ee7891
        |
        |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/.git
        |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/.mvn
        |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1003-21ee7891", "RL1003-aaaaaaa")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.allRepoUrls()).thenReturn(Seq("https://repo.example.org/"))
      Mockito.when(mockRepo.createAll(ArgumentMatchers.any())).thenReturn(Seq(mockRepo))
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "202"))
      Mockito.when(mockRepo.depDate(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString())).
        thenReturn(Some(ZonedDateTime.now()))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val env = Map(
        "CI_CONFIG_PATH" -> ".gitlab-ci.yml",
        "CI_COMMIT_REF_NAME" -> "feature/bre",
        "CI_COMMIT_TAG" -> "")
      System.exit(Lint.run(sys.out, sys.err, opts.copy(repoSupplier = _ => mockRepo), env, file))

    })
  }

  @Test
  def testLintProjectVersion_merge(): Unit = {
    val expected =
      """
        |[INFO]     main-SNAPSHOT
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      LintMaven.lintProjectVersion(sys.out, opts, "main-SNAPSHOT", new AtomicBoolean(), new AtomicBoolean(),
        BranchTagMerge.merge, Nil)
    })
  }

  @Test
  def testLintProjectVersion_main(): Unit = {
    val expected =
      """
        |[INFO]     main-SNAPSHOT
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val boolean = new AtomicBoolean()
      LintMaven.lintProjectVersion(sys.out, opts, "main-SNAPSHOT", warnExit = boolean, new AtomicBoolean(),
        Some(BranchTagMerge(tagName = None, branchName = Some("main"))), Nil)
      Assert.assertFalse(boolean.get())
    })
  }

  @Test
  def testLintProjectVersion_main_with_released_version(): Unit = {
    val expected =
      """
        |[INFO]     1.2.3-SNAPSHOT
        |[WARNING]  tag v1.2.3 is already existing 😬
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val boolean = new AtomicBoolean()
      LintMaven.lintProjectVersion(sys.out, opts, "1.2.3-SNAPSHOT", warnExit = boolean, new AtomicBoolean(),
        Some(BranchTagMerge(tagName = None, branchName = Some("main"))), allGitTags = Seq("v1.2.3"))
      Assert.assertTrue(boolean.get())
    })
  }

  @Test
  def testLintProjectVersion_tag(): Unit = {
    val expected =
      """
        |[INFO]     1.2.3
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val boolean = new AtomicBoolean()
      val btm = Some(BranchTagMerge(tagName = Some("v1.2.3"), branchName = None))
      Assert.assertTrue(Lint.isValidTag(btm))
      LintMaven.lintProjectVersion(sys.out, opts, "1.2.3", warnExit = boolean, new AtomicBoolean(),
        btm, allGitTags = Seq("v1.2.3"))
      Assert.assertFalse(boolean.get())
    })
  }

  @Test
  def testLintProjectVersion_fail(): Unit = {
    val expected =
      """
        |[INFO]     null
        |[WARNING]  null != None() 😬 RL1014
        |""".stripMargin.trim
    TermTest.testSys(Nil, expected, "", outFn = replaceVarLiterals, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))

      val boolean = new AtomicBoolean()
      LintMaven.lintProjectVersion(sys.out, opts, "null", warnExit = boolean, new AtomicBoolean(), None, Nil)
      Assert.assertTrue(boolean.get())
    })
  }
}
