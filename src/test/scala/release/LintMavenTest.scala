package release

import com.google.googlejavaformat.java.Formatter
import org.eclipse.aether.repository.RemoteRepository
import org.junit.{Assert, Ignore, Rule, Test}
import org.junit.rules.TemporaryFolder
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Repo.ReachableResult
import release.Starter.{LintOpts, Opts}

import java.io.File

class LintMavenTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  def outT(in: String): String = {
    in.replaceAll("- $", "-")
      .replaceAll("/junit[0-9]+/", "/junit-REPLACED/")
      .replaceAll("^\\[..:..:..\\...Z\\] ", "[00:00:00.00Z] ")
      .replaceAll(": git version 2\\.[0-9]+\\.[0-9]+", ": git version 2.999.999")
      .replaceAll("[a-f0-9]{40}$", "affe4533042ef887a5477d73d958814317675be1")
      .replaceAll("dependecies in [0-9]+ms \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "dependecies in 999ms (2000-01-01)")
  }

  @Test
  def testRunEmpty(): Unit = {
    val file = temp.newFolder("release-lint-empty")
    val expected =
      """
        |[00:00:00.00Z] [[34mINFO[0m] --------------------------------[ lint ]--------------------------------
        |[00:00:00.00Z] [[31mERROR[0m] E: NO FILES FOUND in /tmp/junit-REPLACED/release-lint-empty
        |[00:00:00.00Z] [[31mERROR[0m] ----------------------------[ end of lint ]----------------------------""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT)(sys => {
      val opts = Opts().copy(lintOpts = Opts().lintOpts.copy(showTimer = true, showTimeStamps = true))
      Assert.assertEquals(1, Lint.run(sys.out, sys.err, opts, Repo.of(opts), Map.empty, file))
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
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/any.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      Assert.assertEquals(42, Lint.run(sys.out, sys.err, opts, Repo.of(opts), Map.empty, fileB))
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
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org # (no ip)
        |[WARNING]  nexus work url must end with a '/' - https://repo.example.org 😬 RL1001
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |Non existing dependencies for:
        |org.springframework:spring-context:1.0.0->Nil
        |  RepoProxy: https://repo.example.orgorg/springframework/spring-context/maven-metadata.xml
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org")
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Nil)

      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, fileB))
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
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- gitlabci.yml @ gitlab ---
        |[WARNING]    ci path: a
        |[WARNING]    use .gitlab-ci.yml 😬 RL1005
        |[INFO]       CI_COMMIT_TAG : vU
        |[INFO]       CI_COMMIT_REF_NAME : vU
        |[INFO]       CI_COMMIT_BRANCH :
        |[WARNING]    an invalid branch/tag: ciRef: vU, ciTag: vU, ciBranch: , gitTags: , gitBranch:
        |[ERROR]      docker tag : » vU « is no valid git tag name. This could lead to
        |[ERROR]        build problems later. A git tag must match the pattern
        |[ERROR]        » ^v[0-9]+\.[0-9]+\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$ « ❌ RL1006
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[ERROR] exit 2 - because lint found errors, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 2)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])

      val value = Map(
        "CI_CONFIG_PATH" -> "a",
        "CI_COMMIT_REF_NAME" -> "vU",
        "CI_COMMIT_TAG" -> "vU",
      )
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, value, fileB))
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
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1.0.0
        |║ ╚═══ 1.0.1, .., 1.2.8, 1.2.9
        |║
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "0.9", "1.0.0", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, fileB))
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
        |      <artifactId>spring-vals</artifactId>
        |      <version>1.0.0-SNAPSHOT</version>
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
        |[INFO] --- skip-conf / self ---
        |[INFO]     skips: RL10015-aa71e948
        |[INFO] --- version / git ---
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / remote @ git ---
        |[INFO]     HEAD branch: master - affe4533042ef887a5477d73d958814317675be1
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[INFO] --- check for snapshots @ maven ---
        |[warning]   found snapshot: org.springframework:spring-vals:1.0.0-SNAPSHOT 😬 RL1011-eef87565
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[warning]   found preview: org.springframework:spring-context:1.0.0-M1 😬
        |[warning]        next     WIP: org.springframework:spring-context:1.0.1
        |[warning]        previous WIP: org.springframework:spring-context:0.99.99
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 2 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1.0.0-M1
        |║ ╚═══ 1.0.0, .., 1.2.8, 1.2.9
        |╠═╦═ org.springframework:spring-vals:1.0.0-SNAPSHOT
        |║ ╚═══ 1.0.1, .., 1.2.8, 1.2.9
        |║
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 0)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL10015-aa71e948")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-vals"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "0.1", "1.0.1", "1.0.2", "1.2.8", "1.2.9"
        ))
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-context"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "0.99.99", "1.0.0", "1.0.1", "1.0.2", "1.2.8", "1.2.9", "1.0.0-M1"
        ))
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, fileB))
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
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11-SNAPSHOT
        |[WARNING]  0.11-SNAPSHOT != None() 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[WARNING]     Found core 50, 51 😬 RL1013-9c03d223
        |[WARNING]       - 50 -
        |[WARNING]       com.novomind.ishop.core.other:other-context:50.2.3 😬 RL1013-253fb8cd
        |[WARNING]       - 51 -
        |[WARNING]       com.novomind.ishop.core.some:core-some-context:51.2.3 😬 RL1013-235d0058
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 2 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ com.novomind.ishop.core.other:other-context:50.2.3
        |║ ╠═══ (50) 50.4.0
        |║ ╚═══ (51) 51.0.0, 51.2.5
        |╠═╦═ com.novomind.ishop.core.some:core-some-context:51.2.3
        |║ ╚═══ 51.2.5
        |║
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "51.0.0", "51.2.5",
        "50.4.0", "50.2.3",
        "0.0.1"
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, file))
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
        |[INFO]     WIP
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
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 2 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ com.novomind.ishop.core.other:other-context:50x-SNAPSHOT
        |║ ╚═══ 50.2.3, 50.4.0
        |╠═╦═ com.novomind.ishop.core.some:core-some-context:50x-SNAPSHOT
        |║ ╚═══ 50.2.3, 50.4.0
        |║
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "49.4.0", "50.4.0", "50.2.3"
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, file))
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
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.14-SNAPSHOT
        |[WARNING]  0.14-SNAPSHOT != None() 😬 RL1014
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for GAV format @ maven ---
        |[INFO]     ✅ all GAVs scopes looks fine
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.14-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6
        |║
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1003-467ad8bc")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "0.1",
        "1.0.1", "1.0.2", "1.2.8", "1.2.9",
        "2.0", "2.1.1", "2.5.5", "2.5.6",
      )
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, file))
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
        |[INFO]     WIP
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
        |[INFO] known scopes are: compile, import, provided, runtime, system, test
        |[INFO] version ranges are not allowed
        |[INFO] unstable marker like LATEST and RELEASE are not allowed
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 4 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context-latest:LATEST
        |║ ╠═══ (0) 0.0.1
        |║ ╚═══ (1) 1.0.0
        |╠═╦═ org.springframework:spring-context-range:(,1.0],[1.2,)
        |║ ╠═══ (0) 0.0.1
        |║ ╚═══ (1) 1.0.0
        |╠═╦═ org.springframework:spring-context-release:RELEASE
        |║ ╠═══ (0) 0.0.1
        |║ ╚═══ (1) 1.0.0
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
        |[ERROR] exit 2 - because lint found errors, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 2)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Seq("0.0.1", "1.0.1-SNAPSHOT", "1.0.0"))
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, file))
    })

  }

  @Test
  def testSnapUpdate(): Unit = {
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
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- project version @ maven ---
        |[INFO]     0.11.0
        |[INFO] --- check for snapshots @ maven ---
        |[WARNING]   found snapshot: org.springframework:spring-context:1.0.1-SNAPSHOT 😬 RL1011-ea7ea019
        |[WARNING]   found snapshot: org.springframework:spring-other:1.0.0-SNAPSHOT:bert 😬 RL1011-bd849fd4
        |[INFO] --- check for GAV format @ maven ---
        |[WARNING] org.springframework:spring-other:1.0.0-SNAPSHOT:bert uses unusual format, please repair 😬 RL1010-bd849fd4
        |[INFO] known scopes are: compile, import, provided, runtime, system, test
        |[INFO] version ranges are not allowed
        |[INFO] unstable marker like LATEST and RELEASE are not allowed
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- check major versions @ ishop ---
        |[INFO]     ✅ no major version diff
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002
        |[INFO]     RELEASE_NEXUS_WORK_URL=null # (no ip)
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 2 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11.0
        |╠═╦═ org.springframework:spring-context:1.0.1-SNAPSHOT
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6
        |╠═╦═ org.springframework:spring-other:1.0.0-SNAPSHOT:bert
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6
        |║
        |[WARNING] org.springframework:spring-context:1.0.1-SNAPSHOT is already released, remove '-SNAPSHOT' suffix 😬 RL1009-ea7ea019
        |[WARNING] org.springframework:spring-other:1.0.0-SNAPSHOT:bert is not released, but next release (1.0.1) was found (maybe orphan snapshot) 😬 RL10015-83c41f0e
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/folder1
        |/tmp/junit-REPLACED/release-lint-mvn-simple/notes.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1012-d3421ec9", "RL1003-8a73d4ae")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)

      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-other"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "0.0.2", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
          "2.0", "2.1.1", "2.5.5", "2.5.6",
        ))
      Mockito.when(mockRepo.newerAndPrevVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.eq("spring-context"), ArgumentMatchers.anyString()))
        .thenReturn(Seq(
          "0.0.2", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
          "2.0", "2.1.1", "2.5.5", "2.5.6",
        ))
      val value = Map(
        "CI_CONFIG_PATH" -> ".gitlab-ci.yml",
        "CI_COMMIT_REF_NAME" -> "v0.11.0",
        "CI_COMMIT_TAG" -> "v0.11.0",
      )
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, value, file))
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
        |[warning]   found snapshots: 😬 RL1012-d3421ec9
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
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1012-637a4930", "RL1003-b4b0c08b")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "202"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, Map.empty, file))

    })
  }

  @Test
  def testValidMergeRequest(): Unit = {
    val file = temp.newFolder("release-lint-mvn-valid-branch")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", null, null, null)))
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", "",null, "")))
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
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", null, null, null)))
    Assert.assertTrue(Lint.isValidMergeRequest(Lint.toBranchTag("work", "", null, "")))

    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", null, gitA, null)))
    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", "", gitA, "")))
  }

  @Test
  def testValidBranch(): Unit = {
    val file = temp.newFolder("release-lint-mvn-valid-branch")
    val gitA = Sgit.init(file, SgitTest.hasCommitMsg)
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", null, gitA, "work")))
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA, "work")))
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
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", null, gitA, "work")))
    Assert.assertTrue(Lint.isValidBranch(Lint.toBranchTag("work", "", gitA, "work")))

    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", null, gitA, "work")))
    Assert.assertFalse(Lint.isValidTag(Lint.toBranchTag("work", "", gitA, "work")))
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
    Assert.assertTrue(Lint.isValidTag(Lint.toBranchTag("v1.0.0", "v1.0.0", gitA, null)))
    Assert.assertFalse(Lint.isValidBranch(Lint.toBranchTag("v1.0.0", "v1.0.0", gitA, null)))
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
      |[INFO] --- -SNAPSHOTS in files @ maven/sbt/gradle ---
      |[INFO]     ✅ NO SNAPSHOTS in other files found
      |[INFO]     WIP
      |[INFO] --- .mvn @ maven ---
      |[INFO]     WIP
      |[INFO] --- project version @ maven ---
      |[INFO]     0.11-SNAPSHOT
      |[INFO] --- check for snapshots @ maven ---
      |[INFO] --- check for GAV format @ maven ---
      |[INFO]     ✅ all GAVs scopes looks fine
      |[INFO] --- check for preview releases @ maven ---
      |[INFO]     WIP
      |[INFO] --- check major versions @ ishop ---
      |[INFO]     ✅ no major version diff
      |[INFO] --- suggest dependency updates / configurable @ maven ---
      |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/ # (no ip)
      |I: checking dependecies against nexus - please wait
      |I: checked 0 dependecies in 999ms (2000-01-01)
      |[INFO]     WIP
      |[INFO] --- dep.tree @ maven ---
      |[INFO]     WIP
      |[WARNING] --- skip-conf / self / end ---
      |[WARNING]     found unused skips, please remove from your config: RL1003-aaaaaaa
      |
      |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/.git
      |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/.mvn
      |/tmp/junit-REPLACED/release-lint-mvn-deploy-none/pom.xml
      |[INFO] ----------------------------[ end of lint ]----------------------------
      |[WARNING] exit 42 - because lint found warnings, see above 😬""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false, skips = Seq("RL1003-21ee7891", "RL1003-aaaaaaa")))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org/")
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "202"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val env = Map(
        "CI_CONFIG_PATH" -> ".gitlab-ci.yml",
        "CI_COMMIT_REF_NAME" -> "feature/bre",
        "CI_COMMIT_TAG" -> "")
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, env, file))

    })
  }
}
