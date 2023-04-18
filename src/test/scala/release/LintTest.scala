package release

import com.google.googlejavaformat.java.Formatter
import org.eclipse.aether.repository.RemoteRepository
import org.junit.{Assert, Ignore, Rule, Test}
import org.junit.rules.TemporaryFolder
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Repo.ReachableResult
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
      .replaceAll("dependecies in [0-9]+ms \\([0-9]{4}-[0-9]{2}-[0-9]{2}\\)", "dependecies in 999ms (2000-01-01)")
  }

  @Test
  def testGoogleFmt(): Unit = {
    val file = temp.newFile("Demo.java")
    Util.write(file,
      """
        |public class Demo {
        |
        |}
        |""".stripMargin)
    val tuple = Lint.doGoogleFmt(new Formatter(), file)
    println(tuple)
    if (tuple._1.isFailure) {
      tuple._1.get
    }
  }

  @Test
  def testRunEmpty(): Unit = {
    val file = temp.newFolder("release-lint-empty")
    val expected =
      """
        |[[34mINFO[0m] --------------------------------[ lint ]--------------------------------
        |[[31mERROR[0m] E: NO FILES FOUND in /tmp/junit-REPLACED/release-lint-empty
        |[[31mERROR[0m] ----------------------------[ end of lint ]----------------------------""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT)(sys => {
      val opts = Opts()
      Assert.assertEquals(1, Lint.run(sys.out, sys.err, opts, new Repo(opts), file))
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
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[WARNING]  shallow clone detected 😬
        |[WARNING]    % git rev-parse --is-shallow-repository # returns true
        |[WARNING]    % git log -n1 --pretty=%H # returns
        |[WARNING]   a79849c3042ef887a5477d73d958814317675be1
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
        |[INFO] --- -SNAPSHOTS in files @ maven ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/any.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      Assert.assertEquals(42, Lint.run(sys.out, sys.err, opts, new Repo(opts), fileB))
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
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org
        |[WARNING]  nexus work url must end with a '/' - https://repo.example.org 😬 RL1001-WIP
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |term: Term(dumb,lint,false)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above ❌""".stripMargin
        val expectedE =
          """Non existing dependencies for:
            |org.springframework:spring-context:1.0.0->Nil
            |  https://repo.example.orgorg/springframework/spring-context/maven-metadata.xml
            |""".stripMargin
    TermTest.testSys(Nil, expected, expectedE, outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org")
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.newerVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(Nil)

      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, fileB))
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
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org/
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1.0.0
        |║ ╚═══ 1.0.1, .., 1.2.8, 1.2.9
        |║
        |term: Term(dumb,lint,false)
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
        "1.0.0", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
      )
      Mockito.when(mockRepo.newerVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, fileB))
    })

  }

  @Test
  @Ignore // TODO later
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
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[INFO] --- list-remotes @ git ---
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(fetch))
        |[INFO]       remote: GitRemote(origin,file:///tmp/junit-REPLACED/release-lint-mvn-simple-init/,(push))
        |[INFO] --- -SNAPSHOTS in files @ maven ---
        |[INFO]     ✅ NO SNAPSHOTS in other files found
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- check for snapshots @ maven ---
        |[INFO]     WIP
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[INFO]     RELEASE_NEXUS_WORK_URL=https://repo.example.org
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1.0.0-M1
        |║ ╚═══ 1.0.0, .., 1.2.8, 1.2.9
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
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn("https://repo.example.org")
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "1.0.0", "1.0.1", "1.0.2", "1.2.8", "1.2.9",
      )
      Mockito.when(mockRepo.newerVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, fileB))
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
    val notes = new File(file, "notes.md")
    Util.write(notes,
      """
        |This is the documentation for 0.11-SNAPSHOT
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(notes)
    gitA.commitAll("some")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-WIP
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004-WIP
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven ---
        |[WARNING]   found snapshot in: notes.md 😬
        |              This is the documentation for 0.11-SNAPSHOT
        |[INFO]     WIP
        |[INFO] --- .mvn @ maven ---
        |[INFO]     WIP
        |[INFO] --- check for snapshots @ maven ---
        |[INFO] --- check for preview releases @ maven ---
        |[INFO]     WIP
        |[INFO] --- suggest dependency updates / configurable @ maven ---
        |[WARNING]  work nexus points to central https://repo1.maven.org/maven2/ 😬 RL1002-WIP
        |[INFO]     RELEASE_NEXUS_WORK_URL=null
        |I: checking dependecies against nexus - please wait
        |
        |I: checked 1 dependecies in 999ms (2000-01-01)
        |║ Project GAV: com.novomind.ishop.any:any:0.11-SNAPSHOT
        |╠═╦═ org.springframework:spring-context:1
        |║ ╠═══ (1) 1.0.1, .., 1.2.8, 1.2.9
        |║ ╚═══ (2) 2.0, .., 2.5.5, 2.5.6
        |║
        |term: Term(dumb,lint,false)
        |[INFO]     WIP
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple/notes.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.workNexusUrl()).thenReturn(Repo.centralUrl)
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "200"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      val mockUpdates = Seq(
        "1.0.0", "1.0.1","1.0.2", "1.2.8", "1.2.9",
        "2.0", "2.1.1", "2.5.5", "2.5.6",
      )
      Mockito.when(mockRepo.newerVersionsOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(mockUpdates)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, file))
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
        |""".stripMargin.linesIterator.toSeq)
    gitA.add(notes)
    gitA.commitAll("some")
    val expected =
      """
        |[INFO] --------------------------------[ lint ]--------------------------------
        |[INFO]     ✅ git version: git version 2.999.999
        |[INFO] --- check clone config / no shallow clone @ git ---
        |[INFO]     ✅ NO shallow clone
        |[INFO] --- .gitattributes @ git ---
        |[INFO] --- .gitignore @ git ---
        |[WARNING]  Found local changes 😬 RL1003-WIP
        |[INFO] --- list-remotes @ git ---
        |[WARNING]  NO remotes found 😬 RL1004-WIP
        |[WARNING]  % git remote -v # returns nothing
        |[INFO] --- -SNAPSHOTS in files @ maven ---
        |[WARNING]   found snapshot in: notes.md 😬
        |              This is the documentation for 0.11-SNAPSHOT
        |[WARNING]     😬 No property replacement found in pom.xmls for: "${non-existing}" - define properties where they are required and not in parent pom.xml. Input is Nil.
        |[WARNING]     skipped because of previous problems 😬
        |[INFO] --- dep.tree @ maven ---
        |[INFO]     WIP
        |
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/.git
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/notes.md
        |/tmp/junit-REPLACED/release-lint-mvn-simple-fail/pom.xml
        |[INFO] ----------------------------[ end of lint ]----------------------------
        |[WARNING] exit 42 - because lint found warnings, see above ❌""".stripMargin
    TermTest.testSys(Nil, expected, "", outFn = outT, expectedExitCode = 42)(sys => {
      val opts = Opts(colors = false, lintOpts = Opts().lintOpts.copy(showTimer = false))
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.isReachable(false)).thenReturn(Repo.ReachableResult(true, "202"))
      Mockito.when(mockRepo.getRelocationOf(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      System.exit(Lint.run(sys.out, sys.err, opts, mockRepo, file))

    })

  }
}
