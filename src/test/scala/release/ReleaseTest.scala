package release

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Gav3, SelfRef}
import release.VersionSkew.SkewResult

import java.io.File
import java.util.regex.Pattern

class ReleaseTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  @Test
  def testSuggestPushCmd_gerrit(): Unit = {
    val sgit = new SgitDiff with SgitDetached {
      override def diffSafe(): Seq[String] = Seq("a")

      override def isDetached: Boolean = false
    }
    val result = Release.suggestPushCmd(changedVersion = true, sgit,
      Opts(), "main", "new-main",
      () => "peter")
    Assert.assertEquals("git push origin main:refs/for/new-main;", result)
  }

  @Test
  def testSuggestPushCmd_gitlab(): Unit = {
    val sgit = new SgitDiff with SgitDetached {
      override def diffSafe(): Seq[String] = Seq("a")

      override def isDetached: Boolean = false
    }
    val result = Release.suggestPushCmd(changedVersion = true, sgit,
      Opts().copy(useGerrit = false), "main", "new-main",
      () => "peter")
    Assert.assertEquals("git push origin main:peter-new+main-patch-b269253c;", result)
  }

  @Test
  def testLines(): Unit = {
    val testFile = new File(Util.localWork, "target/grep1.txt")
    if (testFile.isFile) {
      FileUtils.deleteRecursive(testFile)
    }
    FileUtils.write(testFile, Seq("", "a", "bert-SNAPSHOT", "bert-SNAP", "otto-SNAPSHOT"))

    val check = Release.findBadLines(Pattern.compile("-SNAPSHOT"))(testFile.getAbsolutePath)

    Assert.assertEquals(Seq((3, "bert-SNAPSHOT", testFile.toPath), (5, "otto-SNAPSHOT", testFile.toPath)), check)
  }

  @Test
  def testFormatVersionLinesHighlight(): Unit = {

    val check = Release.formatVersionLinesGav(Seq(
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-core-projects", Some("29.6.4-SNAPSHOT")),
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-api", Some("1.0.2.1")),
      ProjectMod.Gav("na", "na", Some("1.0.2.1")),
      ProjectMod.Gav("any", "an", Some("2.2")),
      ProjectMod.Gav("any", "any", Some("2"))
    ), color = true)

    Assert.assertEquals(Seq(
      "* com.novomind.ishop.core:ishop-api:            \u001B[31m1.0.2.1\u001B[0m",
      "* na:na:                                        \u001B[31m1.0.2.1\u001B[0m",
      "* any:any:                                      2",
      "* any:an:                                       2.2",
      "* com.novomind.ishop.core:ishop-core-projects:  \u001B[31m29.6.4-SNAPSHOT\u001B[0m"
    ).mkString("\n"), check.mkString("\n"))
  }

  @Test
  def testFormatVersionLinesGav(): Unit = {

    val check = Release.formatVersionLinesGav(Seq(
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-core-projects", Some("29.6.4-SNAPSHOT")),
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-api", Some("1.0.2.1")),
      ProjectMod.Gav("na", "na", Some("1.0.2.1")),
      ProjectMod.Gav("any", "ax", Some("2.2.2")),
      ProjectMod.Gav("any", "an", Some("2.2")),
      ProjectMod.Gav("any", "any", Some("2")),
      ProjectMod.Gav("", "any", Some("2")),
      ProjectMod.Gav("", "other", Some("7.21")),
      ProjectMod.Gav("", "", Some("2"))
    ))

    Assert.assertEquals(Seq(
      "* com.novomind.ishop.core:ishop-api:            1.0.2.1",
      "* na:na:                                        1.0.2.1",
      "* any:any:                                      2",
      "* :any:                                         2",
      "* ::                                            2",
      "* any:an:                                       2.2",
      "* any:ax:                                       2.2.2",
      "* :other:                                       7.21",
      "* com.novomind.ishop.core:ishop-core-projects:  29.6.4-SNAPSHOT"
    ).mkString("\n"), check.mkString("\n"))
  }

  def repSha(a: String): String = {
    if (a.matches("(.*)\\* [0-9a-f]+ (.*)")) {
      "" // because of unstable graph
    } else {
      a.replaceFirst("\\s*$", "")
    }
  }

  @Test(timeout = 20_000)
  def testWork(): Unit = {
    val localWorkFolder = temp.newFolder()
    val remoteWorkFolder = temp.newFolder()

    val gitRemote = Sgit.init(remoteWorkFolder)
    gitRemote.configSetLocal("user.email", "you@example.com")
    gitRemote.configSetLocal("user.name", "Your Name")

    val f1 = SgitTest.testFile(gitRemote.file, "pom.xml")
    FileUtils.write(f1,
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>0.11-SNAPSHOT</version>
        |
        |</project>
        |
        |""".stripMargin.linesIterator.toSeq)
    gitRemote.add(f1)
    gitRemote.commitAll("test")

    val gitLocal = Sgit.doClone(remoteWorkFolder, localWorkFolder, verify = false)
    gitLocal.configSetLocal("user.email", "you@example.com")
    gitLocal.configSetLocal("user.name", "Your Name")
    val term = Term.select("xterm", "b", simpleChars = true, isInteractice = false)
    val expected =
      """I: Reading pom.xmls ... done (g)
        |---------
        |1. MAJOR version when you make incompatible API changes,
        |2. MINOR version when you add functionality in a backwards-compatible manner, and
        |3. PATCH version when you make backwards-compatible bug fixes.
        |(4. SUFFIX e.g. »-M1« for milestone1 or »-RC1« for releaseCandidate1)
        |   see also: http://semver.org/
        |---------
        |Enter release version for »0.11-SNAPSHOT« [0.11]:
        |Selected release is 0.11
        |Enter the next version without -SNAPSHOT [0.12.0]:
        |Committing pom changes ... done (f)
        |Checking out release/0.11 ... done (e)
        |Commiting pom changes ... done (d)
        |Checking out master ... done (c)
        |
        |
        ||/
        |
        |Push to Gerrit and publish release? [y/n]: y
        |done.""".stripMargin

    TermTest.testSys(Seq("", "", "y", ""), expected, "", outFn = repSha, expectedExitCode = 0)(sys => {
      val opts = Opts(useJlineInput = false)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, () => "abc",
        ReleaseConfig.default(true), opts.newRepo, opts)

    })
    Assert.assertEquals(Seq("Releasetool-sha1: abc"), gitRemote.log().linesIterator.filter(_.startsWith("Releasetool-sha1")).toSeq)

  }

  @Test(timeout = 200_000)
  def testWorkSelectNextChoose(): Unit = {
    val localWorkFolder = temp.newFolder()
    val remoteWorkFolder = temp.newFolder()

    val gitRemote = Sgit.init(remoteWorkFolder)
    gitRemote.configSetLocal("user.email", "you@example.com")
    gitRemote.configSetLocal("user.name", "Your Name")

    val f1 = SgitTest.testFile(gitRemote.file, "pom.xml")
    FileUtils.write(f1,
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>1x-SNAPSHOT</version>
        |
        |</project>
        |
        |""".stripMargin.linesIterator.toSeq)
    gitRemote.add(f1)
    gitRemote.commitAll("test")
    gitRemote.doTag("1.0.0")

    val gitLocal = Sgit.doClone(remoteWorkFolder, localWorkFolder, verify = false)
    gitLocal.configSetLocal("user.email", "you@example.com")
    gitLocal.configSetLocal("user.name", "Your Name")
    val term = Term.select("xterm", "b", simpleChars = true, isInteractice = false)
    val expected =
      """I: Reading pom.xmls ... done (g)
        |---------
        |1. MAJOR version when you make incompatible API changes,
        |2. MINOR version when you add functionality in a backwards-compatible manner, and
        |3. PATCH version when you make backwards-compatible bug fixes.
        |(4. SUFFIX e.g. »-M1« for milestone1 or »-RC1« for releaseCandidate1)
        |   see also: http://semver.org/
        |---------
        |Enter release version for »1x-SNAPSHOT«
        |[1] 2.0.0
        |[2] 1.1.0
        |[3] 1.0.1
        |Enter option or type [1.0.1]:
        |Selected release is 1.0.1
        |Enter the next version without -SNAPSHOT [1x]:
        |skipped release commit on master
        |Checking out release/1.0.1 ... done (e)
        |Commiting pom changes ... done (d)
        |Checking out master ... done (c)
        |
        |
        |Push to Gerrit and publish release? [y/n]: y
        |done.""".stripMargin

    TermTest.testSys(Seq("", "", "y", ""), expected, "", outFn = repSha, expectedExitCode = 0)(sys => {
      val opts = Opts(useJlineInput = false)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, () => "abc",
        ReleaseConfig.default(true), opts.newRepo, opts)

    })

  }

  @Test(timeout = 200_000)
  def testWorkSelectNextChoose_patch(): Unit = {
    val localWorkFolder = temp.newFolder()
    val remoteWorkFolder = temp.newFolder()

    val gitRemote = Sgit.init(remoteWorkFolder)
    gitRemote.configSetLocal("user.email", "you@example.com")
    gitRemote.configSetLocal("user.name", "Your Name")

    val f1 = SgitTest.testFile(gitRemote.file, "pom.xml")
    FileUtils.write(f1,
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>1x-SNAPSHOT</version>
        |
        |</project>
        |
        |""".stripMargin.linesIterator.toSeq)
    gitRemote.add(f1)
    gitRemote.commitAll("test")
    gitRemote.doTag("1.0.0")

    val gitLocal = Sgit.doClone(remoteWorkFolder, localWorkFolder, verify = false)
    gitLocal.configSetLocal("user.email", "you@example.com")
    gitLocal.configSetLocal("user.name", "Your Name")
    val term = Term.select("xterm", "b", simpleChars = true, isInteractice = false)
    val expected =
      """I: Reading pom.xmls ... done (g)
        |---------
        |1. MAJOR version when you make incompatible API changes,
        |2. MINOR version when you add functionality in a backwards-compatible manner, and
        |3. PATCH version when you make backwards-compatible bug fixes.
        |(4. SUFFIX e.g. »-M1« for milestone1 or »-RC1« for releaseCandidate1)
        |   see also: http://semver.org/
        |---------
        |Selected release is 1.0.1
        |Enter the next version without -SNAPSHOT [1x]:
        |skipped release commit on master
        |Checking out release/1.0.1 ... done (e)
        |Commiting pom changes ... done (d)
        |Checking out master ... done (c)
        |
        |
        |Push to Gerrit and publish release? [y/n]: y
        |done.""".stripMargin

    TermTest.testSys(Seq("", "y", ""), expected, "", outFn = repSha, expectedExitCode = 0)(sys => {
      val opts = Opts(useJlineInput = false, versionIncrement = Increment.patch)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, () => "abc",
        ReleaseConfig.default(true), opts.newRepo, opts)

    })

  }

  @Test(timeout = 200_000)
  def testWorkSelectNextChoose_minor(): Unit = {
    val localWorkFolder = temp.newFolder()
    val remoteWorkFolder = temp.newFolder()

    val gitRemote = Sgit.init(remoteWorkFolder)
    gitRemote.configSetLocal("user.email", "you@example.com")
    gitRemote.configSetLocal("user.name", "Your Name")

    val f1 = SgitTest.testFile(gitRemote.file, "pom.xml")
    FileUtils.write(f1,
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>1x-SNAPSHOT</version>
        |
        |</project>
        |
        |""".stripMargin.linesIterator.toSeq)
    gitRemote.add(f1)
    gitRemote.commitAll("test")
    gitRemote.doTag("1.0.0")

    val gitLocal = Sgit.doClone(remoteWorkFolder, localWorkFolder, verify = false)
    gitLocal.configSetLocal("user.email", "you@example.com")
    gitLocal.configSetLocal("user.name", "Your Name")
    val term = Term.select("xterm", "b", simpleChars = true, isInteractice = false)
    val expected =
      """I: Reading pom.xmls ... done (g)
        |---------
        |1. MAJOR version when you make incompatible API changes,
        |2. MINOR version when you add functionality in a backwards-compatible manner, and
        |3. PATCH version when you make backwards-compatible bug fixes.
        |(4. SUFFIX e.g. »-M1« for milestone1 or »-RC1« for releaseCandidate1)
        |   see also: http://semver.org/
        |---------
        |Selected release is 1.1.0
        |Enter the next version without -SNAPSHOT [1x]:
        |skipped release commit on master
        |Checking out release/1.1.0 ... done (e)
        |Commiting pom changes ... done (d)
        |Checking out master ... done (c)
        |
        |
        |Push to Gerrit and publish release? [y/n]: y
        |done.""".stripMargin

    TermTest.testSys(Seq("", "y", ""), expected, "", outFn = repSha, expectedExitCode = 0)(sys => {
      val opts = Opts(useJlineInput = false, versionIncrement = Increment.minor)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, () => "abc",
        ReleaseConfig.default(true), opts.newRepo, opts)

    })

  }

  @Test(timeout = 200_000)
  def testWorkSelectNextChoose_major(): Unit = {
    val localWorkFolder = temp.newFolder()
    val remoteWorkFolder = temp.newFolder()

    val gitRemote = Sgit.init(remoteWorkFolder)
    gitRemote.configSetLocal("user.email", "you@example.com")
    gitRemote.configSetLocal("user.name", "Your Name")

    val f1 = SgitTest.testFile(gitRemote.file, "pom.xml")
    FileUtils.write(f1,
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        |  <modelVersion>4.0.0</modelVersion>
        |
        |  <groupId>com.novomind.ishop.any</groupId>
        |  <artifactId>any</artifactId>
        |  <version>1x-SNAPSHOT</version>
        |
        |</project>
        |
        |""".stripMargin.linesIterator.toSeq)
    gitRemote.add(f1)
    gitRemote.commitAll("test")
    gitRemote.doTag("1.0.0")

    val gitLocal = Sgit.doClone(remoteWorkFolder, localWorkFolder, verify = false)
    gitLocal.configSetLocal("user.email", "you@example.com")
    gitLocal.configSetLocal("user.name", "Your Name")
    val term = Term.select("xterm", "b", simpleChars = true, isInteractice = false)
    val expected =
      """I: Reading pom.xmls ... done (g)
        |---------
        |1. MAJOR version when you make incompatible API changes,
        |2. MINOR version when you add functionality in a backwards-compatible manner, and
        |3. PATCH version when you make backwards-compatible bug fixes.
        |(4. SUFFIX e.g. »-M1« for milestone1 or »-RC1« for releaseCandidate1)
        |   see also: http://semver.org/
        |---------
        |Selected release is 2.0.0
        |Enter the next version without -SNAPSHOT [1x]:
        |skipped release commit on master
        |Checking out release/2.0.0 ... done (e)
        |Commiting pom changes ... done (d)
        |Checking out master ... done (c)
        |
        |
        |Push to Gerrit and publish release? [y/n]: y
        |done.""".stripMargin

    TermTest.testSys(Seq("", "y", ""), expected, "", outFn = repSha, expectedExitCode = 0)(sys => {
      val opts = Opts(useJlineInput = false, versionIncrement = Increment.major)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, () => "abc",
        ReleaseConfig.default(true), opts.newRepo, opts)

    })
  }

  @Test
  def testFindDiff_single(): Unit = {
    val value = "50.1.2"
    val d = ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some(value))
    val deps = Seq((value, d))
    Assert.assertEquals((Seq("50"), false, Seq(("50", d))), Release.findDiff("50", deps))
  }

  @Test
  def testFindDiff_single_2(): Unit = {
    val value = "50.1.2"
    val d = ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some(value))
    val deps = Seq((value, d), (value, d.copy(artifactId = "a2")))
    Assert.assertEquals((Seq("50"), false, Seq(("50", d), ("50", d.copy(artifactId = "a2")))), Release.findDiff("50", deps))
  }

  @Test
  def testFindDiff_single_snpashot(): Unit = {
    val value = "50.1.2-SNAPSHOT"
    val d = ProjectModTest.depOf(pomRef = SelfRef.undef, groupId = "g", artifactId = "a", version = Some(value))
    val deps = Seq((value, d))
    Assert.assertEquals((Seq("50"), false, Seq(("50", d))), Release.findDiff("50", deps))
  }

  @Test
  def testFindDiff_single_snpashot_2(): Unit = {
    val value = "50.1.2-SNAPSHOT"
    val d = ProjectModTest.depOfUndef(groupId = "g", artifactId = "a", version = Some(value))
    val deps = Seq((value, d), (value, d.copy(artifactId = "a2")))
    Assert.assertEquals((Seq("50"), false, Seq(("50", d), ("50", d.copy(artifactId = "a2")))), Release.findDiff("50", deps))
  }

  @Test
  def testFindDiff_single_x_snpashot(): Unit = {
    val value = "50x-SNAPSHOT"
    val d = ProjectModTest.depOfUndef(groupId = "g", artifactId = "a", version = Some(value))
    val deps = Seq((value, d))
    Assert.assertEquals((Seq("50"), false, Seq(("50", d))), Release.findDiff("50", deps))
  }

  @Test
  def testFindDiff_single_x_snpashot_2(): Unit = {
    val value = "50x-SNAPSHOT"
    val d = ProjectModTest.depOfUndef(groupId = "g", artifactId = "a", version = Some(value))
    val deps = Seq((value, d), (value, d.copy(artifactId = "a2")))
    Assert.assertEquals((Seq("50"), false, Seq(("50", d), ("50", d.copy(artifactId = "a2")))), Release.findDiff(value, deps))
  }

  def progFilter(a: String): String = {
    if (a.startsWith("Progress: (")) {
      "Progress: (...)[                                                  ]" // because of parallel
    } else {
      a.replaceFirst("\\s*$", "")
    }
  }

  @Test
  def testOfferAutoFixForReleaseSnapshots(): Unit = {
    val expected =
      """
        |Progress: (...)[                                                  ]
        |Progress: (...)[                                                  ]
        |Progress: (...)[                                                  ]
        |Progress: (...)[                                                  ]
        |Progress: (...)[                                                  ]
        |
        |Other snapshot found for (please fix manually (remove -SNAPSHOT in most cases)):
        |  : Gav(org.example,plugin,Some(1.0.0-SNAPSHOT),,,)
        |
        |Snapshots found for (please fix manually in pom.xml (remove -SNAPSHOT in most cases)):
        |  No Release for    org.example:example:1.0.0-SNAPSHOT
        |  No Release for    org.example:example2:1.0.0-SNAPSHOT maybe 0.99 is the latest one
        |
        |Try again? [y/n]: n""".stripMargin.trim
    TermTest.testSys(Seq("n", ""), expected, "", expectedExitCode = 1, outFn = progFilter)(sys => {
      val opts = Opts(useJlineInput = false)
      val testMod = new ProjectModTest.MockMod() {
        override def listSnapshotDependenciesDistinct: Seq[ProjectMod.Dep] = {
          Seq(
            ProjectModTest.depOfShort("org.example", "example", "1.0.0-SNAPSHOT"),
            ProjectModTest.depOfShort("org.example", "example2", "1.0.0-SNAPSHOT"),
          )
        }

        override private[release] def listDeps() = {
          Seq(
            ProjectModTest.depOfShort("org.example", "example3", "3.0.0"),
            ProjectModTest.depOfShort("org.example", "example", "1.0.0-SNAPSHOT"),
            ProjectModTest.depOfShort("org.example", "self", "1.0.0-SNAPSHOT"),
            ProjectModTest.depOfShort("org.example", "plugin", "1.0.0-SNAPSHOT"),
          )
        }

        override def getSelfDepsMod: Seq[ProjectMod.Dep] = {
          Seq(
            ProjectModTest.depOfShort("org.example", "self", "1.0.0-SNAPSHOT"),
          )
        }
      }
      val mockRepo = Mockito.mock(classOf[Repo])
      Mockito.when(mockRepo.latestGav(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyString()))
        .thenReturn(None)
      Mockito.when(mockRepo.latestGav("org.example", "example2", "1.0.0-SNAPSHOT"))
        .thenReturn(Some(Gav3("org.example", "example2", Some("0.99"))))
      Release.offerAutoFixForReleaseSnapshots(sys, mod = testMod, gitFiles = Nil, shellWidth = 72, mockRepo, opts)

    })

  }
}
