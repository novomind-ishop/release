package release

import org.junit.rules.TemporaryFolder

import java.io.{BufferedReader, File, InputStreamReader, PrintStream}
import java.util.regex.Pattern
import org.junit.{Assert, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.{Opts}

class ReleaseTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  @Test
  def testLines(): Unit = {
    val testFile = new File(Util.localWork, "target/grep1.txt")
    if (testFile.isFile) {
      Util.deleteRecursive(testFile)
    }
    Util.write(testFile, Seq("", "a", "bert-SNAPSHOT", "bert-SNAP", "otto-SNAPSHOT"))

    val check = Release.findBadLines(Pattern.compile("-SNAPSHOT"))(testFile.getAbsolutePath)

    Assert.assertEquals(Seq((2, "bert-SNAPSHOT", testFile.toPath), (4, "otto-SNAPSHOT", testFile.toPath)), check)
  }

  @Test
  def testFormatVersionLinesHighlight(): Unit = {

    val check = Release.formatVersionLinesGav(Seq(
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-core-projects", "29.6.4-SNAPSHOT"),
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-api", "1.0.2.1"),
      ProjectMod.Gav("na", "na", "1.0.2.1"),
      ProjectMod.Gav("any", "an", "2.2"),
      ProjectMod.Gav("any", "any", "2")
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
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-core-projects", "29.6.4-SNAPSHOT"),
      ProjectMod.Gav("com.novomind.ishop.core", "ishop-api", "1.0.2.1"),
      ProjectMod.Gav("na", "na", "1.0.2.1"),
      ProjectMod.Gav("any", "ax", "2.2.2"),
      ProjectMod.Gav("any", "an", "2.2"),
      ProjectMod.Gav("any", "any", "2"),
      ProjectMod.Gav("", "any", "2"),
      ProjectMod.Gav("", "other", "7.21"),
      ProjectMod.Gav("", "", "2")
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
    Util.write(f1,
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
        |   see also: http://semver.org/
        |---------
        |Enter the release version [0.11]:
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

    TermTest.testSys(Seq("", "", "y", ""), expected, "", outFn = repSha)(sys => {
      val opts = Opts(useJlineInput = false)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, "abc",
        ReleaseConfig.default(true), new Repo(opts), opts)

    })

  }

  @Test(timeout = 200_000)
  def testWorkSelectNextChoose(): Unit = {
    val localWorkFolder = temp.newFolder()
    val remoteWorkFolder = temp.newFolder()

    val gitRemote = Sgit.init(remoteWorkFolder)
    gitRemote.configSetLocal("user.email", "you@example.com")
    gitRemote.configSetLocal("user.name", "Your Name")

    val f1 = SgitTest.testFile(gitRemote.file, "pom.xml")
    Util.write(f1,
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
        |   see also: http://semver.org/
        |---------
        |Enter the release version
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

    TermTest.testSys(Seq("", "", "y", ""), expected, "", outFn = repSha)(sys => {
      val opts = Opts(useJlineInput = false)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, "abc",
        ReleaseConfig.default(true), new Repo(opts), opts)

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
    Util.write(f1,
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

    TermTest.testSys(Seq("", "y", ""), expected, "", outFn = repSha)(sys => {
      val opts = Opts(useJlineInput = false, versionIncrement = Increment.patch)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, "abc",
        ReleaseConfig.default(true), new Repo(opts), opts)

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
    Util.write(f1,
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

    TermTest.testSys(Seq("", "y", ""), expected, "", outFn = repSha)(sys => {
      val opts = Opts(useJlineInput = false, versionIncrement = Increment.minor)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, "abc",
        ReleaseConfig.default(true), new Repo(opts), opts)

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
    Util.write(f1,
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

    TermTest.testSys(Seq("", "y", ""), expected, "", outFn = repSha)(sys => {
      val opts = Opts(useJlineInput = false, versionIncrement = Increment.major)
      Release.work(localWorkFolder, sys,
        rebaseFn = () => {

        }, branch = "master", gitLocal, term, 72, "abc",
        ReleaseConfig.default(true), new Repo(opts), opts)

    })

  }
}
