package release

import com.google.googlejavaformat.java.Formatter
import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Lint.{BranchTagMerge, NePrLa, PackageResult}
import release.ProjectMod.Gav3
import release.Sgit.{GitShaBranch, GitShaTag}

import java.time.{Duration, Period, ZonedDateTime}

class LintTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  def br(branchName: String) = Some(BranchTagMerge(tagName = None, branchName = Some(branchName)))

  def tag(tagName: String, branchName: Option[String] = None) = Some(BranchTagMerge(tagName = Some(tagName), branchName = branchName))

  @Test
  def testSelectPackage_blank(): Unit = {
    Assert.assertEquals(None, Lint.selectPackage(
      """
        |""".stripMargin.linesIterator))
  }

  @Test
  def testSelectPackage(): Unit = {
    Assert.assertEquals(Some("package a.b;"), Lint.selectPackage(
      """package a.b;
        |package c.b;
        |""".stripMargin.linesIterator))
  }

  @Test
  def testUnwantedPackage_timeout(): Unit = {
    val result = PackageResult.timeout(Duration.ofMillis(10), "hallo")
    Assert.assertEquals(Seq("timeout"), result.unwantedPackages)
  }

  @Test
  def testUnwantedPackage(): Unit = {
    val testee = PackageResult(names = Seq(
      "package a.bl",
      "package a.bl.ba",
      "package oi.io;",
      "package oi.package.io;",
      "package   a.j; ",
      " package a.s",
    ), Duration.ZERO,
      """a.bl;
        |oi.
        |package a.j;
        |package oi.package.io;
        |package a.s
        |""".stripMargin, msg = "")
    Assert.assertEquals(Seq(
      "a.bl",
      "oi.io;",
      "oi.package.io;",
      "a.j;",
      "a.s",
    ), testee.unwantedPackages)
  }

  @Test
  def testSelectPackage_with_header(): Unit = {
    Assert.assertEquals(Some("package a.b"), Lint.selectPackage(
      """
        |/*
        | * Copyright (c) 2001, 2018 and/or its affiliates. All rights reserved.
        |*/
        |package a.b
        |""".stripMargin.linesIterator))
  }

  @Test
  def testNextAndPrevious(): Unit = {
    val in = Gav3("a", "b", Some("1.0.0-M1"))
    Assert.assertEquals(None, Lint.selectNextAndPrevious(None, in))

    Assert.assertEquals(Some(
      NePrLa(next = Some(in.copy(version = Some("1.0.0"))),
        previous = Some(in.copy(version = Some("0.9"))),
        latest = None)),
      Lint.selectNextAndPrevious(Some(Seq("0.9", "1.0.0")), in))
    Assert.assertEquals(Some(
      NePrLa(next = Some(in.copy(version = Some("1.0.0"))),
        previous = Some(in.copy(version = Some("0.9"))),
        latest = Some(in.copy(version = Some("2.2.2"))))),
      Lint.selectNextAndPrevious(Some(Seq("0.8", "0.9", "1.0.0", "2.2.2")), in))

    Assert.assertEquals(Some(
      NePrLa(next = Some(in.copy(version = Some("1.0.0"))),
        previous = None,
        latest = None)),
      Lint.selectNextAndPrevious(Some(Seq("1.0.0")), in))

    Assert.assertEquals(Some(
      NePrLa(next = Some(in.copy(version = Some("1.0.1"))),
        previous = None,
        latest = None)),
      Lint.selectNextAndPrevious(Some(Seq("1.0.1")), in))

    Assert.assertEquals(Some(
      NePrLa(next = Some(in.copy(version = Some("1.0.0-M2"))),
        previous = None,
        latest = Some(in.copy(version = Some("1.0.0-M3")))
      )),
      Lint.selectNextAndPrevious(Some(Seq("1.0.0-M2", "1.0.0-M3")), in))

    Assert.assertEquals(Some(
      NePrLa(next = None,
        previous = Some(in.copy(version = Some("0.0.9"))),
        latest = None
      )),
      Lint.selectNextAndPrevious(Some(Seq("1.0.0-M1", "0.0.9")), in))

  }

  @Test
  def testVersionMatches(): Unit = {
    Assert.assertEquals(Lint.MismatchResult.valid,
      Lint.versionMismatches("1.2.3", tag("v1.2.3", branchName = Some("HEAD"))))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("RC-2024.31-SNAPSHOT", br("release/RC-2024.31")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("45x-SNAPSHOT", br("feature/45x")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("0.0.8-SNAPSHOT", br("feature/0x")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("0.0.8-SNAPSHOT", br("feature/0.0x")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("1.1.1-SNAPSHOT", br("main")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("3.2.1-SNAPSHOT", br("master")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("master-SNAPSHOT", br("master")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("test-SNAPSHOT", br("feature/test")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("bert-SNAPSHOT", br("release/bert")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("core44-SNAPSHOT", br("feature/core44")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("1.2.3", tag("v1.2.3")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("main", tag("vmain")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("main", BranchTagMerge.merge))

    Assert.assertEquals(
      Lint.MismatchResult.problem(" project.version »master« has no git. Please add some .git folder. \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("master", None))
    Assert.assertEquals(Lint.MismatchResult.problem(" project.version »master« is detached. Maybe add a ref. \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("master", Some(BranchTagMerge(tagName = None, branchName = None))))
    Assert.assertEquals(Lint.MismatchResult.problem(" project.version »« is detached. Maybe add a ref. \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("", Some(BranchTagMerge(tagName = None, branchName = None))))
    Assert.assertEquals(
      Lint.MismatchResult.problem(" »1.0.0-M1-SNAPSHOT« does not relate to git branch: »feature/0x«." +
        " Please use a plausible version marker and git marker combination like:" +
        " (project.version: 1.0.0-M1-SNAPSHOT -> git branch: feature/1x), ... \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("1.0.0-M1-SNAPSHOT", br("feature/0x")))
    Assert.assertEquals(Lint.MismatchResult.problem(" »1.0.0-M1-SNAPSHOT« does not relate to git branch: »feature/1.1x«." +
      " Please use a plausible version marker and git marker combination like:" +
      " (project.version: 1.0.0-M1-SNAPSHOT -> git branch: feature/1x), ... \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("1.0.0-M1-SNAPSHOT", br("feature/1.1x")))

    Assert.assertEquals(Lint.MismatchResult.problem(" »main-SNAPSHOT« does not relate to git tag: »master«." +
      " Please use a plausible version marker and git marker combination like:" +
      " (project.version: 1.2.3 -> git tag:v1.2.3), ... (hint: a git tag should not be a SNAPSHOT) \uD83D\uDE2C RL1014"), Lint.versionMismatches("main-SNAPSHOT", tag("master")))
    Assert.assertEquals(Lint.MismatchResult.problem(" »main-SNAPSHOT« does not relate to git tag: »main«." +
      " Please use a plausible version marker and git marker combination like:" +
      " (project.version: 1.2.3 -> git tag:v1.2.3), ... (hint: a git tag should not be a SNAPSHOT) \uD83D\uDE2C RL1014"), Lint.versionMismatches("main-SNAPSHOT", tag("main")))
    Assert.assertEquals(Lint.MismatchResult.problem(" »main« does not relate to git tag: »master«." +
      " Please use a plausible version marker and git marker combination like:" +
      " (project.version: 1.2.3 -> git tag:v1.2.3), ... \uD83D\uDE2C RL1014"), Lint.versionMismatches("main", tag("master")))
    Assert.assertEquals(Lint.MismatchResult.problem(" project.version »main-SNAPSHOT« does not relate to git branch: »master«." +
      " Please use a plausible version marker and git marker combination like:" +
      " (project.version: main-SNAPSHOT -> git branch:main), ... \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("main-SNAPSHOT", br("master")))
    Assert.assertEquals(
      Lint.MismatchResult.problem(" »1.0.0-M1« does not relate to git branch: »feature/0x«." +
        " Please use a plausible version marker and git marker combination like:" +
        " (project.version: 1.2.3 -> git tag:v1.2.3), ... \uD83D\uDE2C RL1014"), Lint.versionMismatches("1.0.0-M1", br("feature/0x")))
    Assert.assertEquals(Lint.MismatchResult.problem(" project.version »RC-2024.25-SNAPSHOT« is detached (HEAD). Maybe add a ref. \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("RC-2024.25-SNAPSHOT", Some(BranchTagMerge(tagName = None, branchName = Some("HEAD")))))
    Assert.assertEquals(Lint.MismatchResult.problem(" »master-SNAPSHOT« does not relate to git branch: »feature/core44«. " +
      "Please use a plausible version marker and git marker combination.\uD83D\uDE2C RL1014"),
      Lint.versionMismatches("master-SNAPSHOT", br("feature/core44")))

  }

  @Test
  def testVersionMismatchMsgTagLikeInFeatureBranchWithVersion(): Unit = {

    val result = Lint.versionMismatches("1.0.1-management-SNAPSHOT",
      Some(BranchTagMerge(tagName = None, branchName = Some("feature/management"))))
    Assert.assertTrue(result.isMismatch)
    Assert.assertEquals(" »1.0.1-management-SNAPSHOT« does not relate to git branch: »feature/management«." +
      " Please use a plausible version marker and git marker combination like:" +
      " (project.version: 1.0.1-management-SNAPSHOT -> git branch: feature/1x)," +
      " (project.version: management-SNAPSHOT -> git branch: feature/management), ... \uD83D\uDE2C RL1014", result.msg)
  }

  def tag(name: String, dateTime: ZonedDateTime) = {
    GitShaTag(sha1 = UtilTest.randomSha1(), tagName = "refs/tags/" + name, dateSupplier = () => Option(dateTime))
  }

  def brn(name: String, dateTime: ZonedDateTime) = {
    GitShaBranch(commitId = UtilTest.randomSha1(), branchName = "refs/heads/" + name, dateSupplier = () => Option(dateTime))
  }

  @Test
  def testRefFreq_single(): Unit = {
    val result = Lint.refFreqBranchTag(Seq(
      brn("develop", null),
      tag("v1.2.3", ZonedDateTime.parse("2024-02-13T08:19:20+01:00")),
      brn("main", ZonedDateTime.parse("2024-02-14T08:19:20+01:00")),
    ), currentDate = ZonedDateTime.parse("2024-03-13T08:19:20+01:00"))
    Assert.assertEquals((Period.parse("P-1D"), Period.parse("P-1D")), result)
  }

  @Test
  def testRefFreq_branch_2(): Unit = {
    val result = Lint.refFreqBranchTag(Seq(
      brn("feature/a", ZonedDateTime.parse("2024-02-13T08:19:20+01:00")),
      brn("main", ZonedDateTime.parse("2024-02-14T08:19:20+01:00")),
    ), currentDate = ZonedDateTime.parse("2024-03-13T08:19:20+01:00"))
    Assert.assertEquals((Period.parse("P1D"), Period.parse("P-1D")), result)
  }

  @Test
  def testRefFreq_branch_3(): Unit = {
    val result = Lint.refFreqBranchTag(Seq(
      brn("feature/a", ZonedDateTime.parse("2024-02-15T08:19:20+01:00")),
      brn("feature/b", ZonedDateTime.parse("2024-02-13T08:19:20+01:00")),
      brn("main", ZonedDateTime.parse("2024-02-14T08:19:20+01:00")),
    ), currentDate = ZonedDateTime.parse("2024-03-13T08:19:20+01:00"))
    Assert.assertEquals((Period.parse("P1D"),Period.parse("P-1D")), result)
  }

  @Test
  def testRefFreq_branch_tag(): Unit = {
    val result = Lint.refFreqBranchTag(Seq(
      brn("feature/a", ZonedDateTime.parse("2024-02-10T08:19:20+01:00")),
      brn("feature/forgotten", ZonedDateTime.parse("1909-02-20T08:19:20+01:00")),
      brn("main", ZonedDateTime.parse("2024-02-20T08:19:20+01:00")),
      tag("v1.2.3", ZonedDateTime.parse("2024-02-13T08:19:20+01:00")),
      tag("v1.2.4", ZonedDateTime.parse("2024-02-14T08:19:20+01:00")),
    ), currentDate = ZonedDateTime.parse("2024-03-13T08:19:20+01:00"))
    Assert.assertEquals((Period.parse("P10D"), Period.parse("P1D")), result)
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
}
