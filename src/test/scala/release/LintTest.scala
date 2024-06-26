package release

import com.google.googlejavaformat.java.Formatter
import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Lint.{BranchTagMerge, NePrLa, PackageResult}
import release.ProjectMod.Gav3

import java.time.Duration

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
  def testUnwantedPackage(): Unit = {
    val testee = PackageResult(names = Seq(
      "package a.bl",
      "package a.bl.ba",
      "package oi.io;",
      "package   a.j; ",
      " package a.s",
    ), Duration.ZERO,
      """a.bl;
        |oi.
        |package a.j;
        |package a.s
        |""".stripMargin)
    Assert.assertEquals(Seq(
      "a.bl",
      "oi.io;",
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
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("45x-SNAPSHOT", br("feature/45x")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("0.0.8-SNAPSHOT", br("feature/0x")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("0.0.8-SNAPSHOT", br("feature/0.0x")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("1.1.1-SNAPSHOT", br("main")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("3.2.1-SNAPSHOT", br("master")))
    Assert.assertEquals(Lint.MismatchResult.valid, Lint.versionMismatches("master-SNAPSHOT", br("master")))
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
        " Please use an plausible version marker and git marker combination like:" +
        " (project.version: 1.0.0-M1-SNAPSHOT -> git branch: feature/1x), ... \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("1.0.0-M1-SNAPSHOT", br("feature/0x")))
    Assert.assertEquals(Lint.MismatchResult.problem(" »1.0.0-M1-SNAPSHOT« does not relate to git branch: »feature/1.1x«." +
      " Please use an plausible version marker and git marker combination like:" +
      " (project.version: 1.0.0-M1-SNAPSHOT -> git branch: feature/1x), ... \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("1.0.0-M1-SNAPSHOT", br("feature/1.1x")))

    Assert.assertEquals(Lint.MismatchResult.problem(" »main-SNAPSHOT« does not relate to git tag: »master«." +
      " Please use an plausible version marker and git marker combination like:" +
      " (project.version: 1.2.3 -> git tag:v1.2.3), ... \uD83D\uDE2C RL1014"), Lint.versionMismatches("main-SNAPSHOT", tag("master")))
    Assert.assertEquals(Lint.MismatchResult.problem(" project.version »main-SNAPSHOT« does not relate to git branch: »master«." +
      " Please use an plausible version marker and git marker combination like:" +
      " (project.version: main-SNAPSHOT -> git branch:main), ... \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("main-SNAPSHOT", br("master")))
    Assert.assertEquals(
      Lint.MismatchResult.problem(" »1.0.0-M1« does not relate to git branch: »feature/0x«." +
        " Please use an plausible version marker and git marker combination like:" +
        " (project.version: 1.2.3 -> git tag:v1.2.3), ... \uD83D\uDE2C RL1014"), Lint.versionMismatches("1.0.0-M1", br("feature/0x")))
    Assert.assertEquals(Lint.MismatchResult.problem(" project.version »RC-2024.25-SNAPSHOT« is detached (HEAD). Maybe add a ref. \uD83D\uDE2C RL1014"),
      Lint.versionMismatches("RC-2024.25-SNAPSHOT", Some(BranchTagMerge(tagName = None, branchName = Some("HEAD")))))

  }

  @Test
  def testVersionMismatchMsgTagLikeInFeatureBranchWithVersion(): Unit = {

    val result = Lint.versionMismatches("1.0.1-management-SNAPSHOT",
      Some(BranchTagMerge(tagName = None, branchName = Some("feature/management"))))
    Assert.assertTrue(result.isMismatch)
    Assert.assertEquals(" »1.0.1-management-SNAPSHOT« does not relate to git branch: »feature/management«." +
      " Please use an plausible version marker and git marker combination like:" +
      " (project.version: 1.0.1-management-SNAPSHOT -> git branch: feature/1x)," +
      " (project.version: management-SNAPSHOT -> git branch: feature/management), ... \uD83D\uDE2C RL1014", result.msg)
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
