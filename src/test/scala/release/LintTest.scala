package release

import org.scalatestplus.junit.AssertionsForJUnit
import com.google.googlejavaformat.java.Formatter
import org.eclipse.aether.repository.RemoteRepository
import org.junit.{Assert, Ignore, Rule, Test}
import org.junit.rules.TemporaryFolder
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Lint.{BranchTagMerge, NePrLa, PackageResult}
import release.ProjectMod.Gav3
import release.Repo.ReachableResult
import release.Starter.{LintOpts, Opts}

import java.time.Duration

class LintTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  def br(branchName: String) = Some(BranchTagMerge(tagName = None, branchName = Some(branchName)))

  def tag(tagName: String) = Some(BranchTagMerge(tagName = Some(tagName), branchName = None))

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
    Assert.assertFalse(Lint.versionMissmatches("45x-SNAPSHOT", br("feature/45x")))
    Assert.assertFalse(Lint.versionMissmatches("0.0.8-SNAPSHOT", br("feature/0x")))
    Assert.assertFalse(Lint.versionMissmatches("0.0.8-SNAPSHOT", br("feature/0.0x")))
    Assert.assertTrue(Lint.versionMissmatches("1.0.0-M1-SNAPSHOT", br("feature/0x")))
    Assert.assertTrue(Lint.versionMissmatches("1.0.0-M1-SNAPSHOT", br("feature/1.1x")))
    Assert.assertTrue(Lint.versionMissmatches("master-SNAPSHOT", None))
    Assert.assertTrue(Lint.versionMissmatches("", Some(BranchTagMerge(tagName = None, branchName = None))))
    Assert.assertFalse(Lint.versionMissmatches("1.1.1-SNAPSHOT", br("main")))
    Assert.assertFalse(Lint.versionMissmatches("3.2.1-SNAPSHOT", br("master")))
    Assert.assertFalse(Lint.versionMissmatches("master-SNAPSHOT", br("master")))
    Assert.assertTrue(Lint.versionMissmatches("main-SNAPSHOT", br("master")))
    Assert.assertTrue(Lint.versionMissmatches("1.0.0-M1", br("feature/0x")))
    Assert.assertTrue(Lint.versionMissmatches("1.3.0-M1", Some(BranchTagMerge(tagName = None, branchName = None))))

    Assert.assertTrue(Lint.versionMissmatches("main-SNAPSHOT", tag("master")))
    Assert.assertFalse(Lint.versionMissmatches("1.2.3", tag("v1.2.3")))
    Assert.assertFalse(Lint.versionMissmatches("main", tag("vmain")))
    Assert.assertFalse(Lint.versionMissmatches("main", BranchTagMerge.merge))

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
