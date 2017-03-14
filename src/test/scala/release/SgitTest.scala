package release

import java.io.File
import java.nio.file.{Files, StandardCopyOption}

import org.junit.{Assert, Assume, Ignore, Test}
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.JavaConverters

class SgitTest extends AssertionsForJUnit {

  @Test
  def testSelectGitCmd(): Unit = {
    val git = Sgit.selectedGitCmd()

    System.getProperty("os.name") match {
      case "Windows 10" ⇒ {
        Assert.assertEquals(Seq("C:\\Programme\\Git\\bin\\git.exe"), git)
      }
      case "Linux" ⇒ {
        Assert.assertEquals(Seq("git"), git)
      }
      case other ⇒ Assert.fail("unknown os: " + other + " => " + git)
    }
  }

  @Test
  def testCommitId(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val commitId = status.commitId("606411c")

    // THEN
    Assert.assertEquals("606411c1e4d62030144e9351fe0567cf3a5e5046", commitId)
  }

  @Test
  def testFindUpstream(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()
    val currentBranch = status.currentBranch
    Assume.assumeTrue("not master", currentBranch.contains("master"))

    // WHEN
    val branch = status.findUpstreamBranch().getOrElse("other")

    // THEN
    Assert.assertEquals("master", branch)
  }

  @Test
  def testCurrentBranch(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val branch = status.currentBranch

    // THEN
    Assert.assertTrue("unknown branch " + branch, Seq("master", "HEAD").contains(branch))
  }

  @Test
  @Ignore
  def testCommitIdHEAd(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val commitId = status.commitId("origin/master")

    // THEN
    Assert.assertEquals("f5d9a967a8971d522d903810d1a2cd804d08a077", commitId)
  }

  @Test
  @Ignore
  def testCommitIds(): Unit = {
    // GIVEN
    val status = Sgit(Util.localWork, showGitCmd = false, doVerify = true)

    // WHEN
    val commitIds = status.commitIds("60f6fe7", "3ac175e").map(_.substring(0, 7))

    // THEN
    Assert.assertTrue(commitIds.isInstanceOf[List[String]])
    Assert.assertEquals(Seq("60f6fe7", "f5f8fe3", "626cfb6"), commitIds)
  }

  @Test
  def testSelectUpstream_master(): Unit = {

    Assert.assertEquals(Some("master"), SgitTest.workSgit().selectUpstream(Some("origin/master")))

  }

  @Test
  def testSelectUpstream_feature(): Unit = {

    Assert.assertEquals(Some("feature/any"), SgitTest.workSgit().selectUpstream(Some("origin/feature/any")))

  }

  @Test
  def testSelectUpstream_none(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(None))

  }

  @Test
  def testSelectUpstream_other_master(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(Some("other/master")))

  }

  @Test
  def testSelectUpstream_other_feature(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(Some("other/feature/any")))

  }

  @Test
  def testOutLogger(): Unit = {
    var s: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(syserrErrors = true, Seq("fetch"), _ ⇒ false, in ⇒ s = s :+ in)

    // WHEN
    testee.err("any")

    // THEN
    Assert.assertEquals(Seq("fetch", "err: any"), s)
  }

  @Test
  def testOutLoggerEmpty(): Unit = {
    var s: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(syserrErrors = true, Seq("fetch"), in ⇒ in == "fetch", in ⇒ s = s :+ in)

    // WHEN
    testee.err("Total 31 (delta 0), reused 0 (delta 0)")

    // THEN
    Assert.assertEquals(Nil, s)
  }

  private def unw(in: String) = {
    in.trim match {
      case in: String if in.startsWith("\"") && in.endsWith("\"") ⇒ in.replaceFirst("^[\"]+", "").replaceFirst("[\"]+$", "")
      case in ⇒ in
    }
  }

  @Test
  def testUnw(): Unit = {
    Assert.assertEquals("a", unw("a"))
    Assert.assertEquals("a\na", unw("a\na"))
    Assert.assertEquals("a\na ", unw("\"a\na \""))
    Assert.assertEquals("a\na ", unw("\"a\na \" "))
    Assert.assertEquals("a\na ", unw("\"a\na \" \n"))
    Assert.assertEquals("a\na ", unw("\"a\na \"\n"))
    Assert.assertEquals("a\na ", unw("\"\"a\na \"\"\n"))
    Assert.assertEquals("a\n\"a ", unw("\"\"a\n\"a \"\"\n"))
  }

  @Test
  def testInitCloneCommitPoms(): Unit = {

    def testFile(folder: File, name: String): File = {
      val testFile = new File(folder, name)
      testFile.createNewFile()
      testFile
    }

    // GIVEN
    val testRepoA = new File(Util.localWork, "target/a")
    if (testRepoA.isDirectory) {
      Util.delete(testRepoA)
    }

    val testRepoB = new File(Util.localWork, "target/b")
    if (testRepoB.isDirectory) {
      Util.delete(testRepoB)
    }

    def write(f: File, content: Seq[String]): Unit = {
      Files.write(f.toPath, JavaConverters.bufferAsJavaList(content.toBuffer))
    }

    def copyMsgHook(to: File): Unit = {
      if (SgitTest.hasCommitMsg) {
        Files.copy(SgitTest.commitMsg, to.toPath.resolve(".git/hooks/commit-msg"), StandardCopyOption.REPLACE_EXISTING)
      }
    }

    def assertMsg(expected: Seq[String], sgit: Sgit): Unit = {
      val gitRawOut = sgit.gitNative(Seq("log", "-n1", "--pretty=\"%B\""))
      println("rawout: ^" + gitRawOut + "^")
      val unwrapped = unw(gitRawOut)
      println("unwrapped: ^" + unwrapped + "^")
      val nativeLines = unwrapped.lines.toList
      val body = nativeLines match {
        case lines if lines.last.startsWith("Change-Id:") ⇒ lines.dropRight(1)
        case lines ⇒ Assert.fail("invalid lines: " + lines.mkString("|"))
      }
      Assert.assertEquals(expected, body)
      val hookLines = nativeLines.takeRight(1)
      Assert.assertEquals("Change-Id:", hookLines.head.replaceFirst(" .*", "").trim)
    }

    // WHEN
    val gitA = Sgit.init(testRepoA, showGitCmd = true, SgitTest.hasCommitMsg)
    copyMsgHook(testRepoA)
    gitA.add(testFile(testRepoA, "test"))
    gitA.commit("add test")
    Assert.assertEquals("master", gitA.currentBranch)
    Assert.assertEquals(None, gitA.findUpstreamBranch())
    gitA.config("receive.denyCurrentBranch", "warn")
    val gitB = Sgit.clone(testRepoA, testRepoB, showGitCmd = true, SgitTest.hasCommitMsg)
    copyMsgHook(testRepoB)

    Assert.assertEquals("master", gitB.currentBranch)
    Assert.assertEquals("master", gitB.findUpstreamBranch().get)
    Assert.assertFalse(gitB.hasChangesToPush)

    gitB.pushFor("master", "master", pushTags = false)

    val pomFile = testFile(testRepoB, "pom.xml")
    gitB.add(pomFile)
    val anyFile = testFile(testRepoB, "any.xml")
    gitB.add(anyFile)
    val subject = "add " + Seq(pomFile, anyFile).map(_.getName).mkString(", ") + "-"
    gitB.commit(subject + "\r\n\r\n test")

    assertMsg(Seq(subject, "", " test"), gitB)

    Assert.assertTrue(gitB.hasChangesToPush)
    Assert.assertFalse(gitB.hasLocalChanges)
    write(pomFile, Seq("a"))
    Assert.assertTrue(gitB.hasLocalChanges)
    gitB.doCommitPomXmls("update pom.xml\n\nSigned-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>")

    assertMsg(Seq("update pom.xml", "", "Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>"), gitB)

    write(anyFile, Seq("a"))
    try {
      gitB.doCommitPomXmls("update pom.xml")
      Assert.fail()
    } catch {
      case e: IllegalStateException ⇒ Assert.assertEquals("only pom changes are allowed = any.xml", e.getMessage)
    }
    Assert.assertTrue(gitB.hasChangesToPush)
    gitB.pushFor("master", "master", pushTags = false)
    gitB.add(anyFile)
    Assert.assertTrue(gitB.hasLocalChanges)
    gitB.commit("add " + Seq(anyFile).map(_.getName).mkString(", "))
    Assert.assertFalse(gitB.hasLocalChanges)

  }

}

object SgitTest {
  val commitMsg = Sgit.findGit(Util.localWork).toPath.resolve(".git/hooks/commit-msg")
  val hasCommitMsg = Files.exists(commitMsg)

  def workSgit(): Sgit = Sgit(Util.localWork, showGitCmd = false, doVerify = hasCommitMsg)
}
