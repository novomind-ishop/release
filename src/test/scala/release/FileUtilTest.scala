package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

import java.io.File

class FileUtilTest extends AssertionsForJUnit {
  @Test
  def testReadLines(): Unit = {
    TestHelper.assertExceptionWithCheck(msg =>
      Assert.assertEquals("... is no regular file", msg.replaceFirst("^[^ ]+ ", "... ")), classOf[IllegalStateException],
      () => FileUtils.readLines(new File("fasdf")))

    Assert.assertNotEquals(Nil, FileUtils.readLines(new File(".gitattributes")))
  }

  @Test
  def testFindAllInFile_licence(): Unit = {
    Assert.assertEquals(Seq(("                                 Apache License", 1)),
      FileUtils.findAllInFile(new File("LICENSE").toPath, l => (l.contains("  Apache License"), l)))
  }

  @Test
  def testFindAllInFile_you(): Unit = {
    Assert.assertEquals(Seq(
      ("      the terms of any separate license agreement you may have executed", 135),
      ("      of your accepting any such warranty or additional liability.", 174),
      ("   APPENDIX: How to apply the Apache License to your work.", 178),
      ("      To apply the Apache License to your work, attach the following", 180),
      ("      replaced with your own identifying information. (Don't include", 182),
      ("   you may not use this file except in compliance with the License.", 192),
    ),
      FileUtils.findAllInFile(new File("LICENSE").toPath, l => (l.contains("you"), l)))
  }
}
