package release

import java.io.File
import java.util.regex.Pattern

import org.junit.{Assert, Test}
import org.scalatest.junit.AssertionsForJUnit

class ReleaseTest extends AssertionsForJUnit {

  @Test
  def testLines(): Unit = {
    val testFile = new File(Util.localWork, "target/grep1.txt")
    if (testFile.isFile) {
      Util.delete(testFile)
    }
    Util.write(testFile, Seq("", "a", "bert-SNAPSHOT", "bert-SNAP", "otto-SNAPSHOT"))

    val check = Release.findBadLines(Pattern.compile("-SNAPSHOT"))(testFile.getAbsolutePath)

    Assert.assertEquals(Seq((2, "bert-SNAPSHOT", testFile.toPath), (4, "otto-SNAPSHOT", testFile.toPath)), check)
  }

  @Test
  def testFormatVersionLines(): Unit = {

    val check = Release.formatVersionLines(Seq(
      "* com.novomind.ishop.core:ishop-core-projects:29.6.4-SNAPSHOT",
      "* com.novomind.ishop.core:ishop-api:1.0.2.1"))

    Assert.assertEquals(Seq(
      "* com.novomind.ishop.core:ishop-core-projects:  29.6.4-SNAPSHOT",
      "* com.novomind.ishop.core:ishop-api:            1.0.2.1"), check)
  }

}
