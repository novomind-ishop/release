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

}
