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
      Util.deleteRecursive(testFile)
    }
    Util.write(testFile, Seq("", "a", "bert-SNAPSHOT", "bert-SNAP", "otto-SNAPSHOT"))

    val check = Release.findBadLines(Pattern.compile("-SNAPSHOT"))(testFile.getAbsolutePath)

    Assert.assertEquals(Seq((2, "bert-SNAPSHOT", testFile.toPath), (4, "otto-SNAPSHOT", testFile.toPath)), check)
  }

  @Test
  def testFormatVersionLinesHighlight(): Unit = {

    val check = Release.formatVersionLinesGav(Seq(
      PomMod.Gav("com.novomind.ishop.core", "ishop-core-projects", "29.6.4-SNAPSHOT"),
      PomMod.Gav("com.novomind.ishop.core", "ishop-api", "1.0.2.1"),
      PomMod.Gav("na", "na", "1.0.2.1"),
      PomMod.Gav("any", "an", "2.2"),
      PomMod.Gav("any", "any", "2")
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
      PomMod.Gav("com.novomind.ishop.core", "ishop-core-projects", "29.6.4-SNAPSHOT"),
      PomMod.Gav("com.novomind.ishop.core", "ishop-api", "1.0.2.1"),
      PomMod.Gav("na", "na", "1.0.2.1"),
      PomMod.Gav("any", "ax", "2.2.2"),
      PomMod.Gav("any", "an", "2.2"),
      PomMod.Gav("any", "any", "2"),
      PomMod.Gav("", "any", "2"),
      PomMod.Gav("", "other", "7.21"),
      PomMod.Gav("", "", "2")
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

}
