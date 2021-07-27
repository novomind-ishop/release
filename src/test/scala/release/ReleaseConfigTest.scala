package release

import java.io.File

import org.junit._
import org.junit.rules.TemporaryFolder
import org.scalatestplus.junit.AssertionsForJUnit

class ReleaseConfigTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  var file: File = null

  @Before
  def tempConfig(): Unit = {
    file = temp.newFile()
    file.deleteOnExit()
  }

  @After
  def cleanup(): Unit = {
    file.delete()
  }

  @Test
  def testParseConfig(): Unit = {
    Assert.assertEquals(Map.empty, ReleaseConfig.parseConfig(""))
    Assert.assertEquals(Map("e" -> ""), ReleaseConfig.parseConfig("e"))
    Assert.assertEquals(Map("a" -> "b"), ReleaseConfig.parseConfig("""a=b"""))
    Assert.assertEquals(Map("a" -> "b "), ReleaseConfig.parseConfig(""" a = b """))
    Assert.assertEquals(Map("a" -> "\" b \""), ReleaseConfig.parseConfig(""" a =" b """"))
    Assert.assertEquals(Map("a" -> "a:b"), ReleaseConfig.parseConfig(""" a =a:b"""))
    Assert.assertEquals(Map("a" -> "a:b"), ReleaseConfig.parseConfig(""" a =a\:b"""))
    Assert.assertEquals(Map("a.b.c" -> "Test <test-@ö> "), ReleaseConfig.parseConfig(""" a.b.c = Test <test-@ö> """))
  }
}
