package release

import java.io.File
import org.junit._
import org.junit.rules.TemporaryFolder
import org.scalatestplus.junit.AssertionsForJUnit
import release.ReleaseConfig.WorkAndMirror

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

  @Test
  def testParseSettings2(): Unit = {
    val content =
      """
        |<settings>
        |  <profiles>
        |    <profile>
        |      <repositories>
        |        <repository>
        |          <url>https://ref-nexus.example.org/content/repositories/public</url>
        |        </repository>
        |        <repository>
        |          <url>http://central</url>
        |        </repository>
        |      </repositories>
        |      <pluginRepositories>
        |        <pluginRepository>
        |          <url>https://ref-nexus.example.org/content/repositories/public</url>
        |        </pluginRepository>
        |        <pluginRepository>
        |          <url>http://central</url>
        |        </pluginRepository>
        |      </pluginRepositories>
        |    </profile>
        |  </profiles>
        |  <mirrors>
        |    <mirror>
        |      <url>http://0.0.0.0/</url>
        |    </mirror>
        |    <mirror>
        |      <url>https://work-nexus.example.org/content/repositories/public</url>
        |    </mirror>
        |  </mirrors>
        |</settings>
        |""".stripMargin

    Assert.assertEquals(WorkAndMirror(
      workUrl = "https://work-nexus.example.org/content/repositories/public",
      mirrorUrl = "https://ref-nexus.example.org/content/repositories/public"),
      ReleaseConfig.extractWorkAndMirror(content).get)
  }

  @Test
  def testParseSettings1(): Unit = {
    val content =
      """
        |<settings>
        |  <profiles>
        |    <profile>
        |      <repositories>
        |        <repository>
        |          <url>http://central</url>
        |        </repository>
        |      </repositories>
        |     <pluginRepositories>
        |        <pluginRepository>
        |          <url>http://central</url>
        |        </pluginRepository>
        |      </pluginRepositories>
        |    </profile>
        |  </profiles>
        |  <mirrors>
        |    <mirror>
        |      <url>http://0.0.0.0/</url>
        |    </mirror>
        |    <mirror>
        |      <url>https://work-nexus.example.org/content/repositories/public</url>
        |    </mirror>
        |  </mirrors>
        |</settings>
        |""".stripMargin

    Assert.assertEquals(WorkAndMirror(
      workUrl = "https://work-nexus.example.org/content/repositories/public",
      mirrorUrl = "https://work-nexus.example.org/content/repositories/public"),
      ReleaseConfig.extractWorkAndMirror(content).get)
  }

  @Test
  def testParseSettingsInvalid(): Unit = {
    val content = "<sett"

    Assert.assertEquals(None, ReleaseConfig.extractWorkAndMirror(content))
  }

  @Test
  def testFromSettings(): Unit = {
    val result = ReleaseConfig.fromSettings()
    Assert.assertEquals(Repo.centralUrl, result.workNexusUrl())
    Assert.assertEquals(Repo.centralUrl, result.mirrorNexusUrl())
  }

  @Test
  def testFromSettingsSuccess(): Unit = {
    val content =
      """
        |<settings>
        |  <profiles>
        |    <profile>
        |      <repositories>
        |        <repository>
        |          <url>http://central</url>
        |        </repository>
        |      </repositories>
        |     <pluginRepositories>
        |        <pluginRepository>
        |          <url>http://central</url>
        |        </pluginRepository>
        |      </pluginRepositories>
        |    </profile>
        |  </profiles>
        |  <mirrors>
        |    <mirror>
        |      <url>http://0.0.0.0/</url>
        |    </mirror>
        |    <mirror>
        |      <url>https://work-nexus.example.org/content/repositories/public</url>
        |    </mirror>
        |  </mirrors>
        |</settings>
        |""".stripMargin
    val settings = temp.newFile("settings.xml")
    Util.write(settings, content)
    val result = ReleaseConfig.fromSettings(settings.getParentFile)
    Assert.assertEquals("https://work-nexus.example.org/content/repositories/public", result.workNexusUrl())
    Assert.assertEquals("https://work-nexus.example.org/content/repositories/public", result.mirrorNexusUrl())
  }
}
