package release

import java.io.File

import org.junit._
import org.junit.rules.TemporaryFolder
import org.scalatestplus.junit.AssertionsForJUnit
import redis.clients.jedis.Jedis

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
  def testJedis(): Unit = {
    try {
      val jedis = new Jedis("localhost", 12345, 20, 20)
      val pass = "none"
      // jedis.configSet("requirepass", pass)
      jedis.auth(pass)

      def push(key: String, value: String): Unit = {
        jedis.rpush(key, value)
        jedis.expire(key, 2)
      }

      push("foor", "hallo")
      val long = jedis.llen("foor")
      val value = jedis.lrange("foor", 0, long)

      println(value)
    } catch {
      case e: Throwable => println("E: " + e.getMessage)
    }
  }
}
