package release

import org.junit.{Assume, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

class AetherTest extends AssertionsForJUnit {

  // convert to IT

  @Test
  def testExistsGav(): Unit = {
    val aether = new Aether(Opts())
    Assume.assumeTrue(aether.isReachable(false))
    val g = "org.apache.maven.plugins"
    val a = "maven-surefire-plugin"
    val v = "3.0.0-M1"
    //println("r: " + aether.existsGav(g, a, v))
    println("r: " + aether.newerVersionsOf(g, a, v))
    // Assert.assertFalse(aether.existsGav("com.novomind.ishop.exi", "ext-b2c", "0.0.1-BERT"))
  }

}
