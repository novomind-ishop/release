package release

import org.junit.{Assert, Assume, Test}
import org.scalatest.junit.AssertionsForJUnit
import release.Starter.Opts

class AetherTest extends AssertionsForJUnit {

  // convert to IT

  @Test
  def testExistsGav(): Unit = {
    val aether = new Aether(Opts())
    Assume.assumeTrue(aether.isReachable)
    Assert.assertTrue(aether.existsGav("com.novomind.ishop.exi", "ext-b2c", "27.0.6"))
    Assert.assertFalse(aether.existsGav("com.novomind.ishop.exi", "ext-b2c", "0.0.1-BERT"))
  }

}
