package release

import org.junit.{Assert, Test}
import org.scalatest.junit.AssertionsForJUnit

class AetherTest extends AssertionsForJUnit {

  // convert to IT

  @Test
  def testExistsGav(): Unit = {
    Assert.assertTrue(Aether.existsGav("com.novomind.ishop.exi", "ext-b2c", "27.0.6"))
    Assert.assertFalse(Aether.existsGav("com.novomind.ishop.exi", "ext-b2c", "0.0.1-BERT"))
  }

}
