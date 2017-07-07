package release

import org.junit.{Assert, Test}
import org.scalatest.junit.AssertionsForJUnit

class TermTest extends AssertionsForJUnit {

  @Test
  def testRemoveSnapshot_0(): Unit = {
    val out = Term.removeSnapshot("RC-2009.01")

    Assert.assertEquals("RC-2009.01", out)
  }

  @Test
  def testRemoveSnapshot_1(): Unit = {
    val out = Term.removeSnapshot("0.1.1-SNAPSHOT")

    Assert.assertEquals("0.1.1", out)
  }

  @Test
  def testRemoveSnapshot_2(): Unit = {
    val out = Term.removeSnapshot("hallo-SNAPSHOT-SNAPSHOT")

    Assert.assertEquals("hallo", out)
  }

}
