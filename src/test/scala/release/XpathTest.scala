package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import org.w3c.dom.Node

class XpathTest extends AssertionsForJUnit {

  @Test
  def testToMapOf_empty(): Unit = {

    val in: Seq[(String, String, Node)] = Nil

    Assert.assertEquals(Map.empty, Xpath.toMapOf(in))
  }

  @Test
  def testToMapOf(): Unit = {

    val in: Seq[(String, String, Node)] = Seq((" a ", " b ", null))

    Assert.assertEquals(Map(" a " -> " b "), Xpath.toMapOf(in))
  }
}
