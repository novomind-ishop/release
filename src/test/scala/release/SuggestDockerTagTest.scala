package release

import org.scalatestplus.junit.AssertionsForJUnit
import org.junit.{Assert, Ignore, Rule, Test}

class SuggestDockerTagTest extends AssertionsForJUnit {

  @Test
  def testSuggest_null(): Unit = {
    val tuple = SuggestDockerTag.suggest(null)
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2) // TODO fail?
  }

  @Test
  def testSuggest_empty(): Unit = {
    val tuple = SuggestDockerTag.suggest("")
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2) // TODO fail?
  }

  @Test
  def testSuggest_main(): Unit = {
    val tuple = SuggestDockerTag.suggest("main")
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_master(): Unit = {
    val tuple = SuggestDockerTag.suggest("master")
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_tag(): Unit = {
    val tuple = SuggestDockerTag.suggest("v1.2.3")
    Assert.assertEquals("1.2.3", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_feature(): Unit = {
    val tuple = SuggestDockerTag.suggest("feature/v1.2.3")
    Assert.assertEquals("v1.2.3", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_release(): Unit = {
    val tuple = SuggestDockerTag.suggest("release/v1.2.3")
    Assert.assertEquals("v1.2.3", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_some(): Unit = {
    val tuple = SuggestDockerTag.suggest(":some/v1.2.3-Ber::-")
    Assert.assertEquals("some-v1.2.3-ber", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }
}
