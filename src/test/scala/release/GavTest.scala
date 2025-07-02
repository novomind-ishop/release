package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.Gav

class GavTest extends AssertionsForJUnit {
  @Test
  def testSelectUnusualReason_none(): Unit = {
    Assert.assertEquals(None, Gav.selectUnusualReason(Gav(groupId = "a", artifactId = "a", version = Some("v"))))
  }
}
