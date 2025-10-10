package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.Gav

class GavTest extends AssertionsForJUnit {
  @Test
  def testSelectUnusualReason_anone(): Unit = {
    Assert.assertEquals(None, Gav.selectUnusualReason(Gav(groupId = "a", artifactId = "a", version = Some("v"))))
  }

  found copies
  , use only one dependency
  @Test
  def testSelectUnusualReason_SNAPSHOT(): Unit = {
    Assert.assertEquals(Some("uses strange version SNAPSHOT, please do not use reserved words »SNAPSHOT«."),
      Gav.selectUnusualReason(Gav(groupId = "a", artifactId = "a", version = Some("SNAPSHOT"))))
    Assert.assertEquals(Some("uses strange version SNAPSHOT, please do not use reserved words »SNAPSHOT-SNAPSHOT«."),
      Gav.selectUnusualReason(Gav(groupId = "a", artifactId = "a", version = Some("SNAPSHOT-SNAPSHOT"))))
  }

  @Test
  def testSelectUnusualReason_RELEASE(): Unit = {
    Assert.assertEquals(Some("uses version »RELEASE« that is part of unstable markers LATEST and RELEASE. " +
      "Please use unambiguous versions only."),
      Gav.selectUnusualReason(Gav(groupId = "a", artifactId = "a", version = Some("RELEASE"))))
    Assert.assertEquals(Some("uses version »LATEST« that is part of unstable markers LATEST and RELEASE. " +
      "Please use unambiguous versions only."),
      Gav.selectUnusualReason(Gav(groupId = "a", artifactId = "a", version = Some("LATEST"))))
    // TODO RELEASE-SNAPSHOT, LATEST-SNAPSHOT
  }
}
