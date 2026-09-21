package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

class SuggestVersionTest extends AssertionsForJUnit {

  private def assertSuggestion(expectedVersion: String, expectedExitCode: Int)(
      commitRef: String,
      tagName: String,
      projectVersion: Option[String] = None,
      externalTag: String = ""): Unit = {
    val result = SuggestVersion.suggest(commitRef, tagName, projectVersion, externalTag)
    Assert.assertEquals(expectedVersion, result._1)
    Assert.assertEquals(expectedExitCode, result._2)
  }

  @Test
  def testDevelopMainMaster(): Unit = {
    assertSuggestion("1.2.3", 0)(commitRef = "master", tagName = "v1.2.3")
    assertSuggestion("develop-SNAPSHOT", 0)(commitRef = "develop", tagName = null)
    assertSuggestion("main-SNAPSHOT", 0)(commitRef = "main", tagName = null)
    assertSuggestion("master-SNAPSHOT", 0)(commitRef = "master", tagName = null)
  }

  @Test
  def noVersionSource(): Unit = {
    assertSuggestion("main-SNAPSHOT", 0)(null, null, None)
  }

  @Test
  def blankValuesAreNotVersionSources(): Unit = {
    assertSuggestion("main-SNAPSHOT", 0)("  ", "\t", Some(" "), "\n")
  }

  @Test
  def projectVersionIsTheFallback(): Unit = {
    assertSuggestion("1.2.3-SNAPSHOT", 0)(
      "feature/some-change",
      null,
      Some("1.2.3-SNAPSHOT"))
  }

  @Test
  @scala.annotation.nowarn("msg=possible missing interpolator")
  def propertyBasedProjectVersionIsPreserved(): Unit = {
    assertSuggestion("${revision}", 0)("main", null, Some(" ${revision} "))
  }

  @Test
  def releaseBranchOverridesProjectVersion(): Unit = {
    assertSuggestion("2.0.0-RC1", 0)(
      "release/v2.0.0-RC1",
      null,
      Some("1.2.3-SNAPSHOT"))
  }

  @Test
  def shopReleaseBranchIsRecognized(): Unit = {
    assertSuggestion("RC-2026.39", 0)("release/vRC-2026.39", null, None)
  }

  @Test
  def invalidReleaseBranchFallsBackToProjectVersion(): Unit = {
    assertSuggestion("1.2.3-SNAPSHOT", 0)(
      "release/v+",
      null,
      Some("1.2.3-SNAPSHOT"))
  }

  @Test
  def tagOverridesCommitRefAndProjectVersion(): Unit = {
    assertSuggestion("3.4.5", 0)(
      "main",
      "refs/tags/v3.4.5",
      Some("3.4.5-SNAPSHOT"))
  }

  @Test
  def milestoneTagIsRecognized(): Unit = {
    assertSuggestion("3.4.5-M2", 0)("main", "v3.4.5-M2", None)
  }

  @Test
  def bareVersionTagIsRecognized(): Unit = {
    assertSuggestion("3.4.5", 0)("main", "3.4.5", None)
  }

  @Test
  def versionLikeCommitRefIsRecognizedWithoutTagName(): Unit = {
    assertSuggestion("3.4.5", 0)("v3.4.5", null, None)
  }

  @Test
  def externalVersionHasHighestPriority(): Unit = {
    assertSuggestion("4.5.6", 0)(
      "release/v2.0.0",
      "v3.0.0",
      Some("1.0.0"),
      "v4.5.6")
  }

  @Test
  def validExternalVersionMayContainBuildMetadata(): Unit = {
    assertSuggestion("4.5.6+build.7", 0)(null, null, None, "v4.5.6+build.7")
  }

  @Test
  def blankExternalVersionIsIgnored(): Unit = {
    assertSuggestion("3.0.0", 0)("main", "v3.0.0", None, " \t")
  }

  @Test
  def nonVersionTagFallsBackToProjectVersion(): Unit = {
    assertSuggestion("1.2.3-SNAPSHOT", 0)(
      "feature/work",
      "nightly",
      Some("1.2.3-SNAPSHOT"))
  }

  @Test
  def invalidTagFallsBackToVersionFromCommitRef(): Unit = {
    assertSuggestion("2.0.0", 0)("release/v2.0.0", "vnot-a-version", None)
  }

  @Test
  def invalidExternalVersionIsRejected(): Unit = {
    TestHelper.assertException(
      "invalid version »1.2.3␍␊«; versions must match pattern »[a-zA-Z0-9][a-zA-Z0-9._+\\-]*«",
      classOf[IllegalArgumentException],
      () => SuggestVersion.suggest("main", null, Some("1.2.3-SNAPSHOT"), "1.2.3\r\n")
    )
  }

  @Test
  def externalVersionMustNotStartWithPunctuation(): Unit = {
    TestHelper.assertException(
      "invalid version ».1.2.3«; versions must match pattern »[a-zA-Z0-9][a-zA-Z0-9._+\\-]*«",
      classOf[IllegalArgumentException],
      () => SuggestVersion.suggest(null, null, None, ".1.2.3")
    )
  }
}
