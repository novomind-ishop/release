package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

class SuggestVersionTest extends AssertionsForJUnit {

  private def assertSuggestion(expectedVersion: String, expectedExitCode: Int)(
      commitRef: String,
      tagName: String,
      projectVersion: Option[String] = None,
      externalTag: String = "",
      branchNames: Seq[String] = Nil,
      tagNames: Seq[String] = Nil): Unit = {
    val result = SuggestVersion.suggest(commitRef, tagName, projectVersion, externalTag, branchNames, tagNames)
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
      null,
      null,
      Some("1.2.3-SNAPSHOT"))
  }

  @Test
  @scala.annotation.nowarn("msg=possible missing interpolator")
  def propertyBasedProjectVersionIsPreserved(): Unit = {
    assertSuggestion("${revision}", 0)(null, null, Some(" ${revision} "))
  }

  @Test
  def releaseBranchOverridesProjectVersion(): Unit = {
    assertSuggestion("release-v2.0.0-RC1-SNAPSHOT", 0)(
      "release/v2.0.0-RC1",
      null,
      Some("1.2.3-SNAPSHOT"))
  }

  @Test
  def shopReleaseBranchIsRecognized(): Unit = {
    assertSuggestion("release-vRC-2026.39-SNAPSHOT", 0)("release/vRC-2026.39", null, None)
  }

  @Test
  def invalidReleaseBranchBecomesSnapshot(): Unit = {
    assertSuggestion("release-v+-SNAPSHOT", 0)(
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
  def tagDoesNotEvaluateProjectVersionFallback(): Unit = {
    var evaluated = false
    def projectVersion: Option[String] = {
      evaluated = true
      Some("1.2.3-SNAPSHOT")
    }

    Assert.assertEquals(("3.4.5", 0), SuggestVersion.suggest("main", "v3.4.5", projectVersion))
    Assert.assertFalse(evaluated)
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
    assertSuggestion("v3.4.5-SNAPSHOT", 0)("v3.4.5", null, None)
  }

  @Test
  def releaseBranchesNeverProduceTagVersions(): Unit = {
    assertSuggestion("release-v2.0.0-SNAPSHOT", 0)(
      commitRef = "release/v2.0.0",
      tagName = null,
      branchNames = Seq("release/v2.0.0"),
      tagNames = Seq("v2.0.0"))
  }

  @Test
  def qaMainPreservesProjectVersionWithExistingReleaseBranches(): Unit = {
    assertSuggestion("50.0.0-SNAPSHOT", 0)(
      commitRef = "qa/main",
      tagName = null,
      projectVersion = Some("50.0.0-SNAPSHOT"),
      branchNames = Seq("release/45x", "release/46x", "qa/main"),
      tagNames = Seq("v45.0.0", "v46.0.0")
    )
  }

  @Test
  def qaMainGenerateReleaseBranches(): Unit = {
    assertSuggestion("47.0.0-SNAPSHOT", 0)(
      commitRef = "qa/main",
      tagName = null,
      branchNames = Seq("release/45x", "release/46x", "qa/main", "main"),
      tagNames = Seq()
    )
  }

  @Test
  def mainGenerateReleaseBranches(): Unit = {
    assertSuggestion("47.0.0-RC-SNAPSHOT", 0)(
      commitRef = "main",
      tagName = null,
      branchNames = Seq("release/45x", "release/46x", "qa/main", "main"),
      tagNames = Seq()
    )
  }

  @Test
  def nextReleaseUsesHighestNumericBranch(): Unit = {
    assertSuggestion("101.0.0-SNAPSHOT", 0)(
      commitRef = "refs/heads/qa/main",
      tagName = null,
      branchNames = Seq("release/9x", "refs/heads/release/100x", "release/10x", "release/999x-fix")
    )
  }

  @Test
  def equivalentExistingTagIsAnErrorAndIsNeverIncremented(): Unit = {
    TestHelper.assertException(
      "version 2.0.0 already exists as Git tag 2.0.0",
      classOf[IllegalArgumentException],
      () =>
        SuggestVersion.suggest(
          commitRef = "v2.0.0",
          tagName = "v2.0.0",
          projectVersion = None,
          branchNames = Nil,
          tagNames = Seq("v2.0.0", "2.0.0"))
    )
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
  def externalVersionMustMatchVersionPatterns(): Unit = {
    Seq("v4.5.6+build.7", "1.2.3.4", "1.2.3-foo.bar").foreach { version =>
      TestHelper.assertException(
        s"invalid version »$version«; versions must match a release version pattern in release.Version",
        classOf[IllegalArgumentException],
        () => SuggestVersion.suggest(null, null, None, version)
      )
    }
    assertSuggestion("1.2.3_4", 0)(null, null, None, "v1.2.3_4")
    assertSuggestion("RC-2026.39", 0)(null, null, None, "RC-2026.39")
  }

  @Test
  def blankExternalVersionIsIgnored(): Unit = {
    assertSuggestion("3.0.0", 0)("main", "v3.0.0", None, " \t")
  }

  @Test
  def nonVersionTagFallsBackToBranch(): Unit = {
    assertSuggestion("work-SNAPSHOT", 0)(
      "feature/work",
      "nightly",
      Some("1.2.3-SNAPSHOT"))
  }

  @Test
  def invalidTagFallsBackToSnapshotFromCommitRef(): Unit = {
    assertSuggestion("release-v2.0.0-SNAPSHOT", 0)("release/v2.0.0", "vnot-a-version", None)
    assertSuggestion("main-SNAPSHOT", 0)("main", "v1.2.3+build.7", None)
    assertSuggestion("main-SNAPSHOT", 0)("main", "v1.2.3.4", None)
  }

  @Test
  def invalidExternalVersionIsRejected(): Unit = {
    TestHelper.assertException(
      "invalid version »1.2.3␍␊«; versions must match a release version pattern in release.Version",
      classOf[IllegalArgumentException],
      () => SuggestVersion.suggest("main", null, Some("1.2.3-SNAPSHOT"), "1.2.3\r\n")
    )
  }

  @Test
  def externalVersionMustNotStartWithPunctuation(): Unit = {
    TestHelper.assertException(
      "invalid version ».1.2.3«; versions must match a release version pattern in release.Version",
      classOf[IllegalArgumentException],
      () => SuggestVersion.suggest(null, null, None, ".1.2.3")
    )
  }

  @Test
  def branchFamiliesProduceReadableSnapshots(): Unit = {
    val examples = Seq(
      "cherry-pick-3e5fd96f" -> "cherry-pick-3e5fd96f",
      "docker-mirror" -> "docker-mirror",
      "feature-45x/formatting" -> "45x-formatting",
      "feature/45/ABC-123/something" -> "45x-abc-123",
      "feature/47x-release/CDF-321" -> "47x-cdf-321",
      "feature/48.x/C-17249/fix" -> "48x-c-17249",
      "feature/48x/demo/IFG-18099" -> "48x-ifg-18099",
      "feature/BO-16921_any_25" -> "bo-16921",
      "feature/IO-17403-to-qa" -> "io-17403",
      "feature/main/any-ABC-18065" -> "abc-18065",
      "feature/main/any/ABC-14208_2" -> "abc-14208_2",
      "feature/any/qa/49x/ABC-18100" -> "qa-49x-abc-18100",
      "feature/qa/48x/any/ABC-17629_2" -> "qa-48x-abc-17629_2",
      "feature/qa48/ABC-17422/doMagix" -> "qa-48x-abc-17422",
      "feature/qamain/ABC-17529/Apply" -> "qa-abc-17529",
      "feature/release48/ABC-17249/fix" -> "release-48x-abc-17249",
      "feature/support48/G-17059/DoIt" -> "support-48x-g-17059",
      "feature/support/45/ZDF-17201/camelCase" -> "support-45x-zdf-17201",
      "feature/v47/WE-17773" -> "47x-we-17773",
      "feature/x48/NO-17967" -> "48x-no-17967",
      "feature/JA-17060" -> "ja-17060",
      "feature/49x/any-3-upgrade" -> "49x-any-3-upgrade",
      "feature/any-room" -> "any-room",
      "main" -> "main",
      "qa/main" -> "qa-main",
      "qa/49x" -> "qa-49x",
      "release/45x" -> "45x-RC",
      "support/48x" -> "support-48x",
      "test100" -> "test100"
    )
    examples.foreach { case (branch, version) =>
      Seq(None, Some("99.0.0-SNAPSHOT")).foreach { project =>
        val branchSnapshot = if (version.endsWith("-SNAPSHOT")) version else version + "-SNAPSHOT"
        val expected = if (branch == "qa/main") project.getOrElse(branchSnapshot) else branchSnapshot
        Assert.assertTrue(branch, Version.isValidBranchSnapshot(branchSnapshot))
        Assert.assertEquals(branch, (expected, 0), SuggestVersion.suggest(branch, null, project))
        Assert.assertEquals(branch, (expected, 0), SuggestVersion.suggest("refs/heads/" + branch, null, project))
      }
    }
  }

  @Test
  def snapshotNormalizationAndFallback(): Unit = {
    assertSuggestion("some-change-SNAPSHOT", 0)("feature/some-change-SNAPSHOT", null)
    assertSuggestion("some-change-SNAPSHOT", 0)("feature/some--change", null)
    assertSuggestion("1.2.3-SNAPSHOT", 0)("feature/1.2.3", null)
    assertSuggestion("1.2.3-SNAPSHOT", 0)("///", null, Some("1.2.3-SNAPSHOT"))
    assertSuggestion("release-v2.0.0-SNAPSHOT", 0)("refs/heads/release/v2.0.0", null)
    assertSuggestion("3.0.0", 0)("feature/48x/ABC-17249", "v3.0.0")
    assertSuggestion("4.0.0", 0)("feature/48x/ABC-17249", "v3.0.0", None, "v4.0.0")
  }
}
