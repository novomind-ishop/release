package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

import scala.util.Success

class SuggestDockerTagTest extends AssertionsForJUnit {

  @Test
  def testSuggest_null(): Unit = {
    val tuple = SuggestDockerTag.suggest(null, null, None)
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2) // TODO fail?
  }

  @Test
  def testSuggest_empty(): Unit = {
    val tuple = SuggestDockerTag.suggest("", null, None)
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2) // TODO fail?
  }

  @Test
  def testSuggest_main(): Unit = {
    val tuple = SuggestDockerTag.suggest("main", null, None)
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_master_1(): Unit = {
    val tuple = SuggestDockerTag.suggest("master", null, Some("1.0.0-SNAPSHOT"))
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_master(): Unit = {
    val tuple = SuggestDockerTag.suggest("master", null, None)
    Assert.assertEquals("latest", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_tag(): Unit = {
    val tuple = SuggestDockerTag.suggest("v1.2.3", null, None)
    Assert.assertEquals("1.2.3", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_tag_tag(): Unit = {
    val tuple = SuggestDockerTag.suggest("v1.2.3", "v1.2.3", None)
    Assert.assertEquals("1.2.3", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_tag_tag_version(): Unit = {
    val tuple = SuggestDockerTag.suggest("v1.2.3", "v1.2.3", Some("1.0.0-SNAPSHOT"))
    Assert.assertEquals("1.2.3_aka_1_0_0_SNAPSHOT", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_tag_tag_version2(): Unit = {
    val tuple = SuggestDockerTag.suggest("v1.2.3", "v1.2.3", Some("1.2.3-SNAPSHOT"))
    Assert.assertEquals("1.2.3_aka_1_2_3_SNAPSHOT", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_tag_tag_version_match(): Unit = {
    val tuple = SuggestDockerTag.suggest("v1.2.3", "v1.2.3", Some("1.2.3"))
    Assert.assertEquals("1.2.3", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_tag_milestone(): Unit = {
    val tuple = SuggestDockerTag.suggest("v1.2.3-M1", null, None)
    Assert.assertEquals("1.2.3-M1", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_noV_tag_milestone(): Unit = {
    val tuple = SuggestDockerTag.suggest("1.2.3-M1", null, None)
    Assert.assertEquals("1.2.3-m1_b7b0aa67_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_noV_tag_beta(): Unit = {
    val value = "BETA-example.org-2"
    val tuple = SuggestDockerTag.suggest(value, value, None)
    Assert.assertEquals("example.org-2_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_noV_tag_beta2(): Unit = {
    val value = s"BETA-example.org-and-a-${List.fill(2)("very-").mkString("")}long-end-2"
    val tuple = SuggestDockerTag.suggest(value, value, None)
    Assert.assertEquals("example.org-and-a-very-very-long-end-2_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_noV_tag_rc(): Unit = {
    val value = "RC-2023.01"
    val tuple = SuggestDockerTag.suggest(value, value, None)
    Assert.assertEquals("RC-2023.01_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_noV_tag_rc1(): Unit = {
    val value = "RC-2023.01_1"
    val tuple = SuggestDockerTag.suggest(value, value, None)
    Assert.assertEquals("RC-2023.01_1_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_symbols_release(): Unit = {
    Assert.assertEquals("3f546f27_TEMP", SuggestDockerTag.suggest("release/v+", null, None)._1)
  }

  @Test
  def testSuggest_symbols(): Unit = {
    Assert.assertEquals("bur_5df686aa_TEMP", SuggestDockerTag.suggest("bür+ ", null, None)._1)
  }

  @Test
  def testSuggest_dash(): Unit = {
    Assert.assertEquals("03eb2a35_TEMP", SuggestDockerTag.suggest("_", null, None)._1)
  }

  @Test
  def testSuggest_minus(): Unit = {
    Assert.assertEquals("1f5345f9_TEMP", SuggestDockerTag.suggest("-", null, None)._1)
  }

  @Test
  def testSuggest_long(): Unit = {
    // he tag must be valid ASCII and can contain lowercase and uppercase letters, digits, underscores, periods, and hyphens.
    // It cannot start with a period or hyphen and must be no longer than 128 characters.
    // https://docs.docker.com/engine/reference/commandline/tag/#:~:text=The%20tag%20must%20be%20valid,no%20longer%20than%20128%20characters.
    val tuple = SuggestDockerTag.suggest(List.fill(2000)("n").mkString(""), null, None)
    Assert.assertEquals(127, tuple._1.length)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_feature(): Unit = {
    val tuple = SuggestDockerTag.suggest("feature/v1.2.3", null, None)
    Assert.assertEquals("1.2.3_7026bb42_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_feature_vLess(): Unit = {
    val tuple = SuggestDockerTag.suggest("feature/1.2.3", null, None)
    Assert.assertEquals("1.2.3_e99748d1_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_release(): Unit = {
    val tuple = SuggestDockerTag.suggest("release/v1.2.3", null, None)
    Assert.assertEquals("1.2.3_e3d473f9_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_release_v(): Unit = {
    val tuple = SuggestDockerTag.suggest("release/vRC-2023.44", null, None)
    Assert.assertEquals("rc-2023.44_25a8ed0f_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_release_rc(): Unit = {
    val tuple = SuggestDockerTag.suggest("release/RC-2023.44", null, None)
    Assert.assertEquals("RC-2023.44", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_some(): Unit = {
    val tuple = SuggestDockerTag.suggest(":some/v1.2.3-Ber::-", "", None)
    Assert.assertEquals("some-v1.2.3-ber_3416b6f8_TEMP", tuple._1)
    Assert.assertEquals(0, tuple._2)
  }

  @Test
  def testSuggest_external(): Unit = {
    TestHelper.assertException(expectedMsg = "invalid docker tag »44x�[5«; " +
      "docker tags must match pattern »[a-zA-Z0-9][a-zA-Z0-9_\\-\\.]{0,127}«." +
      " This will lead to »Error response from daemon: invalid reference format« from docker",
      classOf[IllegalArgumentException], () => {
        SuggestDockerTag.suggest("any", "any", None, "44x\u001B[5")
      })
  }

  @Test
  def testFindTagname_noDockerfiles(): Unit = {
    val tuple = SuggestDockerTag.findTagname(null, null, None, hasDockerfiles = false)
    Assert.assertEquals("no Dockerfiles", tuple.failed.get.getMessage)
  }

  @Test
  def testFindTagname_null(): Unit = {
    val tuple = SuggestDockerTag.findTagname(null, null, None, hasDockerfiles = true)
    Assert.assertEquals("nor tag nor ref", tuple.failed.get.getMessage)
  }

  @Test
  def testFindTagname_branch(): Unit = {
    val tuple = SuggestDockerTag.findTagname("main", null, None, hasDockerfiles = true)
    Assert.assertEquals("no tag", tuple.failed.get.getMessage)
  }

  @Test
  def testFindTagname_validTag(): Unit = {
    val tuple = SuggestDockerTag.findTagname("v1.0.0", "v1.0.0", None, hasDockerfiles = true)
    Assert.assertEquals(Success("v1.0.0"), tuple.get)
  }

  @Test
  def testFindTagname_validTag_milestone(): Unit = {
    val tuple = SuggestDockerTag.findTagname("v1.0.0-M1", "v1.0.0-M1", None, hasDockerfiles = true)
    Assert.assertEquals(Success("v1.0.0-M1"), tuple.get)
  }

  @Test
  def testFindTagname_validTag_milestone_withProject(): Unit = {
    val tuple = SuggestDockerTag.findTagname("v1.0.0-M1", "v1.0.0-M1", Some("1.0.0-M1"), hasDockerfiles = true)
    Assert.assertEquals(Success("v1.0.0-M1"), tuple.get)
  }

  @Test
  def testFindTagname_invalidVersion(): Unit = {
    val tuple = SuggestDockerTag.findTagname("v1.0.0-M1", "v1.0.0-M1", Some("1.0.0-SNAPSHOT"), hasDockerfiles = true)
    Assert.assertEquals("auto suggested docker tag » v1.0.0-M1_aka_1_0_0_SNAPSHOT « is no valid docker tag name. " +
      "This could lead to build problems later. " +
      "A git tag must match the pattern » ^v[0-9]+\\.[0-9]+\\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$ « to suggest valid docker tags. " +
      "It is also possible to export an environment variable e.g. HARBOR_TAG", tuple.get.failed.get.getMessage)
  }

  @Test
  def testFindTagname_invalidTag(): Unit = {
    val tuple = SuggestDockerTag.findTagname("main", "main", None, hasDockerfiles = true)
    Assert.assertEquals("auto suggested docker tag » main « is no valid docker tag name. " +
      "This could lead to build problems later. " +
      "A git tag must match the pattern » ^v[0-9]+\\.[0-9]+\\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$ « to suggest valid docker tags. " +
      "It is also possible to export an environment variable e.g. HARBOR_TAG", tuple.get.failed.get.getMessage)
  }

  @Test
  def testFindTagname_versionMissmatch(): Unit = {
    val tuple = SuggestDockerTag.findTagname("v2.0.0", "v2.0.0", Some("2.0.0-SNAPSHOT"), hasDockerfiles = true)
    Assert.assertEquals("auto suggested docker tag » v2.0.0_aka_2_0_0_SNAPSHOT « is no valid docker tag name. " +
      "This could lead to build problems later. " +
      "A git tag must match the pattern » ^v[0-9]+\\.[0-9]+\\.[0-9]+(?:-(?:RC|M)[1-9][0-9]*)?$ « to suggest valid docker tags. " +
      "It is also possible to export an environment variable e.g. HARBOR_TAG", tuple.get.failed.get.getMessage)
  }

  @Test
  def testAkaVersion(): Unit = {
    Assert.assertEquals("1_2_3_SNAPSHOT", SuggestDockerTag.akaVersion("1.2.3-SNAPSHOT"))
    Assert.assertEquals("1_2_3_SNAPSHOT", SuggestDockerTag.akaVersion("(1.2..3-SNAPSHOT//"))
  }

}
