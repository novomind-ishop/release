package release

import java.time.{LocalDate, Month}

import org.junit.{Assert, Ignore, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.w3c.dom.Node
import release.PomMod.{Dep, PluginDep, PluginExec, PomRef}

class PomModTest extends AssertionsForJUnit {

  @Test
  def writeSelf(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")
    val targetPoms = TestHelper.testResources("novosales1")

    // WHEN
    PomMod(srcPoms)
      .writeTo(targetPoms)

    // THEN
    assert(Nil === TestHelper.gitDiff())
  }

  @Test
  def suggestReleaseVersionShop(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val releaseVersion = PomMod(srcPoms).suggestReleaseVersion()

    // THEN
    assert(Seq("27.0.0-SNAPSHOT") === releaseVersion)
  }

  @Test
  def findSelected(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    val pomMod = PomMod(srcPoms)
    // WHEN
    val nodes: Seq[Node] = pomMod.findNodes("com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT")

    // THEN
    Assert.assertEquals(4, nodes.size)
  }

  @Test
  @Ignore
  def depTreeFile(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")
    val pomMod = PomMod(srcPoms)
    // WHEN
    val treeFile: Option[String] = pomMod.depTreeFile()

    // THEN
    Assert.assertEquals(Some("asdf"), treeFile)
  }

  @Test
  def pluginDeps(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")
    val pomMod = PomMod(srcPoms)

    // WHEN
    val deps: Seq[PluginDep] = pomMod.listPluginDependecies

    // THEN
    assertPluginDeps(Novosales1Deps.plugins(), deps)
  }

  @Test
  @Ignore
  def findNodesAndSetVersion(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("novosales1")
    val orgMod = PomMod(orgPoms)
    val srcPoms = TestHelper.testResources("novosales3")
    orgMod.writeTo(srcPoms)

    val pomMod = PomMod(srcPoms)
    // WHEN
    pomMod.findNodesAndSetVersion("com.novomind.ishop.shops.novosales", "novosales-erp",
      "27.0.0-SNAPSHOT", "ublgusa")
    pomMod.writeTo(srcPoms)

    pomMod.findNodesAndSetVersion("com.novomind.ishop.shops.novosales", "novosales-erp",
      "ublgusa", "27.0.0-SNAPSHOT")
    pomMod.writeTo(srcPoms)
    // THEN

  }

  @Test
  def changeVersion(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("novosales1")
    val orgMod = PomMod(orgPoms)
    val srcPoms = TestHelper.testResources("novosales3")
    orgMod.writeTo(srcPoms)

    // WHEN
    val mod = PomMod(srcPoms)
    assertDeps(Novosales1Deps.selfVersion("27.0.0-SNAPSHOT"), mod.listSelf)
    assertDeps(Novosales1Deps.all(), mod.listDependecies)

    mod.changeVersion("RC-2017.01-SNAPSHOT")
    mod.writeTo(srcPoms)

    val allMod = Novosales1Deps.all()
      .map(dep ⇒ if (dep.artifactId == "novosales-erp" && dep.version.nonEmpty) {
        dep.copy(version = "RC-2017.01-SNAPSHOT")
      } else if (dep.artifactId == "novosales-projects" && dep.version.nonEmpty) {
        dep.copy(version = "RC-2017.01-SNAPSHOT")
      } else {
        dep
      }).map(dep ⇒
      dep.copy(pomRef = PomRef(dep.pomRef.id.replace("27.0.0-SNAPSHOT", "RC-2017.01-SNAPSHOT")))
    )

    // THEN
    val newMod = PomMod(srcPoms)
    assertDeps(allMod, newMod.listDependecies)
  }

  @Test
  def suggestReleaseVersion(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN
    val releaseVersion = PomMod(srcPoms).suggestReleaseVersion()

    // THEN
    assert(Seq("0.11") === releaseVersion)
  }

  @Test
  def suggestReleaseVersionFail(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1/novosales-erp")

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomMod(srcPoms).suggestReleaseVersion(),
      "novosales-erp as no version, please define", classOf[IllegalStateException])

  }

  @Test
  def suggestNextRelease(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val next = PomMod(srcPoms).suggestNextRelease("27.0.0")

    // THEN
    assert("27.0.1" === next)
  }

  @Test
  def suggestNextRelease_other(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val next = PomMod(srcPoms).suggestNextRelease("28.0.0")

    // THEN
    assert("28.0.1" === next)
  }

  @Test
  def suggestNextRelease_shop(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("RC-2017.02-SNAPSHOT", "RC-2017.02-SNAPSHOT")

    // THEN
    assert("RC-2017.03" === next)
  }

  @Test
  def suggestNextRelease_shop_patch(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("RC-2017.02.1-SNAPSHOT", "RC-2017.02.1-SNAPSHOT")

    // THEN
    assert("RC-2017.03" === next)
  }

  @Test
  def suggestNextRelease_shop_next_year(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("RC-2017.52-SNAPSHOT", "RC-2017.52-SNAPSHOT")

    // THEN
    assert("RC-2018.01" === next)
  }

  @Test
  def suggestNextRelease_shop_next(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("RC-2017.2-SNAPSHOT")

    // THEN
    assert("RC-2017.03" === next)
  }

  @Test
  def suggestNextRelease_shop_master(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("master-SNAPSHOT", "RC-2017.52-SNAPSHOT")

    // THEN
    assert("master" === next)
  }

  @Test
  def suggestNextRelease_any(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("mööp-SNAPSHOT")

    // THEN
    assert("mööp-UNDEF" === next)
  }

  @Test
  def suggestNextRelease_lowdash(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("28.0.0_1")

    // THEN
    assert("28.0.0_2" === next)
  }

  @Test
  def suggestRelease_shop(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "RC-2017.52-SNAPSHOT", hasShopPom = true)

    // THEN
    assert(Seq("RC-2017.52-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_other(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "3.43.2-SNAPSHOT", hasShopPom = false)

    // THEN
    assert(Seq("3.43.2") === release)
  }

  @Test
  def suggestRelease_other_lowdash(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "3.43.2_3-SNAPSHOT", hasShopPom = false)

    // THEN
    assert(Seq("3.43.2_3") === release)
  }

  @Test
  def suggestRelease_other_lowdash_text(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "3.43.2_mööp-SNAPSHOT", hasShopPom = false)

    // THEN
    assert(Seq("3.43.2_mööp") === release)
  }

  @Test
  def suggestRelease_other_master(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "master-SNAPSHOT", hasShopPom = false)

    // THEN
    assert(Seq("master") === release)
  }

  @Test
  def suggestRelease_shop_master(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "master-SNAPSHOT", hasShopPom = true)

    // THEN
    assert(Seq("RC-2017.05-SNAPSHOT", "RC-2017.06-SNAPSHOT", "RC-2017.07-SNAPSHOT") === release)
  }

  @Test
  def isShopPomSubmodule(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1/novosales-erp")

    // WHEN / THEN
    Assert.assertFalse(PomMod(srcPoms).hasShopPom)
  }

  @Test
  def isShopPomMini(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN / THEN
    Assert.assertFalse(PomMod(srcPoms).hasShopPom)
    Assert.assertTrue(PomMod(srcPoms).hasNoShopPom)
  }

  @Test
  def isShopPomShop(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN / THEN
    Assert.assertTrue(PomMod(srcPoms).hasShopPom)
  }

  @Test
  def writeOthers(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")
    val targetPoms = TestHelper.testResources("novosales2")

    // WHEN
    PomMod(srcPoms)
      .writeTo(targetPoms)

    // THEN
    assert(Nil === TestHelper.gitDiff())
  }

  @Test
  def listDependecies(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val deps = PomMod(srcPoms).listDependecies

    // THEN
    assertDeps(Novosales1Deps.all(), deps)
  }

  @Test
  def listSnapshots(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val deps = PomMod(srcPoms).listSnapshots

    // THEN
    assertDeps(Novosales1Deps.snapshots(), deps)
  }

  @Test
  def listSnapshotsMini(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN
    val deps = PomMod(srcPoms).listSnapshots

    // THEN
    assertDeps(Seq(Dep(PomRef("com.novomind.ishop.deployment:deployment-planning:0.11-SNAPSHOT"),
      "org.springframework", "spring-other", "0.11-SNAPSHOT", "", "", "")), deps)
  }

  @Test
  def listSnapshotsPacakgeDepsPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    val deps = PomMod(srcPoms).listSnapshots

    // THEN
    assertDeps(Nil, deps)
  }

  @Test
  def listSelfMod(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val deps = PomMod(srcPoms).selfDepsMod

    // THEN
    assertDeps(Novosales1Deps.selfMod(), deps)
  }

  @Test
  def listSelf(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val deps = PomMod(srcPoms).listSelf

    // THEN
    assertDeps(Novosales1Deps.self(), deps)
  }

  @Test
  def listSelfDepsPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    // WHEN
    val deps = PomMod(srcPoms).listSelf

    // THEN
    assertDeps(PomPackagedDeps.self(), deps)
  }

  @Test
  def testHasNoShopPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    // WHEN / THEN
    Assert.assertTrue(PomMod(srcPoms).hasNoShopPom)
  }

  @Test
  def testHasShopPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN / THEN
    Assert.assertTrue(PomMod(srcPoms).hasShopPom)
  }

  @Test
  def listProperties(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("novosales1")

    // WHEN
    val props = PomMod(srcPoms).listProperties

    // THEN
    val expected = Map(
      "ishop-core.version" → "27.1.2-SNAPSHOT",
      "project.build.sourceEncoding" → "UTF-8",
      "ishop-plugin.version" → "27.3.0-SNAPSHOT",
      "project.version" -> "27.0.0-SNAPSHOT",
      "webappDirectory" → "skip/skip",
      "aspectj.version" → "1.8.8",
      "aspectj-maven-plugin.version" → "1.8",
      "ishop-core-sub.version" → "27.1.2-SNAPSHOT",
      "build.timestamp" → "skip",
      "zkm.skip" → "true",
      "bo-client" → "27.0.0-SNAPSHOT",
      "subproject.property" -> "true",
      "allowedProjectDir" -> "skip/any",
      "project.reporting.outputEncoding" → "UTF-8",
      "java.version" → "1.8")
    Assert.assertEquals(expected, props)
  }

  @Test
  def replaceInOnline(): Unit = {
    val input = """<root><child>x</child></root>"""

    val result = PomMod.format(input, "/root/child", "y")

    Assert.assertEquals("""<root><child>y</child></root>""", result)
  }

  @Test
  def replaceInOnlineBy(): Unit = {
    val input =
      """<dependencies><dependency>      <groupId>com.novomind.ishop.shops.novosales</groupId><br />
        |      <artifactId>novosales-commons</artifactId>
        |      <version>27.0.0-SNAPSHOT</version></dependency><br    />
        |  <dependency>   <groupId>com.novomind.ishop.shops.novosales</groupId>
        |      <artifactId>novosales-erp</artifactId>
        |     <version>27.0.0-SNAPSHOT</version></dependency>
        |      </dependencies>""".stripMargin

    val result = PomMod.formatDependcy(input, "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "27.0.0")

    Assert.assertEquals(
      """<dependencies><dependency>      <groupId>com.novomind.ishop.shops.novosales</groupId><br />
        |      <artifactId>novosales-commons</artifactId>
        |      <version>27.0.0-SNAPSHOT</version></dependency><br />
        |  <dependency>   <groupId>com.novomind.ishop.shops.novosales</groupId>
        |      <artifactId>novosales-erp</artifactId>
        |     <version>27.0.0</version></dependency>
        |      </dependencies>""".stripMargin, result)
  }

  @Test
  def replaceIn(): Unit = {
    val input =
      """<root>
        |<any>y</any>
        |</root>""".stripMargin

    val result = PomMod.format(PomMod.format(input, "/root/any", "z"), "/any", "x")

    Assert.assertEquals(
      """<root>
        |<any>z</any>
        |</root>""".stripMargin, result)
  }

  private def assertPluginDeps(expected: Seq[PluginDep], actual: Seq[PluginDep]): Unit = {
    def defstr(dep: PluginDep): String = {
      def formatExec(pluginExec: Seq[PluginExec]): String = {
        "Seq(" + pluginExec.map(in ⇒ {
          val goals = in.goals.map(in ⇒ "\"" + in + "\"").mkString(",")
          val conf = in.config.map(in ⇒ "\"" + in._1 + "\" -> \"" + in._2 + "\"").mkString(",")
          "PluginExec(\"" + in.id + "\", Seq(" + goals + "), \"" + in.phase + "\", Map(" + conf + "))"
        }).mkString(", ") + ")"
      }

      "PluginDep(PomRef(\"" + dep.pomRef.id + "\"),\n \"" + dep.groupId + "\", \"" +
        dep.artifactId + "\", \"" + dep.version + "\", " + formatExec(dep.execs) +
        ",\n Seq(" + dep.pomPath.map(in ⇒ "\"" + in + "\"").mkString(", ") + "))"
    }

    assertBy(expected, actual, defstr)
  }

  private def assertDeps(expected: Seq[Dep], actual: Seq[Dep]) = {
    def defstr(dep: Dep): String = {
      "Dep(PomRef(\"" + dep.pomRef.id + "\"),\n \"" + dep.groupId + "\", \"" +
        dep.artifactId + "\", \"" + dep.version + "\", \"" + dep.typeN + "\", \"" + dep.scope + "\", \"" + dep.packaging + "\")"
    }

    assertBy(expected, actual, defstr)
  }

  private def assertBy[L](expected: Seq[L], actual: Seq[L], fn: L ⇒ String) = {
    Assert.assertEquals(expected.map(fn).mkString(",\n"), actual.map(fn).mkString(",\n"))
    Assert.assertEquals(expected, actual)
  }

}
