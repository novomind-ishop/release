package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Gav, PluginDep, PluginExec, SelfRef}

class PomCheckerTest extends AssertionsForJUnit {

  @Test
  def testCheck_ishop_maven_changes_before(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add \"check-for-changes-before\" to your ishop maven plugin",
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven_changes_package(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add \"check-for-changes-package\" to your ishop maven plugin",
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "",
      Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN
    PomChecker.checkIshopMaven(deps)

    // THEN
    // no exception
  }

  @Test
  def testCheck_ishop_maven_managed(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "",
      Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(SelfRef("com.novomind.ishop.shops:anyshop-any:27.0.0-SNAPSHOT:jar"),
        "com.novomind.ishop.maven", "ishop-maven-plugin", "", Nil,
        Seq("plugin", "plugins", "pluginManagement", "build", "project")))

    // WHEN
    PomChecker.checkIshopMaven(deps)

    // THEN
    // no exception
  }

  @Test
  def testCheck_ishop_maven_multiple_no_exec(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef("c.n.i.s:n:0.0.1-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("",
        Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(SelfRef("c.n.i.s:n-any:0.0.1-SNAPSHOT:jar"),
        "com.novomind.ishop.maven", "ishop-maven-plugin", "", Nil,
        Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("c.n.i.s:n-any:0.0.1-SNAPSHOT:jar - a single execution section for ishop maven plugin " +
      "please required. Input is Nil.", classOf[IllegalArgumentException], () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven_pom_path(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "",
      Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "profile", "profiles", "project")))

    // WHEN / THEN
    TestHelper.assertException("please check your pom.xml's and move your ishop-maven-plugin to <project>/<build>/<plugins>/<plugin>. " +
      "Your path in xml is <project>/<profiles>/<profile>/<build>/<plugins>/<plugin>",
      classOf[PomChecker.ValidationException], () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_maven_dependecy_plugin_invalid_output_position(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-any", Seq("tree"), "validate", Map("outputFile" -> "target/dependency-tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("Please check your pom.xml's. The maven-dependency-plugin execution with id " +
      "pre-build-validate-any has no configuration-element or the outputFile-tag contains slashes",
      classOf[PomChecker.ValidationException], () => PomChecker.checkPlugins(deps))
  }

  @Test
  def testCheck_maven_dependecy_plugin_invalid_phase(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "test", Map("outputFile" -> "target/dependency-tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "compile", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("maven-dependency-plugin goals tree, list must be executed on phase \"validate\"",
      classOf[PomChecker.ValidationException], () => PomChecker.checkPlugins(deps))
  }

  @Test
  def testCheck_maven_dependecy_plugin_empty_config(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("Please check your pom.xml's. The maven-dependency-plugin execution with id " +
      "pre-build-validate-tree has no configuration-element or the outputFile-tag contains slashes",
      classOf[PomChecker.ValidationException], () => PomChecker.checkPlugins(deps))
  }

  @Test
  def testCheck_maven_dependecy_plugin_invalid_exec(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8", Nil,
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add at least one execution to you maven-dependency-plugin",
      classOf[PomChecker.ValidationException], () => PomChecker.checkPlugins(deps))
  }

  @Test
  def testCheck_maven_dependecy_plugin_valid(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map("outputFile" -> "dep.tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN
    PomChecker.checkPlugins(deps)
    // THEN
    // - no exception
  }

  @Test
  def testCheck_maven_dependecy_plugin_pomPath(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map("outputFile" -> "dep.tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "profile", "profiles", "project")))

    // WHEN / THEN
    TestHelper.assertException("please check your pom.xml's and move your maven-dependency-plugin to <project>/<build>/<plugins>/<plugin>. " +
      "Your path in xml is <project>/<profiles>/<profile>/<build>/<plugins>/<plugin>",
      classOf[PomChecker.ValidationException], () => PomChecker.checkPlugins(deps))

  }

  @Test
  def testCheckGavFormat_empty(): Unit = {
    val result = TermTest.withOutErr[Unit]()(sys => {
      PomChecker.checkGavFormat(Nil, sys.out)
    })
    Assert.assertEquals("", result.out)
  }

  @Test
  def testCheckGav(): Unit = {
    Assert.assertFalse(Gav.isUnu(""))
    Assert.assertFalse(Gav.isUnu("a"))
    Assert.assertFalse(Gav.isUnu("a_b"))
    Assert.assertFalse(Gav.isUnu("a-b"))
    Assert.assertTrue(Gav.isUnu(" "))
    Assert.assertTrue(Gav.isUnu("a "))
    Assert.assertTrue(Gav.isUnu("a ;"))
    Assert.assertTrue(Gav.isUnu(" a"))
    Assert.assertTrue(Gav.isUnu(". a"))
    Assert.assertTrue(Gav.isUnu("a;"))
    Assert.assertTrue(Gav.isUnu("a-"))
    Assert.assertTrue(Gav.isUnu("a_"))
  }

  @Test
  def testCheckGavFormat(): Unit = {
    val result = TermTest.withOutErr[Unit]()(sys => {
      val base = ProjectMod.Dep(SelfRef.undef, "g", "a", "v", "t", "s", "jar", "classi")
      PomChecker.checkGavFormat(Seq(
        base,
        base.copy(groupId = "g."),
        base.copy(groupId = " g"),
        base.copy(artifactId = "/a"),
        base.copy(version = "v;"),
        base.copy(typeN = "t "),
        base.copy(scope = " s"),
        base.copy(scope = "s "),
        base.copy(packaging = "~p"),
        base.copy(packaging = ""),
        base.copy(scope = ""),
      ), sys.out)
    })
    Assert.assertEquals(
      """Warning: Found dependencies with unusual symbols - please check your dependencies
        |  " g:a:v:jar:classi:s"
        |  "g.:a:v:jar:classi:s"
        |  "g:/a:v:jar:classi:s"
        |  "g:a:v:jar:classi: s"
        |  "g:a:v:jar:classi:s "
        |  "g:a:v:~p:classi:s"
        |  "g:a:v;:jar:classi:s"""".stripMargin, result.out)
  }
}
