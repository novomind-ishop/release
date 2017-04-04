package release

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import release.PomMod.{PluginDep, PluginExec, PomRef}

class PomCheckerTest extends AssertionsForJUnit {

  @Test
  def testCheck_ishop_maven_changes_before(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.checkIshopMaven(deps),
      "please add \"check-for-changes-before\" to your ishop maven plugin", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_ishop_maven_changes_package(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.checkIshopMaven(deps),
      "please add \"check-for-changes-package\" to your ishop maven plugin", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_ishop_maven(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN
    PomChecker.checkIshopMaven(deps)

    // THEN
    // no exception
  }

  @Test
  def testCheck_ishop_maven_managed(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(PomRef("com.novomind.ishop.shops:novosales-any:27.0.0-SNAPSHOT:jar"),
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
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("c.n.i.s:n:0.0.1-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(PomRef("c.n.i.s:n-any:0.0.1-SNAPSHOT:jar"),
        "com.novomind.ishop.maven", "ishop-maven-plugin", "", Nil,
        Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.checkIshopMaven(deps),
      "c.n.i.s:n-any:0.0.1-SNAPSHOT:jar - a single execution section for ishop maven plugin please required, but was Nil.",
      classOf[IllegalArgumentException])
  }

  @Test
  def testCheck_ishop_maven_pom_path(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "profile", "profiles", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.checkIshopMaven(deps),
      "please check your pom.xml's and move your ishop-maven-plugin to plugin/plugins/build/project " +
        "your path is plugin/plugins/build/profile/profiles/project", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_maven_dependecy_plugin_invalid_output_position(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-any", Seq("tree"), "validate", Map("outputFile" -> "target/dependency-tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "maven-dependecy-plugin pre-build-validate-any has no config or outputFile contains slashes", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_maven_dependecy_plugin_invalid_phase(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "test", Map("outputFile" -> "target/dependency-tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "compile", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "maven-dependecy-plugin goals tree, list must be executed on phase \"validate\"", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_maven_dependecy_plugin_empty_config(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "maven-dependecy-plugin pre-build-validate-tree has no config or outputFile contains slashes", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_maven_dependecy_plugin_invalid_exec(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8", Nil,
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "please add at least one execution to you maven-dependecy-plugin",
      classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_maven_dependecy_plugin_valid(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map("outputFile" -> "dep.tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN
    PomChecker.check(deps)
    // THEN
    // - no exception
  }

  @Test
  def testCheck_maven_dependecy_plugin_pomPath(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map("outputFile" -> "dep.tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "profile", "profiles", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "please check your pom.xml's and move your maven-dependency-plugin to plugin/plugins/build/project " +
        "your path is plugin/plugins/build/profile/profiles/project", classOf[PomChecker.ValidationException])

  }
}
