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
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "please add \"check-for-changes-before\" to your ishop maven plugin", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_ishop_maven_changes_package(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "please add \"check-for-changes-package\" to your ishop maven plugin", classOf[PomChecker.ValidationException])
  }

  @Test
  def testCheck_ishop_maven(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN
    PomChecker.check(deps)

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
    PomChecker.check(deps)

    // THEN
    // no exception
  }

  @Test
  def testCheck_ishop_maven_multiple_no_exec(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(PomRef("com.novomind.ishop.shops:novosales-any:27.0.0-SNAPSHOT:jar"),
        "com.novomind.ishop.maven", "ishop-maven-plugin", "", Nil,
        Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "A single execution section for ishop maven plugin please required, but was Nil.", classOf[IllegalArgumentException])
  }

  @Test
  def testCheck_ishop_maven_pom_path(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "profile", "profiles", "project")))

    // WHEN / THEN
    TestHelper.assertException(() ⇒ PomChecker.check(deps),
      "please move your ishop-maven-plugin to plugin/plugins/build/project " +
        "your path is plugin/plugins/build/profile/profiles/project", classOf[PomChecker.ValidationException])
  }
}
