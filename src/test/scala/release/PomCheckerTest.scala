package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.PomChecker.ValidationException
import release.PomModTest.{document, pomfile}
import release.ProjectMod.{Dep, Gav, PluginDep, PluginExec, SelfRef}
import release.Starter.Opts

class PomCheckerTest extends AssertionsForJUnit {

  @Test
  def testCheckDepScopes_noException(): Unit = {
    // GIVEN
    val ref1 = SelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = SelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      Dep(ref2, "any.group", "valid", "1.0.0", "", "test", "", ""),
      Dep(ref1, "any.group", "valid", "1.0.0", "", "compile", "", ""),
    )

    // WHEN / THEN
    PomChecker.checkDepScopes(deps) // no exception
  }

  @Test
  def testCheckDepScopes(): Unit = {
    // GIVEN
    val ref1 = SelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = SelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      Dep(ref1, "any.group", "some", "1.0.0", "", "compile", "", ""),
      Dep(ref1, "any.group", "some", "1.0.0", "", "runtime", "", ""),

      Dep(ref1, "any.group", "other", "1.0.0", "", "runtime", "", ""),

      Dep(ref1, "any.group", "wrong", "1.0.0", "", "test", "", ""),
      Dep(ref1, "any.group", "wrong", "1.0.0", "", "compile", "", ""),

      Dep(ref2, "any.group", "valid", "1.0.0", "", "test", "", ""),
      Dep(ref1, "any.group", "valid", "1.0.0", "", "compile", "", ""),
    )

    // WHEN / THEN
    TestHelper.assertException(
      """found overlapping scopes
        |any.group:some:1.0.0
        | related to
        |Dep(SelfRef(com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war),any.group,some,1.0.0,,compile,,)
        |Dep(SelfRef(com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war),any.group,some,1.0.0,,runtime,,)
        |
        |any.group:wrong:1.0.0
        | related to
        |Dep(SelfRef(com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war),any.group,wrong,1.0.0,,test,,)
        |Dep(SelfRef(com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war),any.group,wrong,1.0.0,,compile,,)
        |""".stripMargin.trim,
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkDepScopes(deps))
  }

  @Test
  def testCheckRootFirstChildPropertiesVar_noChilds(): Unit = {

    val root = PomModTest.document(<project>
      <properties>
        <a>b</a>
      </properties>
    </project>)

    Assert.assertEquals(Map("a" -> "b"), PomMod.createPropertyMap(root))
    PomChecker.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root)))
  }

  @Test
  def testCheckRootFirstChildPropertiesVar(): Unit = {

    val root = document(<project>
      <groupId>very.long.groupid.any</groupId>
      <artifactId>a-parent</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>a</module>
      </modules>
      <properties>
        <p>1</p>
        <valid>1</valid>
      </properties>
    </project>)

    val child = document(<project>
      <parent>
        <groupId>very.long.groupid.any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>any</groupId>
      <artifactId>a</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>1</p>
        <valid>1</valid>
      </properties>
    </project>)

    Assert.assertEquals(Map("p" -> "1", "valid" -> "1"), PomMod.createPropertyMap(root))
    Assert.assertEquals(Map("p" -> "1", "valid" -> "1"), PomMod.createPropertyMap(child))

    TestHelper.assertException("unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
      "  (p -> 1)\n" +
      "      -> very.long.groupid.any:a-parent:1.0.0-SNAPSHOT\n" +
      "      -> any:a:1.0.0-SNAPSHOT",
      classOf[ValidationException], () => {
        PomChecker.checkRootFirstChildPropertiesVar(Opts().copy(skipProperties = Seq("valid")), Seq(pomfile(root), pomfile(child)))
      })
  }

  @Test
  def testCheckRootFirstChildOnePropertiesParentRoot(): Unit = {

    val root = document(<project>
      <groupId>any</groupId>
      <artifactId>a-parent</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>
      <modules>
        <module>a</module>
        <module>b</module>
      </modules>
    </project>)

    val child0 = document(<project>
      <parent>
        <groupId>any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>any</groupId>
      <artifactId>a</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    val child1 = document(<project>
      <parent>
        <groupId>any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>any</groupId>
      <artifactId>b</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    Assert.assertEquals(Map.empty, PomMod.createPropertyMap(root))
    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(child0))
    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(child1))

    TestHelper.assertException("unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
      "  (p -> 1)\n" +
      "      -> any:a-parent:1.0.0-SNAPSHOT\n" +
      "      -> any:a:1.0.0-SNAPSHOT\n" +
      "      -> any:b:1.0.0-SNAPSHOT",
      classOf[ValidationException], () => {
        PomChecker.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))
      })
  }

  @Test
  def testCheckRootFirstChildPropertiesVar_different_values(): Unit = {

    val root = document(<project>
      <groupId>very.long.groupid.any</groupId>
      <artifactId>a-parent</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>a</module>
      </modules>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    val child = document(<project>
      <parent>
        <groupId>very.long.groupid.any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>any</groupId>
      <artifactId>a</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>2</p>
      </properties>
    </project>)

    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(root))
    Assert.assertEquals(Map("p" -> "2"), PomMod.createPropertyMap(child))
    PomChecker.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child)))
  }

  @Test
  def testCheckRootFirstChildNoParentProperties(): Unit = {

    val root = document(<project>
      <groupId>any</groupId>
      <artifactId>a-parent</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>a</module>
      </modules>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    val child = document(<project>
      <groupId>any</groupId>
      <artifactId>a</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(root))
    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(child))

    PomChecker.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child)))

  }

  @Test
  def testCheckRootFirstChildOneProperties(): Unit = {

    val root = document(<project>
      <groupId>any</groupId>
      <artifactId>a-parent</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>a</module>
        <module>b</module>
      </modules>
    </project>)

    val child0 = document(<project>
      <groupId>any</groupId>
      <artifactId>a</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    val child1 = document(<project>
      <groupId>any</groupId>
      <artifactId>b</artifactId>
      <version>1.0.0-SNAPSHOT</version>
    </project>)

    Assert.assertEquals(Map.empty, PomMod.createPropertyMap(root))
    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(child0))
    Assert.assertEquals(Map.empty, PomMod.createPropertyMap(child1))

    PomChecker.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))

  }

  @Test
  def testCheckRootFirstChildOnePropertiesParent(): Unit = {

    val root = document(<project>
      <groupId>any</groupId>
      <artifactId>a-parent</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>a</module>
        <module>b</module>
      </modules>
    </project>)

    val child0 = document(<project>
      <parent>
        <groupId>any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>any</groupId>
      <artifactId>a</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    val child1 = document(<project>
      <parent>
        <groupId>any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>any</groupId>
      <artifactId>b</artifactId>
      <version>1.0.0-SNAPSHOT</version>
    </project>)

    Assert.assertEquals(Map.empty, PomMod.createPropertyMap(root))
    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(child0))
    Assert.assertEquals(Map.empty, PomMod.createPropertyMap(child1))

    PomChecker.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))

  }

  @Test
  def testCheckRootFirstChildOnePropertiesParentTwo(): Unit = {

    val root = document(<project>
      <groupId>very.long.groupid.any</groupId>
      <artifactId>a-parent</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>
      <properties>
        <p>1</p>
      </properties>
      <modules>
        <module>a</module>
        <module>b</module>
      </modules>
    </project>)

    val child0 = document(<project>
      <parent>
        <groupId>very.long.groupid.any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>any</groupId>
      <artifactId>a</artifactId>
      <version>1.0.0-SNAPSHOT</version>
      <properties>
        <p>1</p>
      </properties>
    </project>)

    val child1 = document(<project>
      <parent>
        <groupId>very.long.groupid.any</groupId>
        <artifactId>a-parent</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <groupId>very.long.groupid.any</groupId>
      <artifactId>b</artifactId>
      <version>1.0.0-SNAPSHOT</version>
    </project>)

    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(root))
    Assert.assertEquals(Map("p" -> "1"), PomMod.createPropertyMap(child0))
    Assert.assertEquals(Map.empty, PomMod.createPropertyMap(child1))

    TestHelper.assertException("unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
      "  (p -> 1)\n" +
      "      -> very.long.groupid.any:a-parent:1.0.0-SNAPSHOT\n" +
      "      -> any:a:1.0.0-SNAPSHOT\n" +
      "      -> very.long.groupid.any:b:1.0.0-SNAPSHOT",
      classOf[ValidationException], () => {
        PomChecker.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))
      })
  }

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
