package release

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.PomChecker.ValidationException
import release.PomModTest.{document, pomTestFile, pomfile}
import release.ProjectMod.{Dep, Gav, PluginDep, PluginExec, SelfRef}
import release.Starter.Opts

import java.io.File

class PomCheckerTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  @Test
  def testCheckSnapshots(): Unit = {
    val file = temp.newFile("some.md")
    Util.write(file, Seq(
      "42-SNAPSHOT",
      "some 42.0.0-SNAPSHOT line",
      "some -SNAPSHOT line",
      "some 42x-SNAPSHOT line",
      "some 4?x-SNAPSHOT line",
      "some 4*x-SNAPSHOT line",
      "some 4/x-SNAPSHOT line",
      "some 4\\x-SNAPSHOT line",
      "release gav will be com.novomind.any:artifact:42x-SNAPSHOT",
      "/42x-SNAPSHOT",
      "A /42x-SNAPSHOT",
      "A .42x-SNAPSHOT",

    ))
    val foundFiles = PomChecker.getSnapshotsInFiles(Seq(file).map(_.getAbsolutePath))
    Assert.assertEquals(Seq(
      (0, "42-SNAPSHOT", file.toPath),
      (1, "some 42.0.0-SNAPSHOT line", file.toPath),
      (3, "some 42x-SNAPSHOT line", file.toPath),
      (8, "release gav will be com.novomind.any:artifact:42x-SNAPSHOT", file.toPath),
      (9, "/42x-SNAPSHOT", file.toPath),
      (10, "A /42x-SNAPSHOT", file.toPath),
      (11, "A .42x-SNAPSHOT", file.toPath),
    ), foundFiles)
  }

  @Test
  def testCheckDepVersions_noException(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = SelfRef.parse("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      Dep(ref2, "any.group", "valid", Some("1.0.0"), "", "test", "", ""),
      Dep(ref1, "any.group", "valid", Some("1.0.0"), "", "compile", "", ""),
    )

    // WHEN / THEN
    PomChecker.checkDepVersions(deps, deps) // no exception
  }

  @Test
  def testCheckDepVersions_noException_var(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val deps: Seq[Dep] = Seq(
      Dep(ref1, "any.group", "valid", Some("${v}"), "", "test", "", ""),
    )
    val props = Map("v" -> "1.2.0")
    // WHEN / THEN
    PomChecker.checkDepVersions(deps, PomMod.replacedVersionProperties(props, skipPropertyReplacement = false)(deps)) // no exception
  }

  @Test
  def testCheckDepVersions(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = SelfRef.parse("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      Dep(ref1, "any.group", "some", Some("1.0.0"), "", "compile", "", ""),
      Dep(ref1, "any.group", "some", Some("1.0.0"), "", "compile", "", ""),

      Dep(ref1, "any.group", "other", Some("1.0.0"), "", "runtime", "", ""),

      Dep(ref1, "any.group", "wrong", Some("1.0.0"), "", "test", "", ""),
      Dep(ref1, "any.group", "wrong", Some("2.0.0"), "", "test", "", ""),

      Dep(ref2, "any.group", "different-ref", Some("1.0.0"), "", "test", "", ""),
      Dep(ref1, "any.group", "different-ref", Some("1.2.0"), "", "test", "", ""),

      Dep(ref2, "any.group", "wrong2", Some("1.0.0"), "", "test", "", ""),
      Dep(ref2, "any.group", "wrong2", Some("1.2.0"), "", "test", "", ""),
      Dep(ref2, "any.group", "wrong2", Some("1.2.0"), "", "test", "", ""),
      Dep(ref2, "any.group", "wrong2", Some("${wrongV}"), "", "test", "", ""),
      Dep(ref2, "any.group", "wrong2", Some("${wrongV2}"), "", "test", "", ""),
      Dep(ref2, "any.group", "wrong2", None, "", "test", "", ""),
      Dep(ref2, "any.group", "wrong2", Some(""), "", "test", "", ""),
    )

    val props = Map("wrongV" -> "1.2.0", "wrongV2" -> "1.2.1")
    // WHEN / THEN
    TestHelper.assertException(
      """found overlapping versions in
        |com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war
        |  any.group:wrong:1.0.0:test
        |  any.group:wrong:2.0.0:test
        |
        |found overlapping versions in
        |com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar
        |  any.group:wrong2:${wrongV}:test
        |  any.group:wrong2:1.0.0:test
        |  any.group:wrong2:1.2.0:test
        |  any.group:wrong2:1.2.1:test
        |""".stripMargin.trim,
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkDepVersions(PomMod.replacedVersionProperties(props, skipPropertyReplacement = false)(deps), deps))
  }

  @Test
  def testCheckDepScopes_noException(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = SelfRef.parse("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      Dep(ref2, "any.group", "valid", Some("1.0.0"), "", "test", "", ""),
      Dep(ref1, "any.group", "valid", Some("1.0.0"), "", "compile", "", ""),
    )

    // WHEN / THEN
    PomChecker.checkDepScopes(deps, deps) // no exception
  }

  @Test
  def testCheckDepScopes(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = SelfRef.parse("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      Dep(ref1, "any.group", "some", Some("1.0.0"), "", "compile", "", ""),
      Dep(ref1, "any.group", "some", Some("1.0.0"), "", "runtime", "", ""),
      Dep(ref1, "any.group", "other", Some("1.0.0"), "", "runtime", "", ""),
      Dep(ref1, "any.group", "wrong", Some("1.0.0"), "", "test", "", ""),
      Dep(ref1, "any.group", "wrong", Some("1.0.0"), "", "compile", "", ""),
      Dep(ref2, "any.group", "valid", Some("1.0.0"), "", "test", "", ""),
      Dep(ref1, "any.group", "valid", Some("1.0.0"), "", "compile", "", ""),
    )

    // WHEN / THEN
    TestHelper.assertException(
      """found overlapping scopes
        |any.group:some:1.0.0
        | found in
        |com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war with scope: ...:compile
        |com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war with scope: ...:runtime
        |
        |any.group:wrong:1.0.0
        | found in
        |com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war with scope: ...:test
        |com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war with scope: ...:compile
        |""".stripMargin.trim,
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkDepScopes(deps, deps))
  }

  @Test
  def testCheckExternalWithProjectScope_noException(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val a = Dep(ref1, "com.novomind.ishop.shops", "valid", Some("1.0.0"), "", "test", "", "")
    val b = Dep(ref1, "any.group", "valid", Some("1.0.0"), "", "test", "", "")

    // WHEN / THEN
    PomChecker.checkExternalWithProjectScope(Seq(a, b), Seq(a), Map("project.version" -> "27.0.0-SNAPSHOT")) // no exception
  }

  @Test
  def testCheckExternalWithProjectScope_selfvar(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val a = Dep(ref1, "com.novomind.ishop.shops", "valid", Some("${project.version}"), "", "test", "", "")
    val b = Dep(ref1, "any.group", "valid", Some("1.0.0"), "", "test", "", "")

    // WHEN / THEN
    PomChecker.checkExternalWithProjectScope(Seq(a, b), Seq(a), Map("project.version" -> "27.0.0-SNAPSHOT")) // no exception
  }

  @Test
  def testCheckExternalWithProjectScope_selfvar1(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val a = Dep(ref1, "com.novomind.ishop.shops", "valid", Some("${project.version}"), "", "", "", "")
    val a1 = Dep(ref1, "com.novomind.ishop.shops", "valid", Some("27.0.0-SNAPSHOT"), "", "", "", "")
    val b = Dep(ref1, "any.group", "valid", Some("1.0.0"), "", "test", "", "")

    // WHEN / THEN
    PomChecker.checkExternalWithProjectScope(Seq(a, b), Seq(a1), Map("project.version" -> "27.0.0-SNAPSHOT")) // no exception
  }

  @Test
  def testCheckExternalWithProjectScope_exception(): Unit = {
    // GIVEN
    val ref1 = SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val a = Dep(ref1, "com.novomind.ishop.shops", "valid", Some("${project.version}"), "", "test", "", "")
    val b = Dep(ref1, "any.group", "valid", Some("${project.version}"), "", "test", "", "")

    // WHEN / THEN
    TestHelper.assertException(
      "Project variables are not allowed in external dependencies: any.group:valid:${project.version}:test",
      classOf[ValidationException], () =>
        PomChecker.checkExternalWithProjectScope(Seq(a, b), Seq(a), Map("project.version" -> "27.0.0-SNAPSHOT")))
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

    val srcPoms: File = pomTestFile(temp,root).sub("a", child).create()
    val msg = "unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
      "  (p -> 1), (valid -> 1)\n" +
      "      -> very.long.groupid.any:a-parent:1.0.0-SNAPSHOT\n" +
      "      -> any:a:1.0.0-SNAPSHOT"
    TestHelper.assertException(msg,
      classOf[ValidationException], () => {
        PomModTest.withRepoForTests(srcPoms,Opts().newRepo)
      })
    var failures:Seq[Exception] = Nil
    PomModTest.withRepoForTests(srcPoms, Opts().newRepo, failureCollector = Some(e => failures = failures :+ e))
    Assert.assertEquals(Seq(msg), failures.map(_.getMessage))
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
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""), Seq(PluginExec("", Seq("check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add \"check-for-changes-before\" to your ishop maven plugin",
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven_changes_package(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin",Some(""), Seq(PluginExec("", Seq("check-for-changes-before"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add \"check-for-changes-package\" to your ishop maven plugin",
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""),
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
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""),
      Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(SelfRef.parse("com.novomind.ishop.shops:anyshop-any:27.0.0-SNAPSHOT:jar"),
        "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""), Nil,
        Seq("plugin", "plugins", "pluginManagement", "build", "project")))

    // WHEN
    PomChecker.checkIshopMaven(deps)

    // THEN
    // no exception
  }

  @Test
  def testCheck_ishop_maven_multiple_no_exec(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef.parse("c.n.i.s:n:0.0.1-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""), Seq(PluginExec("",
        Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(SelfRef.parse("c.n.i.s:n-any:0.0.1-SNAPSHOT:jar"),
        "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""), Nil,
        Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("c.n.i.s:n-any:0.0.1-SNAPSHOT:jar - a single execution section for ishop maven plugin " +
      "please required. Input is Nil.", classOf[IllegalArgumentException], () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven_pom_path(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""),
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
    val deps = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"),
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
    val deps = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"),
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
    val deps = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"),
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
    val deps = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"), Nil,
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add at least one execution to you maven-dependency-plugin",
      classOf[PomChecker.ValidationException], () => PomChecker.checkPlugins(deps))
  }

  @Test
  def testCheck_maven_dependecy_plugin_valid(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"),
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
    val deps = Seq(PluginDep(SelfRef.parse("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"),
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
    Assert.assertFalse(Gav.isUnusualElementValue(""))
    Assert.assertFalse(Gav.isUnusualElementValue("a"))
    Assert.assertFalse(Gav.isUnusualElementValue("a_b"))
    Assert.assertFalse(Gav.isUnusualElementValue("a-b"))
    Assert.assertTrue(Gav.isUnusualElementValue(" "))
    Assert.assertTrue(Gav.isUnusualElementValue("a "))
    Assert.assertTrue(Gav.isUnusualElementValue("a ;"))
    Assert.assertTrue(Gav.isUnusualElementValue(" a"))
    Assert.assertTrue(Gav.isUnusualElementValue(". a"))
    Assert.assertTrue(Gav.isUnusualElementValue("a;"))
    Assert.assertTrue(Gav.isUnusualElementValue("a-"))
    Assert.assertTrue(Gav.isUnusualElementValue("a_"))
  }

  @Test
  def testCheckGavFormat(): Unit = {
    val result = TermTest.withOutErr[Unit]()(sys => {
      val base = ProjectMod.Dep(SelfRef.undef, "g", "a", Some("v"), "t", "runtime", "jar", "classi")
      PomChecker.checkGavFormat(Seq(
        base,
        base.copy(groupId = "g."),
        base.copy(groupId = " g"),
        base.copy(artifactId = "/a"),
        base.copy(version = Some("v;")),
        base.copy(version = Some("")),
        base.copy(version = None),
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
        |  " g:a:v:jar:classi:runtime"
        |  "g.:a:v:jar:classi:runtime"
        |  "g:/a:v:jar:classi:runtime"
        |  "g:a:v:jar:classi: s"
        |  "g:a:v:jar:classi:s "
        |  "g:a:v:~p:classi:runtime"
        |  "g:a:v;:jar:classi:runtime"""".stripMargin, result.out)
  }
}