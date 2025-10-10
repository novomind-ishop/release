package release

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Ignore, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.PomChecker.ValidationException
import release.PomModTest.{document, pomTestFile, pomfile}
import release.ProjectMod.{Dep, Gav, Gav2, Gav3, PluginDep, PluginExec, SelfRef}

import java.io.File
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.nowarn

class PomCheckerTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  @Test
  def testGetOwnArtifactNames_empty(): Unit = {
    val root = temp.newFolder()
    val rel = Seq()
    val result = PomChecker.getOwnArtifactNames(rel, root)

    Assert.assertEquals((Seq(
    ), None), result)
  }

  @Test
  def testGetOwnArtifactNames_one(): Unit = {
    val root = temp.newFolder()
    val dep = Dep(SelfRef.ofGav3(Gav3(groupId = "maven", artifactId = "parent", version = Some("v"))), "g", "a", Some("v"), "t", "s", "p", "c", Nil)
    val rel = Seq((dep, new File(root, "any")))
    val result = PomChecker.getOwnArtifactNames(rel, root)

    Assert.assertEquals((Seq(
      Gav2(groupId = "maven", artifactId = "parent")
    ), None), result)
  }

  @Test
  def testGetOwnArtifactNames_simple(): Unit = {
    val root = temp.newFolder()
    val dep = Dep(SelfRef.ofGav3(Gav3(groupId = "maven", artifactId = "parent", version = Some("v"))), "g", "a", Some("v"), "t", "s", "p", "c", Nil)
    val gav1 = Gav3(groupId = "any.path.to", artifactId = "novomind", version = Some("v"))
    val gav2 = Gav3(groupId = "any.path.to", artifactId = "novonind", version = Some("v"))
    val gav3 = Gav3(groupId = "any.path.to", artifactId = "jolo", version = Some("v"))

    val gav4 = Gav3(groupId = "any.path.to", artifactId = "bre", version = Some("v"))
    val gav5 = Gav3(groupId = "any.path.to", artifactId = "core", version = Some("v"))
    val gav6 = Gav3(groupId = "any.path.to", artifactId = "ui", version = Some("v"))
    val gav7 = Gav3(groupId = "any.path.to", artifactId = "vue", version = Some("v"))
    val gav8 = Gav3(groupId = "any.path.to", artifactId = "api", version = Some("v"))
    val gav9 = Gav3(groupId = "any.path.to", artifactId = "web", version = Some("v"))
    val gav10 = Gav3(groupId = "any.path.to", artifactId = "app", version = Some("v"))
    val gav11 = Gav3(groupId = "any.path.to", artifactId = "sba", version = Some("v"))

    val gavs = Seq(gav1, gav2, gav3, gav4, gav5, gav6, gav7, gav8, gav9, gav10, gav11)
    val counter = new AtomicInteger()

    val rel = gavs.map(g => (dep.copy(pomRef = SelfRef.ofGav3(g)), new File(root, s"any${counter.getAndIncrement()}")))
    val result = PomChecker.getOwnArtifactNames(rel, root)

    Assert.assertEquals((
      gavs.map(_.toGav2(),
      ), Some("»any.path.to:novomind« (in any0) is too similar to »any.path.to:novonind« (in any1). Please choose distinguishable names.")), result)
  }

  @Test
  def testCheckSnapshots(): Unit = {
    val file = temp.newFile("some.md")
    FileUtils.write(file, Seq(
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
      (1, "42-SNAPSHOT", file.toPath),
      (2, "some 42.0.0-SNAPSHOT line", file.toPath),
      (4, "some 42x-SNAPSHOT line", file.toPath),
      (9, "release gav will be com.novomind.any:artifact:42x-SNAPSHOT", file.toPath),
      (10, "/42x-SNAPSHOT", file.toPath),
      (11, "A /42x-SNAPSHOT", file.toPath),
      (12, "A .42x-SNAPSHOT", file.toPath),
    ), foundFiles)
  }

  @Test
  def testCheckDepVersions_noException(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "valid", version = Some("1.0.0"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "valid", version = Some("1.0.0"), scope = "compile"),
    )

    // WHEN / THEN
    PomChecker.checkDepVersions(deps, deps) // no exception
  }

  @Test
  def testCheckDepVersions_noException_var(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val deps: Seq[Dep] = Seq(
      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "valid", version = Some("${v}"), scope = "test"),
    )
    val props = Map("v" -> "1.2.0")
    // WHEN / THEN
    PomChecker.checkDepVersions(deps, PomMod.replacedVersionProperties(props, skipPropertyReplacement = false)(deps)) // no exception
  }

  @Test
  def testCheckDepVersions(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"), scope = "compile"),
      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"), scope = "compile"),

      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "other", version = Some("1.0.0"), scope = "runtime"),

      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "wrong", version = Some("1.0.0"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "wrong", version = Some("2.0.0"), scope = "test"),

      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "different-ref", version = Some("1.0.0"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "different-ref", version = Some("1.2.0"), scope = "test"),

      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "wrong2", version = Some("1.0.0"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "wrong2", version = Some("1.2.0"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "wrong2", version = Some("1.2.0"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "wrong2", version = Some("${wrongV}"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "wrong2", version = Some("${wrongV2}"), scope = "test"),
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "wrong2", version = None, scope = "test"),
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "wrong2", version = Some(""), scope = "test"),

      ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some-parent", version = Some("1.0.0"),
        pomPath = Seq("project", "parent")),
      ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some-parent", version = Some("2.0.0"),
        pomPath = Seq("project", "parent")),
    )

    val props = Map("wrongV" -> "1.2.0", "wrongV2" -> "1.2.1")
    // WHEN / THEN
    TestHelper.assertException(
      """found overlapping versions in
        |virtual-group-of-all-parents
        |  any.group:some-parent:1.0.0
        |  any.group:some-parent:2.0.0
        |
        |found overlapping versions in
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
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      ProjectModTest.depOf(pomRef = ref2, "any.group", "valid", Some("1.0.0"), "", "test", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "valid", Some("1.0.0"), "", "compile", "", ""),
    )

    // WHEN / THEN
    PomChecker.checkDepScopes(deps, Nil) // no exception
  }

  @Test
  def testCheckDepScopesCopy(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val deps: Seq[Dep] = Seq(
      ProjectModTest.depOf(pomRef = ref1, "any.group", "copy", Some("1.0.0"), "", "compile", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "copy", Some("1.0.0"), "", "compile", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "copy", Some("1.0.0"), "", "", "", ""),
    )

    // WHEN / THEN
    TestHelper.assertException(
      """found copies, use only one dependency in com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war
        |  any.group:copy:1.0.0:compile (times 3). This can also happen if you override an unused dependencyManagement.
        |
        |""".stripMargin.trim,
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkDepScopes(deps, Nil))
  }

  @Test
  def testCheckDepScopes(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val deps: Seq[Dep] = Seq(
      ProjectModTest.depOf(pomRef = ref1, "any.group", "some", Some("1.0.0"), "", "compile", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "some", Some("1.0.0"), "", "runtime", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "other", Some("1.0.0"), "", "runtime", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "wrong", Some("1.0.0"), "", "test", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "wrong", Some("1.0.0"), "", "compile", "", ""),
      ProjectModTest.depOf(pomRef = ref2, "any.group", "valid", Some("1.0.0"), "", "test", "", ""),
      ProjectModTest.depOf(pomRef = ref1, "any.group", "valid", Some("1.0.0"), "", "compile", "", ""),
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
      () => PomChecker.checkDepScopes(deps, Nil))
  }

  @Test
  def testCheckExternalWithProjectScope_noException(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val a = ProjectModTest.depOf(pomRef = ref1, "com.novomind.ishop.shops", "valid", Some("1.0.0"), "", "test", "", "")
    val b = ProjectModTest.depOf(pomRef = ref1, "any.group", "valid", Some("1.0.0"), "", "test", "", "")

    // WHEN / THEN
    PomChecker.checkExternalWithProjectScope(Seq(a, b), Seq(a), Map("project.version" -> "27.0.0-SNAPSHOT")) // no exception
  }

  @Test
  def testCheckExternalWithProjectScope_selfvar(): Unit = {
    // GIVEN
    @nowarn("msg=possible missing interpolator")
    val someVersion = Some("${project.version}")
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val a = ProjectModTest.depOf(pomRef = ref1, "com.novomind.ishop.shops", "valid", someVersion, "", "test", "", "")
    val b = ProjectModTest.depOf(pomRef = ref1, "any.group", "valid", Some("1.0.0"), "", "test", "", "")

    // WHEN / THEN
    PomChecker.checkExternalWithProjectScope(Seq(a, b), Seq(a), Map("project.version" -> "27.0.0-SNAPSHOT")) // no exception
  }

  @Test
  def testCheckExternalWithProjectScope_selfvar1(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    @nowarn("msg=possible missing interpolator")
    val someVersion = Some("${project.version}")
    val a = ProjectModTest.depOf(pomRef = ref1, "com.novomind.ishop.shops", "valid", someVersion, "", "", "", "")
    val a1 = ProjectModTest.depOf(pomRef = ref1, "com.novomind.ishop.shops", "valid", Some("27.0.0-SNAPSHOT"), "", "", "", "")
    val b = ProjectModTest.depOf(pomRef = ref1, "any.group", "valid", Some("1.0.0"), "", "test", "", "")

    // WHEN / THEN
    PomChecker.checkExternalWithProjectScope(Seq(a, b), Seq(a1), Map("project.version" -> "27.0.0-SNAPSHOT")) // no exception
  }

  @Test
  def testCheckExternalWithProjectScope_exception(): Unit = {
    // GIVEN
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    @nowarn("msg=possible missing interpolator")
    val someVersion = Some("${project.version}")
    val a = ProjectModTest.depOf(pomRef = ref1, "com.novomind.ishop.shops", "valid", someVersion, "", "test", "", "")
    val b = ProjectModTest.depOf(pomRef = ref1, "any.group", "valid", someVersion, "", "test", "", "")

    // WHEN / THEN
    @nowarn("msg=possible missing interpolator")
    val msg = "Project variables are not allowed in external dependencies: any.group:valid:${project.version}:test"
    TestHelper.assertException(
      msg,
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

    val srcPoms: File = pomTestFile(temp, root).sub("a", child).create()
    val msg = "unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
      "  (p -> 1), (valid -> 1)\n" +
      "      -> very.long.groupid.any:a-parent:1.0.0-SNAPSHOT\n" +
      "      -> any:a:1.0.0-SNAPSHOT"
    TestHelper.assertException(msg,
      classOf[ValidationException], () => {
        PomModTest.withRepoForTests(srcPoms, Opts().newRepo)
      })
    var failures: Seq[Exception] = Nil
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
    val deps: Seq[PluginDep] = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
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
    val deps: Seq[PluginDep] = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""), Seq(PluginExec("", Seq("check-for-changes-before"), "", Map())),
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add \"check-for-changes-package\" to your ishop maven plugin",
      classOf[PomChecker.ValidationException],
      () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
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
    val deps: Seq[PluginDep] = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""),
      Seq(PluginExec("", Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop-any:27.0.0-SNAPSHOT:jar"),
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
    val deps: Seq[PluginDep] = Seq(PluginDep(ProjectModTest.parseSelfRef("c.n.i.s:n:0.0.1-SNAPSHOT:war"),
      "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""), Seq(PluginExec("",
        Seq("check-for-changes-before", "check-for-changes-package"), "", Map())),
      Seq("plugin", "plugins", "build", "project")),

      PluginDep(ProjectModTest.parseSelfRef("c.n.i.s:n-any:0.0.1-SNAPSHOT:jar"),
        "com.novomind.ishop.maven", "ishop-maven-plugin", Some(""), Nil,
        Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("c.n.i.s:n-any:0.0.1-SNAPSHOT:jar - a single execution section for ishop maven plugin " +
      "please required. Input is Nil.", classOf[IllegalArgumentException], () => PomChecker.checkIshopMaven(deps))
  }

  @Test
  def testCheck_ishop_maven_pom_path(): Unit = {
    // GIVEN
    val deps: Seq[PluginDep] = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
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
    val deps = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
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
    val deps = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
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
    val deps = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
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
    val deps = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"), Nil,
      Seq("plugin", "plugins", "build", "project")))

    // WHEN / THEN
    TestHelper.assertException("please add at least one execution to you maven-dependency-plugin",
      classOf[PomChecker.ValidationException], () => PomChecker.checkPlugins(deps))
  }

  @Test
  def testCheck_maven_dependecy_plugin_valid(): Unit = {

    // GIVEN
    val deps = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
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
    val deps = Seq(PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
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

    def assertGavTrue(expected: String, g: String): Unit = {
      Assert.assertTrue(Gav.isUnusualElementValue(g))
      Assert.assertEquals(expected, Gav.replaceUnusualElements(g))
    }

    def assertGavFalse(expected: String, g: String): Unit = {
      Assert.assertFalse(Gav.isUnusualElementValue(g))
      Assert.assertEquals(expected, Gav.replaceUnusualElements(g))
    }

    assertGavTrue("a␣a", "a a")
    assertGavTrue("a␣a", "a\u0020a")
    assertGavTrue("a␣a␣", "a\u0020a\u200b")
    assertGavFalse("", "")
    Assert.assertFalse(Gav.isUnusualElementValue("a"))
    Assert.assertFalse(Gav.isUnusualElementValue("a_b"))
    Assert.assertFalse(Gav.isUnusualElementValue("a-b"))
    Assert.assertFalse(Gav.isUnusualElementValue("a-8b"))
    Assert.assertFalse(Gav.isUnusualElementValue("1_8"))
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
  def testContainsRange(): Unit = {
    Assert.assertFalse(Gav.isContainsRange("a"))
    Assert.assertFalse(Gav.isContainsRange("1.2.3"))
    Assert.assertTrue(Gav.isContainsRange("(1,2)"))
    Assert.assertTrue(Gav.isContainsRange("[1,2]"))
    Assert.assertTrue(Gav.isContainsRange("(1,2]"))

  }

  @Test
  def testCheckGavFormat(): Unit = {
    val result = TermTest.withOutErr[Unit]()(sys => {
      val base = ProjectModTest.depOfUndef(groupId = "g", artifactId = "a", version = Some("v"), typeN = "t", scope = "runtime",
        packaging = "jar", classifier = "classi")
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

  @Test
  def testCheckOwnArtifacts_noException(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some2", version = Some("1.0.0"))

    PomChecker.checkOwnArtifactNames(Seq(
      (dep1, new File("file/a/pom.xml")),
      (dep2, new File("file/b/pom.xml")),
    ), new File("file/"))

    // no exception
  }

  @Test
  def testCheckOwnArtifacts_2(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:otil:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some2", version = Some("1.0.0"))

    TestHelper.assertException("" +
      "»com.novomind.ishop.shops:otil« (in a/pom.xml) is too similar to " +
      "»com.novomind.ishop.shops:util« (in b/pom.xml). Please choose distinguishable names.",
      classOf[ValidationException], () => {
        PomChecker.checkOwnArtifactNames(Seq(
          (dep1, new File("file/a/pom.xml")),
          (dep2, new File("file/b/pom.xml")),
        ), new File("file/"))
      })
  }

  @Test
  def testCheckOwnArtifacts_3(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-core:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-bom:27.0.0-SNAPSHOT:jar")
    val ref3 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-api:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some2", version = Some("1.0.0"))
    val dep3 = ProjectModTest.depOf(pomRef = ref3, groupId = "any.group", artifactId = "some3", version = Some("1.0.0"))

    PomChecker.checkOwnArtifactNames(Seq(
      (dep1, new File("file/a/pom.xml")),
      (dep2, new File("file/b/pom.xml")),
      (dep3, new File("file/c/pom.xml")),
    ), new File("file/"))
  }

  @Test
  def testCheckOwnArtifacts_4(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-licuide:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-liquid:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some2", version = Some("1.0.0"))

    TestHelper.assertException("" +
      "»com.novomind.ishop.shops:ishop-licuide« (in a/pom.xml) is too similar to " +
      "»com.novomind.ishop.shops:ishop-liquid« (in b/pom.xml). Please choose distinguishable names.",
      classOf[ValidationException], () => {
        PomChecker.checkOwnArtifactNames(Seq(
          (dep1, new File("file/a/pom.xml")),
          (dep2, new File("file/b/pom.xml")),
        ), new File("file/"))
      })
  }

  @Test
  def testCheckOwnArtifacts_5(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-bar-core:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-core:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some2", version = Some("1.0.0"))

    PomChecker.checkOwnArtifactNames(Seq(
      (dep1, new File("file/a/pom.xml")),
      (dep2, new File("file/b/pom.xml")),
    ), new File("file/"))
  }

  @Test
  def testCheckOwnArtifacts_6(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:bre:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:core:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some2", version = Some("1.0.0"))

    PomChecker.checkOwnArtifactNames(Seq(
      (dep1, new File("file/a/pom.xml")),
      (dep2, new File("file/b/pom.xml")),
    ), new File("file/"))
  }

  @Test
  @Ignore
  def testCheckOwnArtifacts_7(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-shop-main:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:main:27.0.0-SNAPSHOT:jar")
    val ref3 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-shop-true:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some1", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some2", version = Some("1.0.0"))
    val dep3 = ProjectModTest.depOf(pomRef = ref3, groupId = "any.group", artifactId = "some3", version = Some("1.0.0"))

    TestHelper.assertException("" +
      "»com.novomind.ishop.shops:ishop-licuide« (in a/pom.xml) is too similar to " +
      "»com.novomind.ishop.shops:ishop-liquid« (in b/pom.xml). Please choose distinguishable names.",
      classOf[ValidationException], () => {
        PomChecker.checkOwnArtifactNames(Seq(
          (dep1, new File("file/a/pom.xml")),
          (dep2, new File("file/b/pom.xml")),
          (dep3, new File("file/c/pom.xml"))
        ), new File("file/"))
      })
  }

  @Test
  def testCheckOwnArtifacts(): Unit = {
    val ref1 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war")
    val ref2 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")
    val ref3 = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:util:27.0.0-SNAPSHOT:jar")

    val dep1 = ProjectModTest.depOf(pomRef = ref1, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep2 = ProjectModTest.depOf(pomRef = ref2, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    val dep3 = ProjectModTest.depOf(pomRef = ref3, groupId = "any.group", artifactId = "some", version = Some("1.0.0"))
    TestHelper.assertException("" +
      "»com.novomind.ishop.shops:util« (in b/pom.xml) is identical to " +
      "»com.novomind.ishop.shops:util« (in c/pom.xml). Please choose distinguishable names.", classOf[ValidationException], () => {
      PomChecker.checkOwnArtifactNames(Seq(
        (dep1, new File("file/a/pom.xml")),
        (dep2, new File("file/b/pom.xml")),
        (dep3, new File("file/c/pom.xml")),
      ), new File("file/"))
    })
  }

  def g2(gav2: Gav2) = {
    (gav2, new File("file/a/pom.xml"))
  }

  @Test
  def testCommonStripped_1(): Unit = {
    val result = PomChecker.commonStriped(Seq(
      g2(Gav2(groupId = "a", artifactId = "b")),
    ))
    Assert.assertEquals(
      Seq(
        g2(Gav2(groupId = "a", artifactId = "b")),
      ), result)
  }

  @Test
  def testCommonStripped_2(): Unit = {
    val result = PomChecker.commonStriped(Seq(
      g2(Gav2(groupId = "a.b", artifactId = "b-a")),
      g2(Gav2(groupId = "a.b", artifactId = "b-b")),
    ))
    Assert.assertEquals(
      Seq(
        g2(Gav2(groupId = "", artifactId = "a")),
        g2(Gav2(groupId = "", artifactId = "b")),
      ), result)
  }

  @Test
  def testCommonStripped_3(): Unit = {
    val result = PomChecker.commonStriped(Seq(
      g2(Gav2(groupId = "a.b", artifactId = "b-a")),
      g2(Gav2(groupId = "a.b", artifactId = "b-b")),
      g2(Gav2(groupId = "a.c", artifactId = "o-oj")),
    ))
    Assert.assertEquals(
      Seq(
        g2(Gav2(groupId = "", artifactId = "a")),
        g2(Gav2(groupId = "", artifactId = "b")),
        g2(Gav2(groupId = "c", artifactId = "o-oj")),
      ), result)
  }
}
