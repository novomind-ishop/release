package release

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Paths
import java.time.{LocalDate, Month}

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, ComparisonFailure, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import org.w3c.dom._
import release.PomChecker.ValidationException
import release.PomModTest.{assertDeps, document, pomfile, _}
import release.ProjectMod._
import release.Starter.Opts

import scala.xml.Elem

class PomModTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  lazy val aether = new Aether(Opts())

  @Test
  def testTeset(): Unit = {
    val srcPoms: File = pomTestFile(temp, document(
      <project>
        <modelVersion>4.0.0</modelVersion>
        <groupId>com.novomind.ishop.shops.any</groupId>
        <artifactId>any-projects</artifactId>
        <version>27.0.0-SNAPSHOT</version>
        <packaging>pom</packaging>
        <modules>
          <module>any-erp</module>
        </modules>
      </project>
    )).sub("any-erp", document(<project>
      <modelVersion>4.0.0</modelVersion>
      <parent>
        <groupId>com.novomind.ishop.shops.any</groupId>
        <artifactId>any-projects</artifactId>
        <version>28.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>
      <artifactId>any-erp</artifactId>
      <name>any-erp</name>
    </project>
    )).create()
    val out = PomMod.allRawModulePomsFiles(Seq(srcPoms)).map(in => srcPoms.toPath.relativize(in.toPath))
    Assert.assertEquals(Seq(Paths.get("pom.xml"), Paths.get("any-erp", "pom.xml")), out)
  }

  @Test
  def testCheckRootFirstChildPropertiesVar_noChilds(): Unit = {

    val root = document(<project>
      <properties>
        <a>b</a>
      </properties>
    </project>)

    Assert.assertEquals(Map("a" -> "b"), PomMod.createPropertyMap(root))
    PomMod.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root)))
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
        PomMod.checkRootFirstChildPropertiesVar(Opts().copy(skipProperties = Seq("valid")), Seq(pomfile(root), pomfile(child)))
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
    PomMod.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child)))
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

    PomMod.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child)))

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

    PomMod.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))

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

    PomMod.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))

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
        PomMod.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))
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
        PomMod.checkRootFirstChildPropertiesVar(Opts(), Seq(pomfile(root), pomfile(child0), pomfile(child1)))
      })
  }

  @Test
  def defectSelfPom(): Unit = {
    val srcPoms: File = pomTestFile(temp, document(<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.novomind.ishop.shops.any</groupId>
      <artifactId>any-projects</artifactId>
      <version>27.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>any-erp</module>
      </modules>
    </project>
    )).sub("any-erp", document(<project>
      <modelVersion>4.0.0</modelVersion>

      <parent>
        <groupId>com.novomind.ishop.shops.any</groupId>
        <artifactId>any-projects</artifactId>
        <version>28.0.0-SNAPSHOT</version>
        <relativePath>..</relativePath>
      </parent>

      <artifactId>any-erp</artifactId>
      <name>any-erp</name>
    </project>
    )).create()

    // WHEN
    TestHelper.assertException("More then one Version found in your pom.xmls: " +
      "Dep(PomRef(any-erp),com.novomind.ishop.shops.any,any-erp,28.0.0-SNAPSHOT,,,,) (27.0.0-SNAPSHOT, 28.0.0-SNAPSHOT)",
      classOf[IllegalArgumentException], () => {
        PomMod.ofAetherForTests(srcPoms, aether).selfVersion
      })

  }

  @Test
  def writeSelf(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")
    val targetPoms = TestHelper.testResources("shop1")

    // WHEN
    PomMod.ofAetherForTests(srcPoms, aether)
      .writeTo(targetPoms)

    // THEN
    assert(Nil === TestHelper.localChanges())
  }

  @Test
  def findSelected(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    val pomMod = PomMod.ofAetherForTests(srcPoms, aether)
    // WHEN
    val nodes: Seq[Node] = pomMod.findNodes("com.novomind.ishop.shops.anyshop", "anyshop-erp", "27.0.0-SNAPSHOT")

    // THEN

    val baseVersion = Map(
      "groupId" -> "com.novomind.ishop.shops.anyshop",
      "artifactId" -> "anyshop-erp",
      "version" -> "${project.version}")
    val noVersion = Map(
      "groupId" -> "com.novomind.ishop.shops.anyshop",
      "artifactId" -> "anyshop-erp")
    val testScope = Map(
      "groupId" -> "com.novomind.ishop.shops.anyshop",
      "artifactId" -> "anyshop-erp",
      "version" -> "27.0.0-SNAPSHOT",
      "scope" -> "test")
    val testsScope = Map(
      "groupId" -> "com.novomind.ishop.shops.anyshop",
      "artifactId" -> "anyshop-erp",
      "version" -> "27.0.0-SNAPSHOT",
      "classifier" -> "tests",
      "scope" -> "test")
    val pomClassifier = Map(
      "groupId" -> "com.novomind.ishop.shops.anyshop",
      "artifactId" -> "anyshop-erp",
      "version" -> "27.0.0-SNAPSHOT",
      "classifier" -> "pom")
    val testsPomScope = Map(
      "groupId" -> "com.novomind.ishop.shops.anyshop",
      "artifactId" -> "anyshop-erp",
      "version" -> "27.0.0-SNAPSHOT",
      "classifier" -> "pom",
      "scope" -> "test")

    Assert.assertEquals(Seq(baseVersion, noVersion, testScope, testsScope, pomClassifier, testsPomScope).sortBy(_.toString()),
      Xpath.mapToSeqMap(nodes).sortBy(_.toString()))
  }

  @Test
  def depTreeFilename(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")
    val pomMod = PomMod.ofAetherForTests(srcPoms, aether)

    // WHEN
    val treeFile: Option[String] = pomMod.depTreeFilename()

    // THEN
    Assert.assertEquals(Seq("dep.tree", "anyshop-erp/dep.tree", "anyshop-shop/dep.tree"), pomMod.depTreeFilenames())
    Assert.assertEquals(Some("dep.tree"), treeFile)
  }

  @Test
  def depTreeFilename_defect(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("defect-dep-tree")
    val pomMod = PomMod.ofAetherForTests(srcPoms, aether)

    // WHEN
    val treeFile: Option[String] = pomMod.depTreeFilename()

    // THEN
    Assert.assertEquals(Some("target/dep.tree"), treeFile)
    Assert.assertEquals(Seq(), pomMod.depTreeFilenames())
  }

  @Test
  def pluginDeps(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")
    val pomMod = PomMod.ofAetherForTests(srcPoms, aether)

    // WHEN
    val deps: Seq[PluginDep] = pomMod.listPluginDependencies

    // THEN
    assertPluginDeps(Anyshop1Deps.plugins(), deps)
  }

  @Test
  def mavenDependecyPlugin(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")
    val pomMod = PomMod.ofAetherForTests(srcPoms, aether)

    // WHEN
    val dep: Seq[PluginDep] = pomMod.mavenDependencyPlugins

    // THEN
    assertPluginDeps(Seq(PluginDep(PomRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", "2.8",
      Seq(
        PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map("outputFile" -> "dep.tree")),
        PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project"))), dep)
  }

  @Test
  def findNodesAndSetVersion(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("shop1")
    val orgMod = PomMod.ofAetherForTests(orgPoms, aether)
    Assert.assertEquals("27.0.0-SNAPSHOT", orgMod.selfVersion)
    assertDeps(Anyshop1Deps.ownDeps(), orgMod.listDependecies.filter(_.artifactId.contains("anyshop")))

    val targetPoms = TestHelper.testResources("shop4")
    orgMod.writeTo(targetPoms)
    val targetMod = PomMod.ofAetherForTests(targetPoms, aether)
    val newVersion = "any-SNAPSHOT"
    // WHEN
    Assert.assertEquals(Map("dep.tree" -> Nil,
      "anyshop-erp" -> Seq("com.novomind.ishop.shops.anyshop:anyshop-erp:jar:27.0.0-SNAPSHOT"),
      "anyshop-shop" -> Seq("+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:tests:27.0.0-SNAPSHOT:test",
        "+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:27.0.0-SNAPSHOT:compile")),
      depTreeMap(targetMod))
    targetMod.findNodesAndChangeVersion("com.novomind.ishop.shops.anyshop", "anyshop-erp", "27.0.0-SNAPSHOT", newVersion)

    Assert.assertEquals(Map("dep.tree" -> Nil,
      "anyshop-erp" -> Seq("com.novomind.ishop.shops.anyshop:anyshop-erp:jar:" + newVersion),
      "anyshop-shop" -> Seq("+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:tests:" + newVersion + ":test",
        "+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:" + newVersion + ":compile")),
      depTreeMap(targetMod))
    targetMod.writeTo(targetPoms)

    // THEN
    val newTargetMod = PomMod.ofAetherForTests(targetPoms, aether)
    Assert.assertEquals("27.0.0-SNAPSHOT", newTargetMod.selfVersion)
    val erpVersionChanged = Anyshop1Deps.ownDeps().map(in => if (in.artifactId.contains("anyshop-erp") && in.version.nonEmpty) {
      in.copy(version = newVersion)
    } else {
      in
    })
    assertDeps(erpVersionChanged, newTargetMod.listDependecies.filter(_.artifactId.contains("anyshop")))

  }

  @Test
  def changeVersionSub(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("pom-packed-sub-sub")
    val orgMod = PomMod.ofAetherForTests(orgPoms, aether)

    assertDeps(Seq(
      Dep(PomRef("any:a-parent:1.0.0-SNAPSHOT"),
        "", "", "", "", "", "", ""),
      Dep(PomRef("other:a:1.0.0-SNAPSHOT"),
        "any", "a-parent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("any:bparent:1.0.0-SNAPSHOT"),
        "any", "a-parent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("any:bsub:1.0.0-SNAPSHOT"),
        "any", "bparent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("bsub:1.0.0-SNAPSHOT"),
        "other", "a", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("any:c:1.0.0-SNAPSHOT"),
        "any", "a-parent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("c:1.0.0-SNAPSHOT"),
        "any", "bsub", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("c:1.0.0-SNAPSHOT"),
        "any", "other", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("c:1.0.0-SNAPSHOT"),
        "any", "final", "1.0.1", "", "", "", "")
    ), orgMod.listDependecies)
    assertDeps(Seq(Dep(PomRef("c:1.0.0-SNAPSHOT"), "any", "other", "1.0.0-SNAPSHOT", "", "", "", "")), orgMod.listSnapshots)
    orgMod.changeVersion("master-SNAPSHOT")
    orgMod.writeTo(orgPoms)
    val orgMod1 = PomMod.ofAetherForTests(orgPoms, aether)
    orgMod1.changeVersion("1.0.0-SNAPSHOT")
    orgMod1.writeTo(orgPoms)
  }

  @Test
  def changeVersion(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("shop1")
    val orgMod = PomMod.ofAetherForTests(orgPoms, aether)
    val srcPoms = TestHelper.testResources("shop3")
    orgMod.writeTo(srcPoms)

    // WHEN
    val mod = PomMod.ofAetherForTests(srcPoms, aether)
    assertDeps(Anyshop1Deps.selfVersion("27.0.0-SNAPSHOT"), mod.listSelf)
    assert("27.0.0-SNAPSHOT" === mod.getVersionFromDocs())
    assertDeps(Anyshop1Deps.all(), mod.listDependecies)

    mod.changeVersion("RC-2017.01-SNAPSHOT")
    mod.writeTo(srcPoms)

    val allMod = Anyshop1Deps.all()
      .map(dep => if (dep.artifactId == "anyshop-erp" && dep.version.nonEmpty) {
        dep.copy(version = "RC-2017.01-SNAPSHOT")
      } else if (dep.artifactId == "anyshop-projects" && dep.version.nonEmpty) {
        dep.copy(version = "RC-2017.01-SNAPSHOT")
      } else {
        dep
      }).map(dep =>
      dep.copy(pomRef = PomRef(dep.pomRef.id.replace("27.0.0-SNAPSHOT", "RC-2017.01-SNAPSHOT")))
    )

    // THEN
    val newMod = PomMod.ofAetherForTests(srcPoms, aether)
    assertDeps(allMod, newMod.listDependecies)
  }

  @Test
  def replaceProperty(): Unit = {
    Assert.assertEquals("ab", PomMod.replaceProperty(Map("a" -> "b"))("a${a}"))

    TestHelper.assertException("No property replacement found in pom.xmls for: \"a${\" " +
      "- define properties where they are required and not in parent pom.xml. Input is Nil.",
      classOf[IllegalArgumentException], () => {
        PomMod.replaceProperty(Map("a" -> "b"))("a${")
      })

    TestHelper.assertException("No property replacement found in pom.xmls for: \"a${b}\" " +
      "- define properties where they are required and not in parent pom.xml. Input is Nil.",
      classOf[IllegalArgumentException], () => {
        PomMod.replaceProperty(Map("a" -> "b"))("a${b}")
      })

    TestHelper.assertException("No property replacement found in pom.xmls for: \"a${}\" " +
      "- define properties where they are required and not in parent pom.xml. Input is Nil.",
      classOf[IllegalArgumentException], () => {
        PomMod.replaceProperty(Map("a" -> "b"))("a${}")
      })

    TestHelper.assertException("No property replacement found in pom.xmls for: \"${u}\" " +
      "- define properties where they are required and not in parent pom.xml. Input is Nil.",
      classOf[IllegalArgumentException], () => {
        PomMod.replaceProperty(Map("a" -> "b"))("${u}")
      })

    TestHelper.assertException("property map is empty", classOf[IllegalStateException], () => {
      PomMod.replaceProperty(Map.empty)("a${b}")
    })
  }

  @Test
  def changeGA(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("shop1")
    val orgMod = PomMod.ofAetherForTests(orgPoms, aether)
    val srcPoms = TestHelper.testResources("shop5")
    orgMod.writeTo(srcPoms)

    // WHEN
    val mod = PomMod.ofAetherForTests(srcPoms, aether)
    assertDeps(Anyshop1Deps.selfVersion("27.0.0-SNAPSHOT"), mod.listSelf)
    assert("27.0.0-SNAPSHOT" === mod.getVersionFromDocs())
    assertDeps(Anyshop1Deps.all(), mod.listDependecies)

    mod.changeShopGroupArtifact("anyshop")
    mod.writeTo(srcPoms)

    val allMod: Seq[Dep] = Anyshop1Deps.all()
      .map(dep => if (!Seq("anyshop-commons", "anyshop-db-migration", "anyshop-ipim-reviews").contains(dep.artifactId)) {
        dep.copy(groupId = dep.groupId.replace("anyshop", "anyshop"),
          artifactId = dep.artifactId.replace("anyshop", "anyshop"),
          pomRef = dep.pomRef.copy(id = dep.pomRef.id.replace("anyshop", "anyshop")))
      } else {
        dep.copy(pomRef = dep.pomRef.copy(id = dep.pomRef.id.replace("anyshop", "anyshop")))
      })

    // THEN
    val s = Seq(
      Dep(PomRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", "27.0.0-SNAPSHOT", "", "", "pom", ""),
      Dep(PomRef("anyshop-erp"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", "27.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:anyshop"),
        "com.novomind.ishop.shops", "anyshop", "27.0.0-SNAPSHOT", "", "", "war", "")
    )
    val newMod = PomMod.ofAetherForTests(srcPoms, aether)
    assertDeps(s, newMod.listSelf)
    assertDeps(allMod, newMod.listDependecies)

  }

  @Test
  def suggestReleaseVersion(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN
    val releaseVersion = PomMod.ofAetherForTests(srcPoms, aether).suggestReleaseVersion()

    // THEN
    assert(Seq("0.11") === releaseVersion)
  }

  @Test
  def suggestReleaseVersionFail(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1/anyshop-erp")

    // WHEN / THEN
    TestHelper.assertException("anyshop-erp as no version, please define", classOf[IllegalStateException],
      () => PomMod.ofAetherForTests(srcPoms, aether).suggestReleaseVersion())

  }

  @Test
  def suggestNextRelease(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val next = PomMod.ofAetherForTests(srcPoms, aether).suggestNextRelease("27.0.0")

    // THEN
    assert("27.0.1" === next)
  }

  @Test
  def suggestNextRelease_other(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val next = PomMod.ofAetherForTests(srcPoms, aether).suggestNextRelease("28.0.0")

    // THEN
    Assert.assertEquals("28.0.1", next)
  }

  @Test
  def suggestNextRelease_shop(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("RC-2017.02-SNAPSHOT")

    // THEN
    Assert.assertEquals("RC-2017.03", next)
  }

  @Test
  def suggestNextRelease_shop_patch(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("RC-2017.02.1-SNAPSHOT")

    // THEN
    Assert.assertEquals("RC-2017.03", next)
  }

  def suggestNextReleaseBy(currentVersion: String): String = {
    PomMod.suggestNextReleaseBy(currentVersion, currentVersion)
  }

  @Test
  def suggestNextRelease_shop_patch_sub(): Unit = {

    // GIVEN/WHEN
    val release = suggestNextReleaseBy("RC-2018.17.5_1-SNAPSHOT")

    // THEN
    Assert.assertEquals("RC-2018.18", release)
  }

  @Test
  def suggestNextRelease_shop_patch_sub_invalid(): Unit = {

    // GIVEN/WHEN
    val release = suggestNextReleaseBy("RC-2018.17.5.1-SNAPSHOT")

    // THEN
    assert("RC-2018.17.5.1-UNDEF" === release)
  }

  @Test
  def suggestNextRelease_shop_next_year(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("RC-2017.52-SNAPSHOT")

    // THEN
    assert("RC-2018.01" === next)
  }

  @Test
  def suggestNextRelease_shop_next(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("RC-2017.2-SNAPSHOT")

    // THEN
    assert("RC-2017.03" === next)
  }

  @Test
  def suggestNextRelease_x(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("28x-SNAPSHOT", "28.0.1-SNAPSHOT")

    // THEN
    Assert.assertEquals("28x", next)
  }

  @Test
  def suggestNextRelease_shop_master(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("master-SNAPSHOT", "RC-2017.52-SNAPSHOT")

    // THEN
    assert("master" === next)
  }

  @Test
  def suggestNextRelease_shop_feature(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("phase2-SNAPSHOT", "RC-2017.52-SNAPSHOT")

    // THEN
    assert("phase2" === next)
  }

  @Test
  def suggestNextRelease_shop_stable(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("31x-stable-SNAPSHOT", "31x-stable.1-SNAPSHOT")

    // THEN
    assert("31x-stable-RELEASE-DEMO-DELETE-ME" === next)
  }

  @Test
  def suggestNextRelease_any(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("mööp-SNAPSHOT")

    // THEN
    assert("mööp-UNDEF" === next)
  }

  @Test
  def suggestNextRelease_lowdash(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("28.0.0_1")

    // THEN
    assert("28.0.0_2" === next)
  }

  @Test
  def suggestNextRelease_no_bugfix(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("28.0")

    // THEN
    assert("28.1.0" === next)
  }

  @Test
  def suggestNextRelease_no_minor(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("28")

    // THEN
    assert("29.0.0" === next)
  }

  @Test
  def suggestReleaseVersionShop(): Unit = {
    Assert.assertEquals(Seq("27.0.0-SNAPSHOT"),
      PomMod.suggestReleaseBy(LocalDate.now(), "27.0.0-SNAPSHOT", hasShopPom = true, Nil))
  }

  @Test
  def suggestRelease_shop(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "RC-2017.52-SNAPSHOT", hasShopPom = true, Nil)

    // THEN
    assert(Seq("RC-2017.52-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_shop_minor(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "RC-2017.52.1-SNAPSHOT", hasShopPom = true, Nil)

    // THEN
    assert(Seq("RC-2017.52.1-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_shop_existing(): Unit = {
    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "RC-2017.52-SNAPSHOT", hasShopPom = true, Seq("release/RC-2017.52"))

    // THEN
    assert(Seq("RC-2017.52.1-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_shop_existing_two(): Unit = {
    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "RC-2017.52.1-SNAPSHOT", hasShopPom = true, Seq("release/RC-2017.52.1"))

    // THEN
    assert(Seq("RC-2017.52.2-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_shop_existing_three(): Unit = {
    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "RC-2017.52.1_1-SNAPSHOT", hasShopPom = true, Seq("release/RC-2017.52.1_1"))

    // THEN
    assert(Seq("RC-2017.52.2-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_other(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "3.43.2-SNAPSHOT", hasShopPom = false, Nil)

    // THEN
    assert(Seq("3.43.2") === release)
  }

  @Test
  def suggestRelease_other_lowdash(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "3.43.2_3-SNAPSHOT", hasShopPom = false, Nil)

    // THEN
    assert(Seq("3.43.2_3") === release)
  }

  @Test
  def suggestRelease_other_lowdash_text(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "3.43.2_mööp-SNAPSHOT", hasShopPom = false, Nil)

    // THEN
    assert(Seq("3.43.2_mööp") === release)
  }

  @Test
  def suggestRelease_other_master(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "master-SNAPSHOT", hasShopPom = false, Nil)

    // THEN
    assert(Seq("master") === release)
  }

  @Test
  def suggestRelease_other_x(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "x34-SNAPSHOT", hasShopPom = false, Nil)

    // THEN
    assert(Seq("x34") === release)
  }

  @Test
  def suggestRelease_shop_master(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "master-SNAPSHOT", hasShopPom = true, Nil)

    // THEN
    Assert.assertEquals(Seq("RC-2017.05-SNAPSHOT", "RC-2017.06-SNAPSHOT", "RC-2017.07-SNAPSHOT"), release)
  }

  @Test
  def suggestRelease_shop_master_used(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "master-SNAPSHOT", hasShopPom = true,
      Seq("hula", "release/RC-2017.05", "release/RC-2017.07", "release/RC-2017.07.1", "release/RC-2017.07.2"))

    // THEN
    Assert.assertEquals(Seq("RC-2017.05.1-SNAPSHOT", "RC-2017.06-SNAPSHOT", "RC-2017.07.3-SNAPSHOT"), release)
  }

  @Test
  def suggestRelease_shop_x(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "28x-SNAPSHOT", hasShopPom = true, Nil)

    // THEN
    assert(Seq("28.0.0-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_shop_x_used(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "29x-SNAPSHOT", hasShopPom = true,
      Seq("hula", "release/RC-2017.05", "release/29.0.0"))

    // THEN
    assert(Seq("29.0.1-SNAPSHOT", "29.1.0-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_shop_x_used_minor(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "29x-SNAPSHOT", hasShopPom = true,
      Seq("hula", "release/RC-2017.05", "release/29.0.0", "release/29.0.1"))

    // THEN
    assert(Seq("29.0.2-SNAPSHOT", "29.1.0-SNAPSHOT") === release)
  }

  @Test
  def suggestRelease_shop_x_used_major(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "29x-SNAPSHOT", hasShopPom = true,
      Seq("hula", "release/29.0.0", "release/29.1.0", "release/29.1.1"))

    // THEN
    assert(Seq("29.1.2-SNAPSHOT", "29.2.0-SNAPSHOT") === release)
  }

  @Test
  def testIsUnknownReleasePattern(): Unit = {
    Assert.assertFalse(PomMod.isUnknownReleasePattern("RC-2018.08-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("RC-2018.08.1-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("RC-2018.08.1_4-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("RC-2018.08"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("1"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("1.0"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("1.0.0"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("1.0.0_1"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("1.0.0-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("RC-2018.08.1"))
    Assert.assertTrue(PomMod.isUnknownReleasePattern("RC.2018.08.1"))
    Assert.assertTrue(PomMod.isUnknownReleasePattern("RC.2018.08.1.3"))
    Assert.assertTrue(PomMod.isUnknownReleasePattern("1."))
    Assert.assertTrue(PomMod.isUnknownReleasePattern("RC-2018-08.1"))
    Assert.assertTrue(PomMod.isUnknownReleasePattern("RC-2018.08-1"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("master-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownReleasePattern("master"))
  }

  @Test
  def isShopPomSubmodule(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1/anyshop-erp")

    // WHEN / THEN
    Assert.assertFalse(PomMod.ofAetherForTests(srcPoms, aether).isShop)
  }

  @Test
  def isShopPomMini(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN / THEN
    Assert.assertFalse(PomMod.ofAetherForTests(srcPoms, aether).isShop)
    Assert.assertTrue(PomMod.ofAetherForTests(srcPoms, aether).isNoShop)
  }

  @Test
  def isShopPomShop(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN / THEN
    Assert.assertTrue(PomMod.ofAetherForTests(srcPoms, aether).isShop)
  }

  @Test
  def writeOthers(): Unit = {
    // GIVEN
    val srcPoms: File = pomTestFile(temp, Anyshop1Deps.rootPom, Anyshop1Deps.rootTree)
      .sub("anyshop-erp", Anyshop1Deps.anyshopErpPom, Anyshop1Deps.anyshopErpTree)
      .sub("anyshop-shop", Anyshop1Deps.anyshopPom, Anyshop1Deps.anyshopTree)
      .create()
    val targetPoms = TestHelper.testResources("shop2")

    // WHEN
    PomMod.ofAetherForTests(srcPoms, aether)
      .writeTo(targetPoms)

    // THEN
    assert(Nil === TestHelper.localChanges())
  }

  @Test
  def listDependecies(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomMod.ofAetherForTests(srcPoms, aether).listDependecies

    // THEN
    assertDeps(Anyshop1Deps.all(), deps)
  }

  @Test
  def listSnapshots(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomMod.ofAetherForTests(srcPoms, aether).listSnapshots

    // THEN
    assertDeps(Anyshop1Deps.snapshots(), deps)
  }

  @Test
  def listSnapshotsMini(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN
    val deps = PomMod.ofAetherForTests(srcPoms, aether).listSnapshots

    // THEN
    assertDeps(Seq(Dep(PomRef("com.novomind.ishop.any:any:0.11-SNAPSHOT"),
      "org.springframework", "spring-other", "0.11-SNAPSHOT", "", "", "", "")), deps)
  }

  @Test
  def listSnapshotsPackgeDepsPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    val deps = PomMod.ofAetherForTests(srcPoms, aether).listSnapshots

    // THEN
    assertDeps(Seq(Dep(PomRef("com.novomind.ishop.core:ishop-core-parent"),
      "com.novomind.ishop", "meta", "29.0.0-SNAPSHOT", "pom", "", "", "tests")), deps)
  }

  @Test
  def listSelfMod(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomMod.ofAetherForTests(srcPoms, aether).selfDepsMod

    // THEN
    assertDeps(Anyshop1Deps.selfMod(), deps)
  }

  @Test
  def listSelf(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomMod.ofAetherForTests(srcPoms, aether).listSelf

    // THEN
    assertDeps(Anyshop1Deps.self(), deps)
  }

  @Test
  def listSelfDepsPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    // WHEN
    val deps = PomMod.ofAetherForTests(srcPoms, aether).listSelf

    // THEN
    assertDeps(PomPackagedDeps.self(), deps)
  }

  @Test
  def testHasNoShopPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    // WHEN / THEN
    Assert.assertTrue(PomMod.ofAetherForTests(srcPoms, aether).isNoShop)
  }

  @Test
  def testHasShopPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN / THEN
    Assert.assertTrue(PomMod.ofAetherForTests(srcPoms, aether).isShop)
  }

  @Test
  def testCheckNoSlashesNotEmptyNoZeros(): Unit = {
    Assert.assertEquals("s", PomMod.checkNoSlashesNotEmptyNoZeros("s"))
    TestHelper.assertException("empty is not allowed", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros(""))
    TestHelper.assertException("no slashes are allowed in \"sdf/s/\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("sdf/s/"))
    TestHelper.assertException("no slashes are allowed in \"sdf\\s\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("sdf\\s"))
    TestHelper.assertException("no leading zeros are allowed in \"00.0.1\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("00.0.1"))
    TestHelper.assertException("no leading zeros are allowed in \"0\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("0"))
    TestHelper.assertException("no leading zeros are allowed in \"00\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("00"))
    TestHelper.assertException("no leading zeros are allowed in \"0.0\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("0.0"))
    TestHelper.assertException("no leading zeros are allowed in \"0.0.0\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("0.0.0"))
    Assert.assertEquals("0.1.0", PomMod.checkNoSlashesNotEmptyNoZeros("0.1.0"))
    Assert.assertEquals("0.0.1", PomMod.checkNoSlashesNotEmptyNoZeros("0.0.1"))
    Assert.assertEquals("0.1", PomMod.checkNoSlashesNotEmptyNoZeros("0.1"))
    Assert.assertEquals("0.100", PomMod.checkNoSlashesNotEmptyNoZeros("0.100"))
    Assert.assertEquals("0.1000", PomMod.checkNoSlashesNotEmptyNoZeros("0.1000"))
    TestHelper.assertException("no trailing zeros are allowed in \"0.0.1.00\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("0.0.1.00"))
    TestHelper.assertException("no trailing zeros are allowed in \"0.0.1.000\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("0.0.1.000"))
    TestHelper.assertException("no trailing zeros are allowed in \"0.0.1-000\"", classOf[IllegalArgumentException],
      () => PomMod.checkNoSlashesNotEmptyNoZeros("0.0.1-000"))
  }

  @Test
  def testWrite(): Unit = {

    val file = File.createTempFile("release", "test")
    val writer: BufferedWriter = new BufferedWriter(new FileWriter(file))
    writer.write("hello")
    try {
      Sgit.getOs match {
        case Sgit.Os.Windows => {
          TestHelper.assertException("Windows tends to lock file handles." +
            " Try to find handle or DLL that locks the file. e.g. with Sysinternals Process Explorer",
            classOf[IllegalStateException], () => {
              PomMod.writeContent(file, "asdf")
            })
        }
        case _ => // do not test
      }
    } finally {
      writer.close()
      Assert.assertTrue("no testfile", file.exists())
      Assert.assertTrue("delete of file failed", file.delete())
    }

  }

  @Test
  def listProperties(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val props = PomMod.ofAetherForTests(srcPoms, aether).listProperties

    // THEN
    val expected = Map(
      "ishop-core.version" -> "27.1.2-SNAPSHOT",
      "project.build.sourceEncoding" -> "UTF-8",
      "ishop-plugin.version" -> "27.3.0-SNAPSHOT",
      "project.version" -> "27.0.0-SNAPSHOT",
      "webappDirectory" -> "skip/skip",
      "aspectj.version" -> "1.8.8",
      "aspectj-maven-plugin.version" -> "1.8",
      "ishop-core-sub.version" -> "27.1.2-SNAPSHOT",
      "build.timestamp" -> "skip",
      "zkm.skip" -> "true",
      "bo-client" -> "27.0.0-SNAPSHOT",
      "allowedProjectDir" -> "skip/any",
      "project.reporting.outputEncoding" -> "UTF-8",
      "java.version" -> "1.8")
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
      """<dependencies><dependency>      <groupId>com.novomind.ishop.shops.anyshop</groupId><br />
        |      <artifactId>anyshop-commons</artifactId>
        |      <version>27.0.0-SNAPSHOT</version></dependency><br    />
        |  <dependency>   <groupId>com.novomind.ishop.shops.anyshop</groupId>
        |      <artifactId>anyshop-erp</artifactId>
        |     <version>27.0.0-SNAPSHOT</version></dependency>
        |      </dependencies>""".stripMargin

    val result = PomMod.formatDependecy(input, "com.novomind.ishop.shops.anyshop", "anyshop-erp", "27.0.0-SNAPSHOT", "27.0.0")

    Assert.assertEquals(
      """<dependencies><dependency>      <groupId>com.novomind.ishop.shops.anyshop</groupId><br />
        |      <artifactId>anyshop-commons</artifactId>
        |      <version>27.0.0-SNAPSHOT</version></dependency><br />
        |  <dependency>   <groupId>com.novomind.ishop.shops.anyshop</groupId>
        |      <artifactId>anyshop-erp</artifactId>
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
        "Seq(" + pluginExec.map(in => {
          val goals = in.goals.map(in => "\"" + in + "\"").mkString(",")
          val conf = in.config.toList.map(in => "\"" + in._1 + "\" -> \"" + in._2 + "\"").mkString(",")
          "PluginExec(\"" + in.id + "\", Seq(" + goals + "), \"" + in.phase + "\", Map(" + conf + "))"
        }).mkString(", ") + ")"
      }

      "PluginDep(PomRef(\"" + dep.pomRef.id + "\"),\n \"" + dep.groupId + "\", \"" +
        dep.artifactId + "\", \"" + dep.version + "\", " + formatExec(dep.execs) +
        ",\n Seq(" + dep.pomPath.map(in => "\"" + in + "\"").mkString(", ") + "))"
    }

    assertBy(expected, actual, defstr)
  }

  @Test
  def testVersionParse(): Unit = {
    Assert.assertEquals(Version("", 1, 2, 3, ""), Version.parse("1.2.3"))
    Assert.assertEquals(Version("", 1, 2, 3, "6"), Version.parse("1.2.3_6"))
    Assert.assertEquals(Version("", 1, 2, 3, "final"), Version.parse("1.2.3_final"))
    Assert.assertEquals(Version("", 3, 2, 1, ""), Version.parse("3.2.1-SNAPSHOT"))
    Assert.assertEquals(Version("", 7, 0, 0, ""), Version.parse("7"))
    Assert.assertEquals(Version("", 8, 43, 0, ""), Version.parse("8.43"))
  }

  @Test
  def testVersionOrdering(): Unit = {
    val in = Seq(
      Version.parse("1.2.3"),
      Version.parse("2.2.3"),
      Version.parse("2.2.4"),
      Version.parse("2.3.0"),
      Version.parse("7"),
      Version.parse("9.1"),
      Version.parse("21.2.3")
    )
    Assert.assertEquals(in, in.sorted)
  }

  @Test
  def testAbbreviate(): Unit = {
    Assert.assertEquals(Nil, PomMod.abbreviate(2)(Nil))
    Assert.assertEquals(Seq("a"), PomMod.abbreviate(2)(Seq("a")))
    Assert.assertEquals(Seq("a", "b"), PomMod.abbreviate(2)(Seq("a", "b")))
    Assert.assertEquals(Seq("a", "b"), PomMod.abbreviate(3)(Seq("a", "b")))
    Assert.assertEquals(Seq("a", "..", "c"), PomMod.abbreviate(2)(Seq("a", "b", "c")))
    Assert.assertEquals(Seq("a", "..", "d"), PomMod.abbreviate(2)(Seq("a", "b", "c", "d")))
    Assert.assertEquals(Seq("a", "..", "c", "d"), PomMod.abbreviate(3)(Seq("a", "b", "c", "d")))
  }

  @Test
  def testUnmanged(): Unit = {
    Assert.assertEquals(Nil, PomMod.unmanged(Nil, Nil))
    TestHelper.assertException("invalid empty versions", classOf[IllegalArgumentException],
      () => PomMod.unmanged(
        Seq(Gav(groupId = "a.b", artifactId = "a", version = "1.0.0")),
        Seq(Gav(groupId = "a.b", artifactId = "a", version = "1.0.0"))
      )
    )

    Assert.assertEquals(Nil, PomMod.unmanged(Seq(Gav(groupId = "a.b", artifactId = "a", version = "", scope = "test")),
      Seq(Gav(groupId = "a.b", artifactId = "a", version = "1.0.0", scope = "compile"))))

    Assert.assertEquals(Nil, PomMod.unmanged(Seq(Gav(groupId = "", artifactId = "", version = "")), Nil))
    Assert.assertEquals(Nil, PomMod.unmanged(Seq(Gav.empty.copy(groupId = "a")), Seq(Gav.empty.copy(groupId = "a"))))
    Assert.assertEquals(Nil, PomMod.unmanged(Seq(Gav.empty.copy(artifactId = "a")), Seq(Gav.empty.copy(artifactId = "a"))))
  }

  @Test
  def testReplacedDepTreesVersionSnapshot(): Unit = {
    val out = PomMod.replacedDepTreesVersion("com.any:any-some:jar:1.0.0",
      "com.any", "any-some", "1.0.0",
      "1.0.0-SNAPSHOT")
    Assert.assertEquals("com.any:any-some:jar:1.0.0-SNAPSHOT\n", out)

    val out2 = PomMod.replacedDepTreesVersion(out,
      "com.any", "any-some", "1.0.0",
      "1.0.0-SNAPSHOT")
    Assert.assertEquals("com.any:any-some:jar:1.0.0-SNAPSHOT\n", out2)
  }

  @Test
  def testReplacedDepTreesVersion(): Unit = {
    val out = PomMod.replacedDepTreesVersion("com.any:any-some:jar:1.0.0",
      "com.any", "any-some", "1.0.0",
      "1.0.1")
    Assert.assertEquals("com.any:any-some:jar:1.0.1\n", out)

    val out2 = PomMod.replacedDepTreesVersion(out,
      "com.any", "any-some", "1.0.1",
      "1.0.0-SNAPSHOT")
    Assert.assertEquals("com.any:any-some:jar:1.0.0-SNAPSHOT\n", out2)
  }

  @Test
  def testReplacedDepTreesVersion_noChange(): Unit = {
    val out = PomMod.replacedDepTreesVersion("com.any:any-some:jar:1.0.0",
      "com.any", "any-some", "1.0.1",
      "1.0.1")
    Assert.assertEquals("com.any:any-some:jar:1.0.0\n", out)
  }

  @Test
  def testAsserts(): Unit = {
    // @formatter:off
    val elem: Elem = <test>
      <!-- bla --></test>
    // @formatter:on
    val doc = PomModTest.document(elem)
    val s = PomMod.toString(doc)
    val value = "<test>\n      <!-- bla --></test>"
    Assert.assertEquals(value, s)

    PomModTest.assertDocs(Seq(doc), Seq(Xpath.newDocument(value)))
    TestHelper.assertAssertionError(
      "expected:<<test>\n<!-- [bla] --></test>> but was:<<test>\n<!-- [invalid] --></test>>",
      classOf[ComparisonFailure], () => {
        PomModTest.assertDocs(Seq(doc), Seq(Xpath.newDocument("<test>\n  <!-- invalid --></test>")))
      })
    PomModTest.assertElems(Seq(elem), Seq(Xpath.newDocument(value)))
    TestHelper.assertAssertionError(
      "expected:<<test>\n<!-- [bla] --></test>> but was:<<test>\n<!-- [invalid] --></test>>",
      classOf[ComparisonFailure], () => {
        PomModTest.assertElems(Seq(elem), Seq(Xpath.newDocument("<test>\n  <!-- invalid --></test>")))
      })
  }

}

object PomModTest {
  def pomTestFile(temp: TemporaryFolder, root: Document, treeFileContent: String = ""): TestFileBuilder = {
    TestFileBuilder(temp, root, treeFileContent, Nil)
  }

  def assertDeps(expected: Seq[Dep], actual: Seq[Dep]) = {
    def defstr(dep: Dep): String = {
      "Dep(PomRef(\"" + dep.pomRef.id + "\"),\n \"" + dep.groupId + "\", \"" +
        dep.artifactId + "\", \"" + dep.version + "\", \"" + dep.typeN + "\", \"" +
        dep.scope + "\", \"" + dep.packaging + "\", \"" + dep.classifier + "\")"
    }

    assertBy(expected, actual, defstr)
  }

  private def assertBy[L](expected: Seq[L], actual: Seq[L], fn: L => String) = {
    Assert.assertEquals(expected.map(fn).mkString(",\n"), actual.map(fn).mkString(",\n"))
    Assert.assertEquals(expected, actual)
  }

  def depTreeMap(pomMod: PomMod): Map[String, Seq[String]] = {
    pomMod.depTreeFileContents.toList.map(entry => {
      val erpLines = entry._2.content.linesIterator.filter(l => l.contains("anyshop-erp")).toList
      val key = if (pomMod.file.getName == entry._1.getParentFile.getName) {
        entry._1.getName
      } else {
        entry._1.getParentFile.getName
      }
      (key, erpLines)
    }).foldLeft(Map.empty[String, Seq[String]])(_ + _)
  }

  def document(elem: Elem): Document = Xpath.newDocument(elem.toString())

  def pomfile(doc: Document) = RawPomFile(new File("f"), doc, new File("f"))

  sealed case class TestFileBuilder(temp: TemporaryFolder, root: Document, treeFileContent: String = "",
                                    subs: Seq[(String, Document, String, Seq[(String, Document, String)])]) {

    def sub(foldername: String, document: Document, treeFileContent: String = "",
            subsub: Seq[(String, Document, String)] = Nil): TestFileBuilder = {
      // TODO create readable signature for subsub
      this.copy(subs = subs ++ Seq((foldername, document, treeFileContent, subsub)))
    }

    def create(): File = {
      val rootDir = temp.newFolder("release-pom-mod-test")
      PomMod.writePom(new File(rootDir, "pom.xml"), root)
      if (treeFileContent != "") {
        PomMod.writeContent(new File(rootDir, "dep.tree"), treeFileContent)
      }
      subs.foreach(in => {
        val sub = new File(rootDir, in._1)
        sub.mkdir()
        PomMod.writePom(new File(sub, "pom.xml"), in._2)
        if (treeFileContent != "") {
          PomMod.writeContent(new File(sub, "dep.tree"), in._3)
        }

        if (in._4 != Nil) {
          in._4.foreach(in => {
            val subsub = new File(sub, in._1)
            subsub.mkdir()
            PomMod.writePom(new File(subsub, "pom.xml"), in._2)
            if (treeFileContent != "") {
              PomMod.writeContent(new File(subsub, "dep.tree"), in._3)
            }
          })

        }
      })
      rootDir
    }
  }

  def assertElems(expected: Seq[Elem], actual: Seq[Document]): Unit = {
    assertDocs(expected.map(PomModTest.document), actual)
  }

  def assertDocs(expected: Seq[Document], actual: Seq[Document]): Unit = {
    def fmt(tf: Seq[Document]): String = {
      tf.map(PomMod.toString).map(in => in.linesIterator.map(_.trim()).mkString("\n")).mkString("\n---\n")
    }

    Assert.assertEquals(fmt(expected), fmt(actual))
  }
}
