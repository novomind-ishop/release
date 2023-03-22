package release

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, ComparisonFailure, Ignore, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import org.w3c.dom._
import release.PomChecker.ValidationException
import release.PomMod.DepTree
import release.PomModTest._
import release.ProjectMod._
import release.Starter.{Opts, PreconditionsException}
import release.Util.linuxPath

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Paths
import java.time.{Duration, LocalDate, Month}
import scala.xml.Elem

class PomModTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  lazy val repo = new Repo(Opts())

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
    val out = PomMod.allRawModulePomsFiles(Seq(srcPoms), withSubPoms = true).map(in => srcPoms.toPath.relativize(in.toPath))
    Assert.assertEquals(Seq(Paths.get("pom.xml"), Paths.get("any-erp", "pom.xml")), out)
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
      "Dep(SelfRef(any-erp),com.novomind.ishop.shops.any,any-erp,28.0.0-SNAPSHOT,,,,) (27.0.0-SNAPSHOT, 28.0.0-SNAPSHOT)",
      classOf[IllegalArgumentException], () => {
        PomModTest.withRepoForTests(srcPoms, repo).selfVersion
      })

  }

  @Test
  def testApplyValueOfXpath(): Unit = {
    val doc = document(<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.novomind.ishop.shops.any</groupId>
      <artifactId>any-projects</artifactId>
      <version>27.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>any-erp</module>
      </modules>
    </project>
    )
    val raw = RawPomFile(new File("."), doc, new File("."))
    val path = "/project/modelVersion"
    val newValue = "new"
    PomMod.applyValueOfXpathTo(raw, path, "bert", newValue)
    Assert.assertEquals(Some("4.0.0"), Xpath.onlyString(doc, path))
    PomMod.applyValueOfXpathTo(raw, path, "4.0.0", newValue)
    Assert.assertEquals(Some(newValue), Xpath.onlyString(doc, path))
    PomMod.applyValueOfXpathTo(raw, path, newValue + newValue)
    Assert.assertEquals(Some(newValue + newValue), Xpath.onlyString(doc, path))
    PomMod.applyValueOfXpathTo(raw, path, "${x}")
    Assert.assertEquals(Some("${x}"), Xpath.onlyString(doc, path))
    PomMod.applyValueOfXpathTo(raw, path, "4.0.0", newValue)
    Assert.assertEquals(Some(newValue), Xpath.onlyString(doc, path))

  }

  @Test
  def testIsVariable(): Unit = {

    Assert.assertFalse(PomMod.isVariable("doc, path"))
    Assert.assertTrue(PomMod.isVariable("${x}"))
    Assert.assertFalse(PomMod.isVariable("${}"))
    Assert.assertFalse(PomMod.isVariable("$x"))
    Assert.assertFalse(PomMod.isVariable("x"))
  }

  @Test
  def testApplyVersionTo(): Unit = {
    val doc = document(<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.novomind.ishop.shops.any</groupId>
      <artifactId>any-projects</artifactId>
      <version>27.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <dependencies>
        <dependency>
          <groupId>org.glassfish.jaxb</groupId>
          <artifactId>jaxb-core</artifactId>
          <version>1.2.3</version>
        </dependency>
      </dependencies>
    </project>
    )
    val raw = RawPomFile(new File("."), doc, new File("."))
    val path = "/project/dependencies/dependency/version[1]"
    val newValue = "new"

    val self: Seq[Dep] = Seq(Gav3("org.glassfish.jaxb", "jaxb-core", "1.2.3").toDep(SelfRef.undef))

    PomMod.applyVersionTo(raw, self, newValue)
    Assert.assertEquals(Some(newValue), Xpath.onlyString(doc, path))
    PomMod.applyVersionTo(raw, self, ",", newValue + newValue)
    Assert.assertEquals(Some(newValue), Xpath.onlyString(doc, path))
    PomMod.applyVersionTo(raw, self, newValue, newValue + newValue)
    Assert.assertEquals(Some(newValue + newValue), Xpath.onlyString(doc, path))
    PomMod.applyVersionTo(raw, self, "${x}")
    Assert.assertEquals(Some("${x}"), Xpath.onlyString(doc, path))
    PomMod.applyVersionTo(raw, self, "any", newValue)
    Assert.assertEquals(Some(newValue), Xpath.onlyString(doc, path))
  }

  @Test
  def testVersionFrom(): Unit = {
    val doc = document(<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.novomind.ishop.shops.any</groupId>
      <artifactId>any-projects</artifactId>
      <version>27.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <dependencies>
        <dependency>
          <groupId>org.glassfish.jaxb</groupId>
          <artifactId>jaxb-core</artifactId>
          <version>1.2.3</version>
        </dependency>
      </dependencies>
    </project>
    )
    val raw = RawPomFile(new File("."), doc, new File("."))

    Assert.assertEquals(Some("27.0.0-SNAPSHOT"), PomMod.selectFirstVersionFrom(Seq(raw)))
  }

  @Test
  def testVersionProjectVersion(): Unit = {
    val k = "{project.version}"
    val srcPoms: File = pomTestFile(temp, document(<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.novomind.ishop.shops.any</groupId>
      <artifactId>any-projects</artifactId>
      <version>27.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>

      <modules>
        <module>any-erp</module>
      </modules>
      <dependencies>
        <dependency>
          <groupId>org.glassfish.jaxb</groupId>
          <artifactId>jaxb-core</artifactId>
          <version>${k}</version>
        </dependency>
      </dependencies>
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
      <version>${k}</version>
    </project>
    )).create()

    TestHelper.assertException("More then one Version found in your pom.xmls: " +
      "Dep(SelfRef(any-erp:${project.version}),com.novomind.ishop.shops.any,any-erp,${project.version},,,,) (27.0.0-SNAPSHOT, ${project.version})",
      classOf[IllegalArgumentException], () => {
        PomModTest.withRepoForTests(srcPoms, repo)
      })
  }

  @Test
  def writeSelf_sub_sub_tree(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("sub-sub-tree")

    // WHEN
    val mod = PomModTest.withRepoForTests(srcPoms, repo)
    Util.deleteRecursive(srcPoms)
    mod.writeTo(srcPoms)

  }

  @Test
  def testUpdates(): Unit = {
    // GIVEN
    val ref = GavWithRef(
      SelfRef("com.novomind.ishop:ishop-shop-frontend:42.0.0-SNAPSHOT:pom"),
      Gav(groupId = "com.github.eirslett", artifactId = "frontend-maven-plugin", version = "1.11.2"))
    val in: Seq[(GavWithRef, (Seq[String], Duration))] = Seq((ref, (List("1.11.3", "1.12.0"), Duration.parse("PT2063H9M3S"))))

    // WHEN
    val out = ProjectMod.toUpdats(in, ProjectMod.rangeFnOf(""))

    // THEN
    Assert.assertEquals(Seq((Gav3("com.github.eirslett", "frontend-maven-plugin", "1.11.2"), "1.12.0")), out)

  }

  @Test
  @Ignore
  def writeSelf(): Unit = {
    // GIVEN
    val srcPoms = new File("/Users/thomas/git/ishop-maven-plugin")

    // WHEN
    val mod = PomModTest.withRepoForTests(srcPoms, repo)
    mod.changeVersion("BERT-SNAPSHOT")
    mod.writeTo(srcPoms)

  }

  @Test
  def findSelected(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    val pomMod = PomModTest.withRepoForTests(srcPoms, repo)
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

    Assert.assertEquals(Seq(baseVersion, noVersion, testScope, testsScope, pomClassifier).sortBy(_.toString()),
      Xpath.mapToSeqMap(nodes).sortBy(_.toString()))
    Assert.assertEquals(Seq(
      Gav3("com.novomind.ishop.shops", "anyshop", ""),
      Gav3("com.novomind.ishop.shops.anyshop", "anyshop-erp", ""),
      Gav3("com.novomind.ishop.shops.anyshop", "anyshop-projects", ""),
    ), pomMod.selfDepsModGavs())
  }

  @Test
  def depTreeFilename(): Unit = {
    // GIVEN
    val path = new File(".").toPath.toAbsolutePath.normalize()
    val srcPoms = TestHelper.testResources("shop1")
    val pomMod = PomModTest.withRepoForTests(srcPoms, repo)

    // WHEN
    val treeFile: Option[String] = pomMod.depTreeFilename()

    // THEN
    Assert.assertEquals(Some("dep.tree"), treeFile)
    Assert.assertEquals(Seq("src/test/resources/shop1/dep.tree",
      "src/test/resources/shop1/anyshop-erp/dep.tree",
      "src/test/resources/shop1/anyshop-shop/dep.tree"),
      pomMod.depTreeFiles(treeFile, pomMod.rawSub).map(f => path.relativize(f.toPath).toStringLinux))
  }

  @Test
  def depTreeFilename_defect(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("defect-dep-tree")
    val pomMod = PomModTest.withRepoForTests(srcPoms, repo)

    // WHEN
    val treeFile: Option[String] = pomMod.depTreeFilename()

    // THEN
    Assert.assertEquals(Some("target/dep.tree"), treeFile)
    Assert.assertEquals(Seq(), pomMod.depTreeFiles(treeFile, pomMod.rawSub))
  }

  @Test
  def pluginDeps(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")
    val pomMod = PomModTest.withRepoForTests(srcPoms, repo)

    // WHEN
    val deps: Seq[PluginDep] = pomMod.listPluginDependencies

    // THEN
    assertPluginDeps(Anyshop1Deps.plugins(), deps)
  }

  @Test
  def mavenDependecyPlugin(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")
    val pomMod = PomModTest.withRepoForTests(srcPoms, repo)

    // WHEN
    val dep: Seq[PluginDep] = pomMod.mavenDependencyPlugins

    // THEN
    assertPluginDeps(Seq(PluginDep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
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
    val orgMod = PomModTest.withRepoForTests(orgPoms, repo)
    Assert.assertEquals("27.0.0-SNAPSHOT", orgMod.selfVersion)
    assertDeps(Anyshop1Deps.ownDeps(), orgMod.listDependecies.filter(_.artifactId.contains("anyshop")))
    val value = Map("dep.tree" -> Nil,
      "anyshop-erp" -> Seq("com.novomind.ishop.shops.anyshop:anyshop-erp:jar:27.0.0-SNAPSHOT"),
      "anyshop-shop" -> Seq("+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:tests:27.0.0-SNAPSHOT:test",
        "+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:27.0.0-SNAPSHOT:compile"))
    Assert.assertEquals(value, depTreeMap(orgMod))
    val targetPoms = TestHelper.testResources("shop4")
    Util.deleteRecursive(targetPoms)
    orgMod.writeTo(targetPoms)
    val targetMod = PomModTest.withRepoForTests(targetPoms, repo)

    // WHEN
    Assert.assertEquals(value, depTreeMap(targetMod))

    val newVersion = "ubglu-SNAPSHOT"
    targetMod.findNodesAndChangeVersion("com.novomind.ishop.shops.anyshop", "anyshop-erp", "27.0.0-SNAPSHOT", newVersion)

    Assert.assertEquals(Map("dep.tree" -> Nil,
      "anyshop-erp" -> Seq("com.novomind.ishop.shops.anyshop:anyshop-erp:jar:" + newVersion),
      "anyshop-shop" -> Seq("+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:tests:" + newVersion + ":test",
        "+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:" + newVersion + ":compile")),
      depTreeMap(targetMod))
    targetMod.writeTo(targetPoms)

    // THEN
    val newTargetMod = PomModTest.withRepoForTests(targetPoms, repo)
    Assert.assertEquals("27.0.0-SNAPSHOT", newTargetMod.selfVersion)
    val erpVersionChanged = Anyshop1Deps.ownDeps().map(in => if (in.artifactId.contains("anyshop-erp") && in.version.nonEmpty) {
      in.copy(version = newVersion)
    } else {
      in
    })
    assertDeps(erpVersionChanged, newTargetMod.listDependecies.filter(_.artifactId.contains("anyshop")))
    targetMod.findNodesAndChangeVersion("com.novomind.ishop.shops.anyshop", "anyshop-erp", newVersion, "27.0.0-SNAPSHOT")
    targetMod.writeTo(targetPoms)

  }

  @Test
  def changeVersionSub(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("pom-packed-sub-sub")
    val orgMod = PomModTest.withRepoForTests(orgPoms, repo)

    assertDeps(Seq(
      Dep(SelfRef("any:a-parent:1.0.0-SNAPSHOT"),
        "", "", "", "", "", "", ""),
      Dep(SelfRef("other:a:1.0.0-SNAPSHOT"),
        "any", "a-parent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("any:bparent:1.0.0-SNAPSHOT"),
        "any", "a-parent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("any:bsub:1.0.0-SNAPSHOT"),
        "any", "bparent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("bsub:1.0.0-SNAPSHOT"),
        "other", "a", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("any:c:1.0.0-SNAPSHOT"),
        "any", "a-parent", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("c:1.0.0-SNAPSHOT"),
        "any", "bsub", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("c:1.0.0-SNAPSHOT"),
        "any", "other", "1.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("c:1.0.0-SNAPSHOT"),
        "any", "final", "1.0.1", "", "", "", "")
    ), orgMod.listDependecies)
    assertDeps(Seq(Dep(SelfRef("c:1.0.0-SNAPSHOT"), "any", "other", "1.0.0-SNAPSHOT", "", "", "", "")), orgMod.listSnapshots)
    orgMod.changeVersion("master-SNAPSHOT")
    orgMod.writeTo(orgPoms)
    val orgMod1 = PomModTest.withRepoForTests(orgPoms, repo)
    orgMod1.changeVersion("1.0.0-SNAPSHOT")
    orgMod1.writeTo(orgPoms)
  }

  @Test
  def changeVersion(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("shop1")
    val orgMod = PomModTest.withRepoForTests(orgPoms, repo)
    val srcPoms = TestHelper.testResources("shop3")
    orgMod.writeTo(srcPoms)

    // WHEN
    val mod = PomModTest.withRepoForTests(srcPoms, repo)
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
      dep.copy(pomRef = SelfRef(dep.pomRef.id.replace("27.0.0-SNAPSHOT", "RC-2017.01-SNAPSHOT")))
    )

    // THEN
    val newMod = PomModTest.withRepoForTests(srcPoms, repo)
    assertDeps(allMod, newMod.listDependecies)
  }

  @Test
  def replacePropertySloppy(): Unit = {
    Assert.assertEquals("ab",
      PomMod.replaceProperty(Map("a" -> "b", "x" -> "x"), sloppy = true)("a${a}"))

    Assert.assertEquals("ab${b}",
      PomMod.replaceProperty(Map("a" -> "b", "x" -> "x"), sloppy = true)("a${a}${b}"))

    Assert.assertEquals("ab${b}b",
      PomMod.replaceProperty(Map("a" -> "b", "x" -> "x"), sloppy = true)("a${a}${b}${a}"))

    Assert.assertEquals("ab${b}b${b}",
      PomMod.replaceProperty(Map("a" -> "b", "x" -> "x"), sloppy = true)("a${a}${b}${a}${b}"))

    TestHelper.assertException("unbalanced curly braces: a${",
      classOf[IllegalArgumentException], () => {
        PomMod.replaceProperty(Map("a" -> "b"), sloppy = true)("a${")
      })

    Assert.assertEquals("a${b}", PomMod.replaceProperty(Map("a" -> "b"), sloppy = true)("a${b}"))

    TestHelper.assertException("empty curly braces: a${}",
      classOf[IllegalArgumentException], () => {
        PomMod.replaceProperty(Map("a" -> "b"), sloppy = true)("a${}")
      })

    Assert.assertEquals("${u}", PomMod.replaceProperty(Map("a" -> "b"), sloppy = true)("${u}"))

    Assert.assertEquals("a${b}", PomMod.replaceProperty(Map.empty, sloppy = true)("a${b}"))
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
  def changeGA_fail(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("shop1")
    // WHEN
    val mod = PomModTest.withRepoForTests(orgPoms, repo)

    TestHelper.assertException(
      "invalid groupidArtifactName \"x-anyshop\"; must match '[a-z0-9]+'" +
        " - We enforce this to build GAs like: 'org.example.{groupidArtifactName}.main:{groupidArtifactName}-service'" +
        " - for example dashes (-) are not part of the naming conventions for groupIds and dots (.) not for artifactIds" +
        " - see also: https://maven.apache.org/guides/mini/guide-naming-conventions.html", classOf[PreconditionsException], () => {
        mod.changeShopGroupArtifact("x-anyshop")
      })

  }

  @Test
  def changeGA(): Unit = {
    // GIVEN
    val orgPoms = TestHelper.testResources("shop1")
    val orgMod = PomModTest.withRepoForTests(orgPoms, repo)
    val srcPoms = TestHelper.testResources("shop5")
    orgMod.writeTo(srcPoms)

    // WHEN
    val mod = PomModTest.withRepoForTests(srcPoms, repo)
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
      Dep(SelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", "27.0.0-SNAPSHOT", "", "", "pom", ""),
      Dep(SelfRef("anyshop-erp"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", "27.0.0-SNAPSHOT", "", "", "", ""),
      Dep(SelfRef("com.novomind.ishop.shops:anyshop"),
        "com.novomind.ishop.shops", "anyshop", "27.0.0-SNAPSHOT", "", "", "war", "")
    )
    val newMod = PomModTest.withRepoForTests(srcPoms, repo)
    assertDeps(s, newMod.listSelf)
    assertDeps(allMod, newMod.listDependecies)

  }

  @Test
  def suggestReleaseVersion(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN
    val releaseVersion = PomModTest.withRepoForTests(srcPoms, repo).suggestReleaseVersion()

    // THEN
    assert(Seq("0.11") === releaseVersion)
  }

  @Test
  def suggestReleaseVersionFail(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1/anyshop-erp")

    // WHEN / THEN
    TestHelper.assertException("anyshop-erp as no version, please define", classOf[IllegalStateException],
      () => PomModTest.withRepoForTests(srcPoms, repo).suggestReleaseVersion())

  }

  @Test
  def suggestNextRelease(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val next = PomModTest.withRepoForTests(srcPoms, repo).suggestNextRelease("27.0.0")

    // THEN
    assert("27.0.1" === next)
  }

  @Test
  def suggestNextRelease_other(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val next = PomModTest.withRepoForTests(srcPoms, repo).suggestNextRelease("28.0.0")

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
    val release = suggestNextReleaseBy("RC-2018/17.5.1-SNAPSHOT")

    // THEN
    assert("RC-2018/17.5.1-UNDEF" === release)
  }

  @Test
  def suggestNextRelease_shop_next_year(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("RC-2017.52-SNAPSHOT")

    // THEN
    assert("RC-2018.01" === next)
  }

  @Test
  def suggestNextRelease_shop_next_missingLeadingZero(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("RC-2017.2-SNAPSHOT")

    // THEN
    assert("RC-2017.03" === next)
  }

  @Test
  def suggestKnownPattern_ubglu(): Unit = {
    // GIVEN
    val name = "ubglu"
    Assert.assertTrue(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("", PomMod.suggestKnownPattern(name))
  }

  @Test
  def suggestKnownPattern_digits_known(): Unit = {
    // GIVEN
    val name = "1.2.3"
    Assert.assertFalse(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("", PomMod.suggestKnownPattern(name))
  }

  @Test
  def suggestKnownPattern_digits_unknown_var(): Unit = {
    // GIVEN
    val name = "1.2.3,4,2"
    Assert.assertTrue(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("", PomMod.suggestKnownPattern(name))
  }

  @Test
  def suggestKnownPattern_digits_unknown(): Unit = {
    // GIVEN
    val name = "1.21,3.48"
    Assert.assertTrue(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("Maybe one of the following versions is what you want: '1.21.3_48', '1.21.3-48'\n",
      PomMod.suggestKnownPattern(name))
  }

  @Test
  def suggestKnownPattern_shop_unknown_beta(): Unit = {
    // GIVEN
    val name = "RC-2022.15.01-beta2"
    Assert.assertTrue(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("Maybe one of the following versions is what you want: 'RC-2022.15.1'\n",
      PomMod.suggestKnownPattern(name))
  }

  @Test
  def suggestKnownPattern_shop_unknown(): Unit = {
    // GIVEN
    val name = "RC-2022.05.00"
    Assert.assertTrue(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("Maybe one of the following versions is what you want: 'RC-2022.05'\n",
      PomMod.suggestKnownPattern(name))
  }

  @Test
  def suggestKnownPattern_asdf(): Unit = {
    // GIVEN
    val name = "RC-0.05.00"
    Assert.assertTrue(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("invalid inner creation RC-0.05",
      PomMod.trySuggestKnownPattern(name).failed.get.getMessage)
  }

  @Test
  def suggestKnownPattern_42x(): Unit = {
    // GIVEN
    val name = "42x"
    Assert.assertFalse(PomMod.isUnknownVersionPattern(name))
    // WHEN / THEN
    Assert.assertEquals("", PomMod.suggestKnownPattern(name))
  }

  @Test
  def suggestNextRelease_x(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("28x-SNAPSHOT", "28.0.1-SNAPSHOT")

    // THEN
    Assert.assertEquals("28x", next)
  }

  @Test
  def suggestNextRelease_xx(): Unit = {

    // GIVEN/WHEN
    val next = PomMod.suggestNextReleaseBy("28x", "28x-SNAPSHOT")

    // THEN
    Assert.assertEquals("28x", next)
  }

  @Test
  def suggestNextRelease_shop_typo_lowerCase(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestNextReleaseBy("rc-2021-48.2-SNAPSHOT", "rc-2021-48.2-SNAPSHOT")

    // THEN
    assert("RC-2021.49" === release)
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
  def suggestNextRelease_lowdash_with_letters(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("28.0.0_tc8.5.58")

    // THEN
    assert("28.0.1_tc8.5.58" === next)
  }

  @Test
  def suggestNextRelease_dash_with_letters_RC(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("28.0.0-RC1")

    // THEN
    assert("28.0.0" === next)
  }

  @Test
  def suggestNextRelease_dash_with_letters_milestone(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("28.0.0-M1")

    // THEN
    assert("28.0.0" === next)
  }

  @Test
  def suggestNextRelease_dash_with_letters(): Unit = {

    // GIVEN/WHEN
    val next = suggestNextReleaseBy("28.0.0-tc8.5.58")

    // THEN
    assert("28.0.1-tc8.5.58" === next)
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
  def suggestRelease_shop_typo(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "rc-2021-48.2-SNAPSHOT", hasShopPom = true, Nil)

    // THEN
    assert(Seq("RC-2021.48.2-SNAPSHOT") === release)
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
  def suggestRelease_other_main(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "main-SNAPSHOT", hasShopPom = false, Nil)

    // THEN
    assert(Seq("main") === release)
  }

  @Test
  def suggestRelease_other_x(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "x34-SNAPSHOT", hasShopPom = false, Nil)

    // THEN
    assert(Seq("x34") === release)
  }

  @Test
  def suggestRelease_other_x_with_tags(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "x34-SNAPSHOT", hasShopPom = false, Nil,
      Seq("34.0.0", "v34.0.1", "v35.0.0"))

    // THEN
    assert(Seq("35.0.0", "34.1.0", "34.0.2") === release)
  }

  @Test
  def suggestRelease_other_x_with_tags_and_supplier(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "x34-SNAPSHOT", hasShopPom = false, Nil,
      Seq("34.0.0", "v34.0.1", "v35.0.0"), () => "34.0.2")

    // THEN
    assert(Seq("34.0.2") === release)
  }

  @Test
  def suggestRelease_other_x_with_tags_and_supplier_outOfRange(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "x34-SNAPSHOT", hasShopPom = false, Nil,
      Seq("34.0.0", "v34.0.1", "v35.0.0"), () => "34.0.99")

    // THEN
    assert(Seq("35.0.0", "34.1.0", "34.0.2") === release)
  }

  @Test
  def suggestRelease_other_x_with_tags_and_supplier_outOfRange_major(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.now(), "x34-SNAPSHOT", hasShopPom = false, Nil,
      Seq("34.0.0", "v34.0.1", "v35.0.0"), () => "39.0.0")

    // THEN
    assert(Seq("35.0.0", "34.1.0", "34.0.2") === release)
  }

  @Test
  def suggestRelease_shop_master(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "master-SNAPSHOT", hasShopPom = true, Nil)

    // THEN
    Assert.assertEquals(Seq("RC-2017.05-SNAPSHOT", "RC-2017.06-SNAPSHOT", "RC-2017.07-SNAPSHOT"), release)
  }

  @Test
  def suggestRelease_shop_main(): Unit = {

    // GIVEN/WHEN
    val release = PomMod.suggestReleaseBy(LocalDate.of(2017, Month.FEBRUARY, 1), "main-SNAPSHOT", hasShopPom = true, Nil)

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
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC-2018.8"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("RC-2018.08-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("RC-2018.08.1-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("RC-2018.08.1_4-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("RC-2018.08"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("1"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("1.0"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("1.0.0"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("1.0.0_1"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("1.0.0-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("RC-2018.08.1"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC.2018.08.1"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC.2018.08.1.3"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC.2018/08.1"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC.2018/08.1.3"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("1."))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC-2018-08.1"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC-2018/08.1"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC-2018.08-1"))
    Assert.assertTrue(PomMod.isUnknownVersionPattern("RC-2018/08-1"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("master-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("master"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("main-SNAPSHOT"))
    Assert.assertFalse(PomMod.isUnknownVersionPattern("main"))

  }

  @Test
  def isShopPomSubmodule(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1/anyshop-erp")

    // WHEN / THEN
    Assert.assertFalse(PomModTest.withRepoForTests(srcPoms, repo).isShop)
  }

  @Test
  def isShopPomMini(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN / THEN
    Assert.assertFalse(PomModTest.withRepoForTests(srcPoms, repo).isShop)
    Assert.assertTrue(PomModTest.withRepoForTests(srcPoms, repo).isNoShop)
  }

  @Test
  def isShopPomShop(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN / THEN
    Assert.assertTrue(PomModTest.withRepoForTests(srcPoms, repo).isShop)
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
    PomModTest.withRepoForTests(srcPoms, repo)
      .writeTo(targetPoms)

    // THEN
    assert(Nil === TestHelper.localChanges())
  }

  @Test
  def listDependecies(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomModTest.withRepoForTests(srcPoms, repo).listDependecies

    // THEN
    assertDeps(Anyshop1Deps.all(), deps)
  }

  @Test
  def listSnapshots(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomModTest.withRepoForTests(srcPoms, repo).listSnapshots

    // THEN
    assertDeps(Anyshop1Deps.snapshots(), deps)
  }

  @Test
  def listSnapshotsMini(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("mini")

    // WHEN
    val deps = PomModTest.withRepoForTests(srcPoms, repo).listSnapshots

    // THEN
    assertDeps(Seq(Dep(SelfRef("com.novomind.ishop.any:any:0.11-SNAPSHOT"),
      "org.springframework", "spring-other", "0.11-SNAPSHOT", "", "", "", "")), deps)
  }

  @Test
  def listSnapshotsPackgeDepsPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    val deps = PomModTest.withRepoForTests(srcPoms, repo).listSnapshots

    // THEN
    assertDeps(Nil, deps)
  }

  @Test
  def listSelfMod(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomModTest.withRepoForTests(srcPoms, repo).selfDepsMod

    // THEN
    assertDeps(Anyshop1Deps.selfMod(), deps)
  }

  @Test
  def listSelf(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val deps = PomModTest.withRepoForTests(srcPoms, repo).listSelf

    // THEN
    assertDeps(Anyshop1Deps.self(), deps)
  }

  @Test
  def listSelfDepsPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    // WHEN
    val deps = PomModTest.withRepoForTests(srcPoms, repo).listSelf

    // THEN
    assertDeps(PomPackagedDeps.self(), deps)
  }

  @Test
  def testHasNoShopPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("pom-packaged")

    // WHEN / THEN
    Assert.assertTrue(PomModTest.withRepoForTests(srcPoms, repo).isNoShop)
  }

  @Test
  def testHasShopPom(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN / THEN
    Assert.assertTrue(PomModTest.withRepoForTests(srcPoms, repo).isShop)
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
      Term.Os.getCurrent match {
        case Term.Os.Windows => {
          TestHelper.assertException("Windows tends to lock file handles." +
            " Try to find handle or DLL that locks the file. e.g. with Sysinternals Process Explorer",
            classOf[IllegalStateException], () => {
              PomMod.writeContent(file.getParentFile)(file, "asdf")
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
  def testWriteParent(): Unit = {

    val cmd: () => Unit = () => {
      PomMod.writeContent(new File("/a"))(new File("/b"), "asdf")
    }
    Term.Os.getCurrent match {
      case Term.Os.Windows => {
        TestHelper.assertException("C:\\b must start with C:\\a", classOf[IllegalStateException], cmd)
      }
      case _ => {
        TestHelper.assertException("/b must start with /a", classOf[IllegalStateException], cmd)
      }
    }
  }

  @Test
  def testWriteParent_relative(): Unit = {
    val cmd: () => Unit = () => {
      PomMod.writeContent(new File("/a/b/../.././b/a/."))(new File("/a/b/../.././a/b/."), "asdf")
    }
    Term.Os.getCurrent match {
      case Term.Os.Windows => {
        TestHelper.assertException("C:\\a\\b\\..\\..\\.\\a\\b\\. must start with C:\\a\\b\\..\\..\\.\\b\\a\\.",
          classOf[IllegalStateException], cmd)
      }
      case _ => {
        TestHelper.assertException("/a/b/../.././a/b/. must start with /a/b/../.././b/a/.",
          classOf[IllegalStateException], cmd)
      }
    }

  }

  @Test
  def listProperties(): Unit = {
    // GIVEN
    val srcPoms = TestHelper.testResources("shop1")

    // WHEN
    val props = PomModTest.withRepoForTests(srcPoms, repo).listProperties

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

    val result = PomMod.formatDependecy(input,
      "com.novomind.ishop.shops.anyshop",
      "anyshop-erp",
      "27.0.0-SNAPSHOT", "27.0.0")

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
    Assert.assertEquals(Version("", 1, 2, 3, "", ""), Version.parse("1.2.3"))
    Assert.assertEquals(Version("", 1, 2, 3, "6", ""), Version.parse("1.2.3_6"))
    Assert.assertEquals(Version("", 1, 2, 3, "final", ""), Version.parse("1.2.3_final"))
    Assert.assertEquals(Version("", 3, 2, 1, "", ""), Version.parse("3.2.1-SNAPSHOT"))
    Assert.assertEquals(Version("", 7, 0, 0, "", ""), Version.parse("7"))
    Assert.assertEquals(Version("", 8, 43, 0, "", ""), Version.parse("8.43"))
  }

  @Test
  def testVersionOrdering(): Unit = {
    val in = Seq(
      Version.parseSloppy("alpha"),
      Version.parse("1.2.3"),
      Version.parse("2.2.3"),
      Version.parse("2.2.4"),
      Version.parse("2.3.0"),
      Version.parse("7"),
      Version.parse("9.1"),
      Version.parseSloppy("10"),
      Version.parseSloppy("10-alpha"), // FIXME alpha should be for 10
      Version.parse("10.2.3"),
      Version.parse("21.2.3"),
    )
    val t = in.sliding(2, 1).flatMap(ts => {
      if (ts.size != 2) {
        throw new IllegalStateException("d")
      } else {
        Seq(Version.ordering.lt(ts.head, ts.last),
          Version.ordering.equiv(ts.head, ts.head),
          Version.ordering.equiv(ts.last, ts.last))
      }

    }).distinct.toList

    Assert.assertEquals(1, t.size)
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

  def mockEntry(content: String): (File, DepTree) = {
    (new File("mock"), DepTree(content))
  }

  @Test
  def testReplacedDepTreesVersionSnapshot(): Unit = {
    val out = PomMod.replacedDepTreesVersion(mockEntry("com.any:any-some:jar:1.0.0"),
      "com.any", "any-some", "1.0.0",
      "1.0.0-SNAPSHOT")
    Assert.assertEquals("com.any:any-some:jar:1.0.0-SNAPSHOT\n", out)

    val out2 = PomMod.replacedDepTreesVersion(mockEntry(out),
      "com.any", "any-some", "1.0.0",
      "1.0.0-SNAPSHOT")
    Assert.assertEquals("com.any:any-some:jar:1.0.0-SNAPSHOT\n", out2)

    val out3 = PomMod.replacedDepTreesVersion(mockEntry("com.novomind.ishop.backoffice:bo-client:war:40.1.4-SNAPSHOT"),
      "com.novomind.ishop.backoffice", "bo-client", "40.1.4-SNAPSHOT",
      "40.1.4")
    Assert.assertEquals("com.novomind.ishop.backoffice:bo-client:war:40.1.4\n", out3)
  }

  @Test
  def testReplacedDepTreesVersion(): Unit = {
    val out = PomMod.replacedDepTreesVersion(mockEntry("com.any:any-some:jar:1.0.0"),
      "com.any", "any-some", "1.0.0",
      "1.0.1")
    Assert.assertEquals("com.any:any-some:jar:1.0.1\n", out)

    val out2 = PomMod.replacedDepTreesVersion(mockEntry(out),
      "com.any", "any-some", "1.0.1",
      "1.0.0-SNAPSHOT")
    Assert.assertEquals("com.any:any-some:jar:1.0.0-SNAPSHOT\n", out2)
  }

  @Test
  def testReplacedDepTreesVersion_noChange(): Unit = {
    val out = PomMod.replacedDepTreesVersion(mockEntry("com.any:any-some:jar:1.0.0"),
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

  @Test
  def testNormalizeUnwanted(): Unit = {
    val gav = Gav3("some.group", "artifcat", "version")
    val result = ProjectMod.normalizeUnwantedVersions(gav,
      Seq("a", "beta", "3.0.2-rc", "2019-03-15T05-03-30-4d2c0d53", "2018-11-16T06-01-54-efdaa48-ignored-chars", "2018-06-04T04-23-07"))
    Assert.assertEquals(Seq("a"), result)
  }
}

object PomModTest {

  def withRepoForTests(file: File, repo: Repo, skipPropertyReplacement: Boolean = false,
                       withSubPoms: Boolean = true): PomMod = {
    PomMod(file, repo, Opts(), skipPropertyReplacement, withSubPoms)
  }

  def pomTestFile(temp: TemporaryFolder, root: Document, treeFileContent: String = ""): TestFileBuilder = {
    TestFileBuilder(temp, root, treeFileContent, Nil, None)
  }

  def assertDeps(expected: Seq[Dep], actual: Seq[Dep]) = {
    def defstr(dep: Dep): String = {
      "Dep(SelfRef(\"" + dep.pomRef.id + "\"),\n \"" + dep.groupId + "\", \"" +
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
                                    subs: Seq[(String, Document, String, Seq[(String, Document, String)])],
                                    extensions: Option[Document]) {

    def sub(foldername: String, document: Document, treeFileContent: String = "",
            subsub: Seq[(String, Document, String)] = Nil): TestFileBuilder = {
      // TODO create readable signature for subsub
      this.copy(subs = subs ++ Seq((foldername, document, treeFileContent, subsub)))
    }

    def extension(document: Document): TestFileBuilder = {
      this.copy(extensions = Some(document))
    }

    def create(): File = {
      val rootDir = temp.newFolder("release-pom-mod-test")
      if (extensions.isDefined) {
        val mvn = new File(rootDir, ".mvn")
        mvn.mkdir()
        PomMod.writeContent(rootDir)(new File(mvn, "extensions.xml"),
          PomMod.toString(extensions.get))
      }
      PomMod.writePom(rootDir)(new File(rootDir, "pom.xml"), root)
      if (treeFileContent != "") {
        PomMod.writeContent(rootDir)(new File(rootDir, "dep.tree"), treeFileContent)
      }
      subs.foreach(in => {
        val sub = new File(rootDir, in._1)
        sub.mkdir()
        PomMod.writePom(rootDir)(new File(sub, "pom.xml"), in._2)
        if (treeFileContent != "") {
          PomMod.writeContent(rootDir)(new File(sub, "dep.tree"), in._3)
        }

        if (in._4 != Nil) {
          in._4.foreach(in => {
            val subsub = new File(sub, in._1)
            subsub.mkdir()
            PomMod.writePom(rootDir)(new File(subsub, "pom.xml"), in._2)
            if (treeFileContent != "") {
              PomMod.writeContent(rootDir)(new File(subsub, "dep.tree"), in._3)
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
