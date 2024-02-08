package release

import java.io.File
import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Dep, PluginDep, SelfRef}
import release.PomModTest._
import release.Starter.Opts

class PomModeTestParentsTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  lazy val repo = Opts().newRepo

  implicit def toOpt(in: String): Option[String] = Option(in)

  @Test
  def testStrip(): Unit = {
    // GIVEN
    val in = document(<project>
      <modelVersion>4.0.0</modelVersion>
      <packaging>war</packaging>
      <groupId>com.some</groupId>
      <artifactId>some</artifactId>
      <dependencyManagement>
        <dependencies>
          <dependency>
            <groupId>some.group</groupId>
            <artifactId>artifact</artifactId>
            <version>1.0.0</version>
            <scope>import</scope>
            <type>pom</type>
          </dependency>
          <!-- comment -->
          <dependency>
            <groupId>some.group</groupId>
            <artifactId>artifact2</artifactId>
            <version>1.0.0</version>
            <scope>compile</scope>
            <!-- comment -->
          </dependency>
        </dependencies>
      </dependencyManagement>
      <dependencies>
        <dependency>
          <groupId>other</groupId>
          <artifactId>data-tests</artifactId>
          <!-- @release:keep-compile-scope -->
          <scope>compile</scope>
        </dependency>
        <dependency>
          <groupId>other</groupId>
          <artifactId>data</artifactId>
          <scope>compile</scope>
        </dependency>
        <dependency>
          <groupId>other2</groupId>
          <type>jar</type>
          <artifactId>data2</artifactId>
        </dependency>
        <dependency>
          <groupId>other3</groupId>
          <artifactId>data3</artifactId>
          <scope>test</scope>
        </dependency>
      </dependencies>
    </project>
    )

    // WHEN
    val doc = PomMod.stripDependencyDefaults(in)

    // THEN
    val result = Seq(doc)
    PomModTest.assertElems(Seq(
      <project>
        <modelVersion>4.0.0</modelVersion>
        <packaging>war</packaging>
        <groupId>com.some</groupId>
        <artifactId>some</artifactId>
        <dependencyManagement>
          <dependencies>
            <dependency>
              <groupId>some.group</groupId>
              <artifactId>artifact</artifactId>
              <version>1.0.0</version>
              <scope>import</scope>
              <type>pom</type>
            </dependency>
            <!-- comment -->
            <dependency>
              <groupId>some.group</groupId>
              <artifactId>artifact2</artifactId>
              <version>1.0.0</version>
              <!-- comment -->
            </dependency>
          </dependencies>
        </dependencyManagement>
        <dependencies>
          <dependency>
            <groupId>other</groupId>
            <artifactId>data-tests</artifactId>
            <!-- @release:keep-compile-scope -->
            <scope>compile</scope>
          </dependency>
          <dependency>
            <groupId>other</groupId>
            <artifactId>data</artifactId>
          </dependency>
          <dependency>
            <groupId>other2</groupId>
            <artifactId>data2</artifactId>
          </dependency>
          <dependency>
            <groupId>other3</groupId>
            <artifactId>data3</artifactId>
            <scope>test</scope>
          </dependency>
        </dependencies>
      </project>), result)
  }

  @Test
  def testChangeOfChildWithDifferentParent(): Unit = {
    val srcPoms: File = pomTestFile(temp, document(<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.novomind.ishop.shops.any</groupId>
      <artifactId>any-projects</artifactId>
      <version>28.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>
      <modules>
        <module>any-erp</module>
        <module>any</module>
      </modules>
    </project>
    ), "com.novomind.ishop.shops:any-projects:pom:28.0.0-SNAPSHOT")
      .sub("any-erp", document(<project>
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
      ), "com.novomind.ishop.shops:any-erp:jar:28.0.0-SNAPSHOT")
      .sub("any", document(<project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.any</groupId>
          <artifactId>any-projects</artifactId>
          <version>27.0.0</version>
        </parent>
        <artifactId>any</artifactId>
        <name>any</name>
        <version>28.0.0-SNAPSHOT</version>
      </project>
      ), "com.novomind.any:any:jar:28.0.0-SNAPSHOT").create()

    // WHEN
    val newpom = PomModTest.withRepoForTests(srcPoms, repo)
    Assert.assertEquals(None, newpom.mvnExtension)
    assertDeps(Seq(
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any-erp:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-projects", Some("28.0.0-SNAPSHOT"), pomPath = List("project", "parent", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.any:any:28.0.0-SNAPSHOT"),
        "com.novomind.any", "any-projects", Some("27.0.0"), pomPath = List("project", "parent", "artifactId"))), newpom.listDependencies)

    Assert.assertEquals(Map.empty, depTreeMap(newpom))

    newpom.changeVersion("12.12")
    newpom.writeTo(srcPoms)
    assertDeps(Seq(
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any-erp:12.12"),
        "com.novomind.ishop.shops.any", "any-projects", Some("12.12"), pomPath = List("project", "parent", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.any:any:12.12"),
        "com.novomind.any", "any-projects", Some("27.0.0"), pomPath = List("project", "parent", "artifactId"))),
      PomModTest.withRepoForTests(srcPoms, repo).listDependencies)
    Assert.assertEquals(3, newpom.allPomsDocs.size)
    val result = newpom.allPomsDocs

    PomModTest.assertElems(Seq(
      <project>
        <modelVersion>4.0.0</modelVersion>
        <groupId>com.novomind.ishop.shops.any</groupId>
        <artifactId>any-projects</artifactId>
        <version>12.12</version>
        <packaging>pom</packaging>
        <modules>
          <module>any-erp</module>
          <module>any</module>
        </modules>
      </project>,
      <project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.ishop.shops.any</groupId>
          <artifactId>any-projects</artifactId>
          <version>12.12</version>
          <relativePath>..</relativePath>
        </parent>
        <artifactId>any-erp</artifactId>
        <name>any-erp</name>
      </project>,
      <project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.any</groupId>
          <artifactId>any-projects</artifactId>
          <version>27.0.0</version>
        </parent>
        <artifactId>any</artifactId>
        <name>any</name>
        <version>12.12</version>
      </project>), result)
  }

  @Test
  def testChangeOfChildWithDifferentParent_sub(): Unit = {
    val srcPoms: File = pomTestFile(temp, document(<project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.novomind.ishop.shops.any</groupId>
      <artifactId>any-projects</artifactId>
      <version>28.0.0-SNAPSHOT</version>
      <packaging>pom</packaging>
      <modules>
        <module>any-erp</module>
        <module>any-parent</module>
      </modules>
    </project>
    ))
      .sub("any-erp", document(<project>
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
      ))
      .sub("any-parent", document(<project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.ishop.shops.any</groupId>
          <artifactId>any-projects</artifactId>
          <version>28.0.0-SNAPSHOT</version>
          <relativePath>..</relativePath>
        </parent>
        <packaging>pom</packaging>
        <artifactId>any-parent</artifactId>
        <name>any-parent</name>
        <modules>
          <module>any</module>
        </modules>
      </project>
      ), subsub = Seq(("any", document(<project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.ishop.shops.any</groupId>
          <artifactId>any-parent</artifactId>
          <version>28.0.0-SNAPSHOT</version>
          <relativePath>..</relativePath>
        </parent>
        <artifactId>any</artifactId>
        <name>any</name>
        <version>28.0.0-SNAPSHOT</version>
      </project>
      ), "")))
      .extension(document(
        <extensions>
          <extension>
            <groupId>a.b.maven</groupId>
            <artifactId>any</artifactId>
            <version>1.10.20-SNAPSHOT</version>
          </extension>
          <extension>
            <groupId>some.maven</groupId>
            <artifactId>other</artifactId>
            <version>3.2</version>
          </extension>
        </extensions>
      ))
      .create()

    // WHEN
    val newpom = PomModTest.withRepoForTests(srcPoms, repo)
    Assert.assertTrue(newpom.mvnExtension.isDefined)
    Assert.assertEquals(Seq(
      PluginDep(SelfRef.extensions,
        "a.b.maven", "any", Some("1.10.20-SNAPSHOT"),
        Nil, Nil),
      PluginDep(SelfRef.extensions,
        "some.maven", "other", Some("3.2"),
        Nil, Nil),
    ), newpom.listPluginDependencies)
    assertDeps(Seq(
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any-erp:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-projects", Some("28.0.0-SNAPSHOT"), pomPath = Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any-parent:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-projects", Some("28.0.0-SNAPSHOT"), pomPath = Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-parent", Some("28.0.0-SNAPSHOT"), pomPath = Seq("project", "parent", "artifactId"))),
      newpom.listDependencies)

    Assert.assertEquals(Map.empty, depTreeMap(newpom))
    Assert.assertEquals(4, newpom.allPomsDocs.size)

    newpom.changeVersion("12.12")
    newpom.writeTo(srcPoms)
    assertDeps(Seq(
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any-erp:12.12"),
        "com.novomind.ishop.shops.any", "any-projects", Some("12.12"), pomPath = List("project", "parent", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any-parent:12.12"),
        "com.novomind.ishop.shops.any", "any-projects", Some("12.12"), pomPath = List("project", "parent", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops.any:any:12.12"),
        "com.novomind.ishop.shops.any", "any-parent", Some("12.12"), pomPath = List("project", "parent", "artifactId"))),
      PomModTest.withRepoForTests(srcPoms, repo).listDependencies)
    Assert.assertEquals(4, newpom.allPomsDocs.size)
    val result = newpom.allPomsDocs

    PomModTest.assertElems(Seq(
      <project>
        <modelVersion>4.0.0</modelVersion>
        <groupId>com.novomind.ishop.shops.any</groupId>
        <artifactId>any-projects</artifactId>
        <version>12.12</version>
        <packaging>pom</packaging>
        <modules>
          <module>any-erp</module>
          <module>any-parent</module>
        </modules>
      </project>,
      <project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.ishop.shops.any</groupId>
          <artifactId>any-projects</artifactId>
          <version>12.12</version>
          <relativePath>..</relativePath>
        </parent>
        <artifactId>any-erp</artifactId>
        <name>any-erp</name>
      </project>,
      <project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.ishop.shops.any</groupId>
          <artifactId>any-projects</artifactId>
          <version>12.12</version>
          <relativePath>..</relativePath>
        </parent>
        <packaging>pom</packaging>
        <artifactId>any-parent</artifactId>
        <name>any-parent</name>
        <modules>
          <module>any</module>
        </modules>
      </project>,
      <project>
        <modelVersion>4.0.0</modelVersion>
        <parent>
          <groupId>com.novomind.ishop.shops.any</groupId>
          <artifactId>any-parent</artifactId>
          <version>12.12</version>
          <relativePath>..</relativePath>
        </parent>
        <artifactId>any</artifactId>
        <name>any</name>
        <version>12.12</version>
      </project>), result)
  }

}
