package release

import java.io.File

import org.junit.rules.TemporaryFolder
import org.junit.{Assert, Rule, Test}
import org.scalatest.junit.AssertionsForJUnit
import release.PomMod.{Dep, PomRef}
import release.PomModTest._

class PomModeTestParentsTest extends AssertionsForJUnit {

  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

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
    val newpom = PomMod(srcPoms)
    assertDeps(Seq(Dep(PomRef("com.novomind.ishop.shops.any:any-projects:28.0.0-SNAPSHOT"),
      "", "", "", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any-erp:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-projects", "28.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("com.novomind.any:any:28.0.0-SNAPSHOT"),
        "com.novomind.any", "any-projects", "27.0.0", "", "", "", "")), newpom.listDependecies)

    Assert.assertEquals(Map.empty, depTreeMap(newpom))

    newpom.changeVersion("12.12")
    newpom.writeTo(srcPoms)
    assertDeps(Seq(Dep(PomRef("com.novomind.ishop.shops.any:any-projects:12.12"),
      "", "", "", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any-erp:12.12"),
        "com.novomind.ishop.shops.any", "any-projects", "12.12", "", "", "", ""),
      Dep(PomRef("com.novomind.any:any:12.12"),
        "com.novomind.any", "any-projects", "27.0.0", "", "", "", "")), PomMod(srcPoms).listDependecies)
    Assert.assertEquals(3, newpom.allPomsDocs.size)
    val result = newpom.allPomsDocs.map(toElement)

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
      .create()

    // WHEN
    val newpom = PomMod(srcPoms)
    assertDeps(Seq(Dep(PomRef("com.novomind.ishop.shops.any:any-projects:28.0.0-SNAPSHOT"),
      "", "", "", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any-erp:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-projects", "28.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any-parent:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-projects", "28.0.0-SNAPSHOT", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any:28.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.any", "any-parent", "28.0.0-SNAPSHOT", "", "", "", "")), newpom.listDependecies)

    Assert.assertEquals(Map.empty, depTreeMap(newpom))
    Assert.assertEquals(4, newpom.allPomsDocs.size)

    newpom.changeVersion("12.12")
    newpom.writeTo(srcPoms)
    assertDeps(Seq(Dep(PomRef("com.novomind.ishop.shops.any:any-projects:12.12"),
      "", "", "", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any-erp:12.12"),
        "com.novomind.ishop.shops.any", "any-projects", "12.12", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any-parent:12.12"),
        "com.novomind.ishop.shops.any", "any-projects", "12.12", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.any:any:12.12"),
        "com.novomind.ishop.shops.any", "any-parent", "12.12", "", "", "", "")), PomMod(srcPoms).listDependecies)
    Assert.assertEquals(4, newpom.allPomsDocs.size)
    val result = newpom.allPomsDocs.map(toElement)

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
