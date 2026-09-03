package release

import org.junit.{Assert, Test}
import org.gradle.tooling.model.GradleModuleVersion
import org.gradle.tooling.model.idea.{IdeaDependencyScope, IdeaSingleEntryLibraryDependency}
import org.mockito.Mockito.*
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.SelfRef

class GradleModTest extends AssertionsForJUnit {

  @Test
  def convertsToolingApiDependencies(): Unit = {
    val module = mock(classOf[GradleModuleVersion])
    when(module.getGroup).thenReturn("org.example")
    when(module.getName).thenReturn("example-api")
    when(module.getVersion).thenReturn("3.2.1")
    val scope = mock(classOf[IdeaDependencyScope])
    when(scope.getScope).thenReturn("TEST")
    val dependency = mock(classOf[IdeaSingleEntryLibraryDependency])
    when(dependency.getGradleModuleVersion).thenReturn(module)
    when(dependency.getScope).thenReturn(scope)

    Assert.assertEquals(
      Some(ProjectModTest.depOfShort("org.example", "example-api", "3.2.1", "test")),
      GradleMod.fromToolingDependency(dependency))
  }

  @Test
  def parsesGroovyDependencies(): Unit = {
    val model = GradleMod.modelOfContents(
      """
        |group = 'org.example'
        |version = '1.2.3-SNAPSHOT'
        |def junitVersion = '5.12.2'
        |
        |repositories {
        |  maven { url 'https://repo.example.org/releases' }
        |}
        |
        |dependencies {
        |  implementation 'com.google.guava:guava:33.4.8-jre'
        |  testImplementation "org.junit.jupiter:junit-jupiter:$junitVersion"
        |  compileOnly group: 'org.jetbrains', name: 'annotations', version: '26.0.2'
        |}
        |""".stripMargin)

    Assert.assertEquals(Some("1.2.3-SNAPSHOT"), model.selfVersion)
    Assert.assertEquals(Some("org.example"), model.groupId)
    Assert.assertEquals(
      Seq(
        ProjectModTest.depOfShort("com.google.guava", "guava", "33.4.8-jre"),
        ProjectModTest.depOfShort("org.junit.jupiter", "junit-jupiter", "5.12.2", "test"),
        ProjectModTest.depOfShort("org.jetbrains", "annotations", "26.0.2", "provided")
      ),
      model.deps
    )
    Assert.assertEquals(Seq("https://repo.example.org/releases"), model.repositoryUrls)
  }

  @Test
  def parsesKotlinDependenciesAndProperties(): Unit = {
    val model = GradleMod.modelOfContents(
      """
        |plugins {
        |  java
        |}
        |
        |dependencies {
        |  api("org.slf4j:slf4j-api:${slf4jVersion}")
        |  runtimeOnly("ch.qos.logback:logback-classic:1.5.18")
        |  testFixturesImplementation(group = "org.assertj", name = "assertj-core", version = "3.27.3")
        |}
        |""".stripMargin,
      Map("version" -> "2.0.0", "slf4jVersion" -> "2.0.17")
    )

    Assert.assertEquals(Some("2.0.0"), model.selfVersion)
    Assert.assertEquals(
      Seq(
        ProjectModTest.depOfShort("org.slf4j", "slf4j-api", "2.0.17"),
        ProjectModTest.depOfShort("ch.qos.logback", "logback-classic", "1.5.18", "runtime"),
        ProjectModTest.depOfShort("org.assertj", "assertj-core", "3.27.3", "test")
      ),
      model.deps
    )
  }

  @Test
  def findsSnapshotDependencies(): Unit = {
    val model = GradleMod.modelOfContents("implementation(\"org.example:demo:1.0-SNAPSHOT\")")
    Assert.assertEquals(Some("1.0-SNAPSHOT"), model.deps.head.version)
    Assert.assertEquals(SelfRef.undef, model.deps.head.pomRef)
  }
}
