package release

import ProjectMod.{Dep, PluginDep, PluginExec, SelfRef}

import scala.annotation.nowarn

object Anyshop1Deps {

  def plugins(): Seq[PluginDep] = Seq(
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.jacoco", "jacoco-maven-plugin", None, Seq(PluginExec("default-prepare-agent", Seq("prepare-agent"), "", Map()), PluginExec("default-report", Seq("report"), "prepare-package", Map())),
      Seq("plugin", "plugins", "build", "project")),
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-dependency-plugin", Some("2.8"), Seq(PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map("outputFile" -> "dep.tree")), PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true"))),
      Seq("plugin", "plugins", "build", "project")),
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "com.novomind.maven", "zkm-maven-plugin", None, Seq(PluginExec("", Seq(), "", Map())),
      Seq("plugin", "plugins", "build", "project")),
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.apache.maven.plugins", "maven-checkstyle-plugin", Some("2.17"), Seq(PluginExec("validate", Seq("check"), "validate", Map("configLocation" -> "checkstyle.xml", "includeTestSourceDirectory" -> "true", "consoleOutput" -> "true", "encoding" -> "UTF-8", "failsOnError" -> "true"))),
      Seq("plugin", "plugins", "build", "project")),
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "org.codehaus.mojo", "aspectj-maven-plugin", Some("1.8"), Seq(PluginExec("", Seq("compile", "test-compile"), "", Map())),
      Seq("plugin", "plugins", "pluginManagement", "build", "project")),
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "", "maven-surefire-plugin", None, Seq(),
      Seq("plugin", "plugins", "build", "profile", "profiles", "project")),
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT:pom"),
      "", "maven-failsafe-plugin", Some("2.19.1"), Seq(PluginExec("", Seq("integration-test", "verify"), "", Map())),
      Seq("plugin", "plugins", "build", "profile", "profiles", "project")),
    PluginDep(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT:war"),
      "org.apache.maven.plugins", "maven-compiler-plugin", None, Seq(),
      Seq("plugin", "plugins", "build", "project"))
  )

  def snapshots(): Seq[Dep] = {
    Seq(
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops", "ishop-shop-parent", Some("27.0.0-SNAPSHOT"), "", "", "", "", Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all", Some("27.1.2-SNAPSHOT"), "pom", "", "", "", Seq("project", "dependencyManagement", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all-tests", Some("27.1.2-SNAPSHOT"), "pom", "test", "", "", Seq("project", "dependencyManagement", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops-ext", "ishop-shops-ext-shop-base", Some("27.0.0-SNAPSHOT"), "", "", "", "", Seq("project", "dependencyManagement", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-projects", Some("27.1.2-SNAPSHOT"), "pom", "import", "", "", Seq("project", "dependencyManagement", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "", "", "", Seq("project", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", pomPath = Seq("project", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-projects", Some("27.1.2-SNAPSHOT"), "pom", "import", "", "", Seq("project", "dependencyManagement", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "", "", "", pomPath = Seq("project", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", pomPath = Seq("project", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "pom", pomPath = Seq("project", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.backoffice", "bo-services", Some("27.1.2-SNAPSHOT"), "", "", "", "", pomPath = Seq("project", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.backoffice", "bo-core", Some("27.1.2-SNAPSHOT"), "", "", "", "", pomPath = Seq("project", "dependencies", "dependency"))
    )
  }

  def selfMod(): Seq[Dep] = {

    val k = Nil
    Seq(ProjectModTest.depOf(SelfRef.undef,
      "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "", "war", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "", "war", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "", "war", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "test", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "test", "war", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "", "test", "war", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "", "war", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "", "war", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "", "war", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "import", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "import", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "import", "war", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "import", "war", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "import", "war", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "runtime", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "test", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "test", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "test", "war", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "pom", "test", "war", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops", "anyshop", Some("27.0.0-SNAPSHOT"), "war", "runtime", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "test", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "", "", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "import", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "import", "", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "import", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "runtime", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "test", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "pom", "test", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "war", "runtime", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "pom", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "pom", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "pom", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "test", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "test", "pom", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "test", "pom", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "", "pom", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "", "pom", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "", "pom", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "import", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "import", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "import", "pom", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "import", "pom", "sources", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "import", "pom", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "runtime", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "test", "", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "test", "", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "test", "pom", "", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "pom", "test", "pom", "tests", k),
      ProjectModTest.depOf(SelfRef.undef,
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "war", "runtime", "", "", k)
    )
  }

  def selfVersion(version: String): Seq[Dep] = {
    Seq(
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:" + version),
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some(version), "", "", "pom", "", pomPath = Seq("project", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:" + version),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some(version), "", "", "", "", pomPath = Seq("project", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:" + version),
        "com.novomind.ishop.shops", "anyshop", Some(version), "", "", "war", "", pomPath = Seq("project", "artifactId"))
    )
  }

  def self(): Seq[Dep] = selfVersion("27.0.0-SNAPSHOT")

  def ownDeps(): Seq[Dep] = {
    val pPath = List("project", "dependencyManagement", "dependencies", "dependency")
    val oPath = List("project", "dependencies", "dependency")
    Seq(
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "", pPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "", "", Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "", "", "", oPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", oPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "", "", Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "", "", "", oPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", oPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", None, "", "", "", "", oPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "test", "", "", oPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", oPath),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "pom", oPath),
    )
  }

  def all(): Seq[Dep] = {
    val pomP = List("project", "dependencies", "dependency")
    val mng = Seq("project", "dependencyManagement", "dependencies", "dependency")
    val plugins = Seq("project", "build", "pluginManagement", "plugins", "plugin", "dependencies", "dependency")
    Seq(
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops", "ishop-shop-parent", Some("27.0.0-SNAPSHOT"), pomPath = Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "org.apache.httpcomponents", "httpclient", Some("4.5.1"), pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "org.apache.commons", "commons-lang3", Some("3.4"), "", "", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "io.reactivex", "rxjava", Some("1.0.12"), "", "", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.netflix.hystrix", "hystrix-core", Some("1.4.12"), "", "", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.github.dreamhead", "moco-core", Some("0.10.2"), "", "test", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all", Some("27.1.2-SNAPSHOT"), "pom", "", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all-tests", Some("27.1.2-SNAPSHOT"), "pom", "test", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops-ext", "ishop-shops-ext-shop-base", Some("27.0.0-SNAPSHOT"), "", "", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "org.apache.httpcomponents", "httpclient", None, "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "org.apache.commons", "commons-lang3", None, "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.github.dreamhead", "moco-core", None, "", "test", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.netflix.hystrix", "hystrix-core", None, "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "io.reactivex", "rxjava", None, "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all", None, "pom", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all-tests", None, "pom", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "org.aspectj", "aspectjrt", Some("1.8.8"), "", "", "", "", pomPath = plugins),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-projects:27.0.0-SNAPSHOT"),
        "org.aspectj", "aspectjtools", Some("1.8.8"), "", "", "", "", pomPath = plugins),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "", "", pomPath =
          Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-projects", Some("27.1.2-SNAPSHOT"), "pom", "import", "", "", pomPath = mng),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all", None, "pom", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops.anyshop:anyshop-erp:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all-tests", None, "pom", "test", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-projects", Some("27.0.0-SNAPSHOT"), "", "", "", "",
        pomPath =  Seq("project", "parent", "artifactId")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-projects", Some("27.1.2-SNAPSHOT"), "pom", "import", "", "",
        pomPath = Seq("project", "dependencyManagement", "dependencies", "dependency")),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-commons", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", None, "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "test", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "test", "", "tests", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops.anyshop", "anyshop-erp", Some("27.0.0-SNAPSHOT"), "", "", "", "pom", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all", None, "pom", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.core", "ishop-core-all-tests", None, "pom", "test", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.shops-ext", "ishop-shops-ext-shop-base", None, "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.backoffice", "bo-services", Some("27.1.2-SNAPSHOT"), "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "com.novomind.ishop.backoffice", "bo-core", Some("27.1.2-SNAPSHOT"), "", "", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "javax.servlet.jsp", "jsp-api", Some("2.2"), "", "provided", "", "", pomPath = pomP),
      ProjectModTest.depOf(ProjectModTest.parseSelfRef("com.novomind.ishop.shops:anyshop:27.0.0-SNAPSHOT"),
        "javax.el", "javax.el-api", Some("3.0.0"), "", "test", "", "", pomPath = pomP)
    )
  }
  @nowarn("msg=possible missing interpolator")
  val rootPom = Xpath.newDocument(
    """<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      |  <modelVersion>4.0.0</modelVersion>
      |
      |  <parent>
      |    <groupId>com.novomind.ishop.shops</groupId>
      |    <artifactId>ishop-shop-parent</artifactId>
      |    <version>27.0.0-SNAPSHOT</version>
      |  </parent>
      |
      |  <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |  <artifactId>anyshop-projects</artifactId>
      |  <version>27.0.0-SNAPSHOT</version>
      |  <packaging>pom</packaging>
      |
      |  <modules>
      |    <module>anyshop-erp</module>
      |    <module>anyshop-shop</module>
      |
      |  </modules>
      |
      |  <properties>
      |    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
      |    <project.reporting.outputEncoding>UTF-8</project.reporting.outputEncoding>
      |
      |    <!-- Plugin Versions -->
      |
      |    <java.version>1.8</java.version>
      |    <zkm.skip>true</zkm.skip>
      |    <webappDirectory>${project.build.directory}/${project.build.finalName}</webappDirectory>
      |
      |    <ishop-core.version>27.1.2-SNAPSHOT</ishop-core.version>
      |    <aspectj-maven-plugin.version>1.8</aspectj-maven-plugin.version>
      |    <aspectj.version>1.8.8</aspectj.version>
      |    <build.timestamp>${maven.build.timestamp}</build.timestamp>
      |  </properties>
      |
      |  <prerequisites>
      |    <maven>3.1.1</maven>
      |  </prerequisites>
      |
      |  <dependencyManagement>
      |    <dependencies>
      |      <dependency>
      |        <groupId>org.apache.httpcomponents</groupId>
      |        <artifactId>httpclient</artifactId>
      |        <version>4.5.1</version>
      |      </dependency>
      |      <dependency>
      |        <groupId>org.apache.commons</groupId>
      |        <artifactId>commons-lang3</artifactId>
      |        <version>3.4</version>
      |      </dependency>
      |      <dependency>
      |        <groupId>io.reactivex</groupId>
      |        <artifactId>rxjava</artifactId>
      |        <version>1.0.12</version>
      |      </dependency>
      |      <dependency>
      |        <groupId>com.netflix.hystrix</groupId>
      |        <artifactId>hystrix-core</artifactId>
      |        <version>1.4.12</version>
      |      </dependency>
      |      <dependency>
      |        <groupId>com.github.dreamhead</groupId>
      |        <artifactId>moco-core</artifactId>
      |        <version>0.10.2</version>
      |        <scope>test</scope>
      |      </dependency>
      |      <dependency>
      |        <groupId>com.novomind.ishop.core</groupId>
      |        <artifactId>ishop-core-all</artifactId>
      |        <version>${ishop-core.version}</version>
      |        <type>pom</type>
      |      </dependency>
      |      <dependency>
      |        <groupId>com.novomind.ishop.core</groupId>
      |        <artifactId>ishop-core-all-tests</artifactId>
      |        <version>${ishop-core.version}</version>
      |        <type>pom</type>
      |        <scope>test</scope>
      |      </dependency>
      |      <dependency>
      |        <groupId>com.novomind.ishop.shops-ext</groupId>
      |        <artifactId>ishop-shops-ext-shop-base</artifactId>
      |        <version>27.0.0-SNAPSHOT</version>
      |      </dependency>
      |      <dependency>
      |        <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |        <artifactId>anyshop-erp</artifactId>
      |        <version>${project.version}</version>
      |      </dependency>
      |    </dependencies>
      |  </dependencyManagement>
      |
      |  <dependencies>
      |    <dependency>
      |      <groupId>org.apache.httpcomponents</groupId>
      |      <artifactId>httpclient</artifactId>
      |    </dependency>
      |    <dependency>
      |      <groupId>org.apache.commons</groupId>
      |      <artifactId>commons-lang3</artifactId>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.github.dreamhead</groupId>
      |      <artifactId>moco-core</artifactId>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.netflix.hystrix</groupId>
      |      <artifactId>hystrix-core</artifactId>
      |    </dependency>
      |    <dependency>
      |      <groupId>io.reactivex</groupId>
      |      <artifactId>rxjava</artifactId>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.core</groupId>
      |      <artifactId>ishop-core-all</artifactId>
      |      <type>pom</type>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.core</groupId>
      |      <artifactId>ishop-core-all-tests</artifactId>
      |      <type>pom</type>
      |    </dependency>
      |  </dependencies>
      |
      |  <build>
      |    <plugins>
      |      <plugin>
      |        <groupId>org.jacoco</groupId>
      |        <artifactId>jacoco-maven-plugin</artifactId>
      |        <executions>
      |          <execution>
      |            <id>default-prepare-agent</id>
      |            <goals>
      |              <goal>prepare-agent</goal>
      |            </goals>
      |          </execution>
      |          <execution>
      |            <id>default-report</id>
      |            <phase>prepare-package</phase>
      |            <goals>
      |              <goal>report</goal>
      |            </goals>
      |          </execution>
      |        </executions>
      |      </plugin>
      |      <plugin>
      |        <groupId>org.apache.maven.plugins</groupId>
      |        <artifactId>maven-dependency-plugin</artifactId>
      |        <version>2.8</version>
      |        <executions>
      |          <execution>
      |            <id>pre-build-validate-tree</id>
      |            <phase>validate</phase>
      |            <!-- mvn dependency:tree -DoutputFile=target/dependency-tree -->
      |            <goals>
      |              <goal>tree</goal>
      |            </goals>
      |            <configuration>
      |              <outputFile>dep.tree</outputFile>
      |            </configuration>
      |          </execution>
      |          <execution>
      |            <id>pre-build-validate-list</id>
      |            <phase>validate</phase>
      |            <!-- mvn dependency:list -DoutputFile=dep.list -Dsort=true -->
      |            <goals>
      |              <goal>list</goal>
      |            </goals>
      |            <configuration>
      |              <outputFile>dep.list</outputFile>
      |              <sort>true</sort>
      |            </configuration>
      |          </execution>
      |        </executions>
      |      </plugin>
      |      <plugin>
      |        <groupId>com.novomind.maven</groupId>
      |        <artifactId>zkm-maven-plugin</artifactId>
      |        <configuration>
      |          <skip>true</skip>
      |        </configuration>
      |        <executions>
      |          <execution>
      |            <phase />
      |            <inherited>false</inherited>
      |          </execution>
      |        </executions>
      |      </plugin>
      |      <plugin>
      |        <groupId>org.apache.maven.plugins</groupId>
      |        <artifactId>maven-checkstyle-plugin</artifactId>
      |        <version>2.17</version>
      |        <executions>
      |          <execution>
      |            <id>validate</id>
      |            <phase>validate</phase>
      |            <configuration>
      |              <configLocation>checkstyle.xml</configLocation>
      |              <encoding>UTF-8</encoding>
      |              <consoleOutput>true</consoleOutput>
      |              <includeTestSourceDirectory>true</includeTestSourceDirectory>
      |              <failsOnError>true</failsOnError>
      |            </configuration>
      |            <goals>
      |              <goal>check</goal>
      |            </goals>
      |          </execution>
      |        </executions>
      |      </plugin>
      |    </plugins>
      |    <pluginManagement>
      |      <plugins>
      |        <plugin>
      |          <groupId>org.codehaus.mojo</groupId>
      |          <artifactId>aspectj-maven-plugin</artifactId>
      |          <version>${aspectj-maven-plugin.version}</version>
      |          <configuration>
      |            <showWeaveInfo>true</showWeaveInfo>
      |            <source>${java.version}</source>
      |            <target>${java.version}</target>
      |            <Xlint>ignore</Xlint>
      |            <complianceLevel>${java.version}</complianceLevel>
      |            <encoding>UTF-8</encoding>
      |            <verbose>false</verbose>
      |            <showWeaveInfo>false</showWeaveInfo>
      |            <aspectLibraries>
      |              <aspectLibrary>
      |                <groupId>org.springframework</groupId>
      |                <artifactId>spring-aspects</artifactId>
      |              </aspectLibrary>
      |            </aspectLibraries>
      |          </configuration>
      |          <executions>
      |            <execution>
      |              <goals>
      |                <goal>compile</goal>
      |                <goal>test-compile</goal>
      |              </goals>
      |            </execution>
      |          </executions>
      |          <dependencies>
      |            <dependency>
      |              <groupId>org.aspectj</groupId>
      |              <artifactId>aspectjrt</artifactId>
      |              <version>${aspectj.version}</version>
      |            </dependency>
      |            <dependency>
      |              <groupId>org.aspectj</groupId>
      |              <artifactId>aspectjtools</artifactId>
      |              <version>${aspectj.version}</version>
      |            </dependency>
      |          </dependencies>
      |        </plugin>
      |      </plugins>
      |    </pluginManagement>
      |  </build>
      |  <profiles>
      |    <profile>
      |      <id>integration-tests</id>
      |      <build>
      |        <plugins>
      |          <plugin>
      |            <artifactId>maven-surefire-plugin</artifactId>
      |            <configuration>
      |              <skip>true</skip>
      |            </configuration>
      |          </plugin>
      |          <plugin>
      |            <artifactId>maven-failsafe-plugin</artifactId>
      |            <version>2.19.1</version>
      |            <executions>
      |              <execution>
      |                <goals>
      |                  <goal>integration-test</goal>
      |                  <goal>verify</goal>
      |                </goals>
      |              </execution>
      |            </executions>
      |          </plugin>
      |        </plugins>
      |      </build>
      |    </profile>
      |  </profiles>
      |
      |</project>
      |""".stripMargin)
  val rootTree =
    """com.novomind.ishop.shops.anyshop:anyshop-projects:pom:27.0.0-SNAPSHOT
      @|  +- org.springframework.security:spring-security-config:jar:4.0.2.RELEASE:compile
      @|  |  \- aopalliance:aopalliance:jar:1.0:compile
      @|  +- org.springframework.mobile:spring-mobile-device:jar:1.1.3.RELEASE:compile
      @|  \- javax.inject:javax.inject:jar:1:compile
      @+- com.novomind.ishop.core:ishop-core-all-tests:pom:28.2.2:test
      @|  +- com.novomind.ishop.exi:ext-export:jar:tests:28.2.2:test
      @|  +- com.novomind.ishop.core:ishop-api:jar:tests:28.2.2:test
      @|  +- com.novomind.ishop.exi:ext-ishopsearch:jar:tests:28.2.2:test
      @|  +- com.novomind.ishop.exi:ishop-ext-searchsuggest:jar:tests:28.2.2:test
      @|  +- javax.servlet:javax.servlet-api:jar:3.0.1:provided
      @|  +- junit:junit:jar:4.12:test
      @|  |  \- org.hamcrest:hamcrest-core:jar:1.3:test
      @|  +- org.springframework:spring-test:jar:4.3.3.RELEASE:test
      @|  +- org.mockito:mockito-all:jar:1.10.8:test
      @|  +- org.eclipse.jgit:org.eclipse.jgit:jar:2.1.0.201209190230-r:test
      @|  \- org.hsqldb:hsqldb:jar:2.2.9:test
      @+- org.aspectj:aspectjrt:jar:1.8.8:provided
      @+- org.aspectj:aspectjtools:jar:1.8.8:compile
      @+- org.aspectj:aspectjweaver:jar:1.8.8:compile
      @\- com.google.code.findbugs:jsr305:jar:3.0.0:compile
      @""".stripMargin('@')
  @nowarn("msg=possible missing interpolator")
  val anyshopErpPom = Xpath.newDocument(
    """<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      |  <modelVersion>4.0.0</modelVersion>
      |
      |  <parent>
      |    <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |    <artifactId>anyshop-projects</artifactId>
      |    <version>27.0.0-SNAPSHOT</version>
      |    <relativePath>..</relativePath>
      |  </parent>
      |
      |  <artifactId>anyshop-erp</artifactId>
      |  <name>anyshop-erp</name>
      |
      |
      |  <build>
      |    <plugins>
      |
      |    </plugins>
      |  </build>
      |
      |  <dependencyManagement>
      |    <dependencies>
      |      <dependency>
      |        <groupId>com.novomind.ishop.core</groupId>
      |        <artifactId>ishop-core-projects</artifactId>
      |        <version>${ishop-core-sub.version}</version>
      |        <scope>import</scope>
      |        <type>pom</type>
      |      </dependency>
      |    </dependencies>
      |  </dependencyManagement>
      |
      |  <dependencies>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-commons</artifactId>
      |      <version>27.0.0-SNAPSHOT</version>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-commons</artifactId>
      |      <version>27.0.0-SNAPSHOT</version>
      |      <classifier>tests</classifier>
      |      <scope>test</scope>
      |    </dependency>
      |
      |    <dependency>
      |      <groupId>com.novomind.ishop.core</groupId>
      |      <artifactId>ishop-core-all</artifactId>
      |      <type>pom</type>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.core</groupId>
      |      <artifactId>ishop-core-all-tests</artifactId>
      |      <type>pom</type>
      |      <scope>test</scope>
      |    </dependency>
      |
      |  </dependencies>
      |
      |  <properties>
      |    <ishop-core-sub.version>27.1.2-SNAPSHOT</ishop-core-sub.version>
      |  </properties>
      |
      |</project>
      |""".stripMargin)
  val anyshopErpTree =
    """com.novomind.ishop.shops.anyshop:anyshop-erp:jar:27.0.0-SNAPSHOT
      @+- com.novomind.ishop.core:ishop-core-all-tests:pom:28.2.2:test
      @|  +- javax.servlet:javax.servlet-api:jar:3.0.1:provided
      @|  +- junit:junit:jar:4.12:test
      @|  |  \- org.hamcrest:hamcrest-core:jar:1.3:test
      @|  +- org.springframework:spring-test:jar:4.3.3.RELEASE:test
      @|  +- org.mockito:mockito-all:jar:1.10.8:test
      @|  +- org.eclipse.jgit:org.eclipse.jgit:jar:2.1.0.201209190230-r:test
      @|  \- org.hsqldb:hsqldb:jar:2.2.9:test
      @+- org.aspectj:aspectjrt:jar:1.8.8:provided
      @+- org.aspectj:aspectjtools:jar:1.8.8:compile
      @+- org.aspectj:aspectjweaver:jar:1.8.8:compile
      @\- com.google.code.findbugs:jsr305:jar:3.0.0:compile
      @""".stripMargin('@')

  @nowarn("msg=possible missing interpolator")
  val anyshopPom = Xpath.newDocument(
    """<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      |  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      |  <modelVersion>4.0.0</modelVersion>
      |
      |  <packaging>war</packaging>
      |
      |  <parent>
      |    <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |    <artifactId>anyshop-projects</artifactId>
      |    <version>27.0.0-SNAPSHOT</version>
      |    <relativePath>..</relativePath>
      |  </parent>
      |
      |  <groupId>com.novomind.ishop.shops</groupId>
      |  <artifactId>anyshop</artifactId>
      |  <name>anyshop-shop</name>
      |
      |  <build>
      |    <plugins>
      |      <plugin>
      |        <groupId>org.apache.maven.plugins</groupId>
      |        <artifactId>maven-compiler-plugin</artifactId>
      |      </plugin>
      |    </plugins>
      |  </build>
      |
      |  <profiles>
      |    <profile>
      |      <id>local</id>
      |      <activation>
      |        <os>
      |          <family>windows</family>
      |        </os>
      |      </activation>
      |    </profile>
      |    <profile>
      |      <id>linux</id>
      |      <activation>
      |        <os>
      |          <family>unix</family>
      |        </os>
      |        <file>
      |          <exists>rootContext</exists>
      |        </file>
      |      </activation>
      |    </profile>
      |  </profiles>
      |
      |  <dependencyManagement>
      |    <dependencies>
      |      <dependency>
      |        <groupId>com.novomind.ishop.core</groupId>
      |        <artifactId>ishop-core-projects</artifactId>
      |        <version>${ishop-core.version}</version>
      |        <scope>import</scope>
      |        <type>pom</type>
      |      </dependency>
      |    </dependencies>
      |  </dependencyManagement>
      |
      |  <dependencies>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-commons</artifactId>
      |      <version>27.0.0-SNAPSHOT</version>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-commons</artifactId>
      |      <version>27.0.0-SNAPSHOT</version>
      |      <classifier>tests</classifier>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-erp</artifactId>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-erp</artifactId>
      |      <version>27.0.0-SNAPSHOT</version>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-erp</artifactId>
      |      <version>27.0.0-SNAPSHOT</version>
      |      <classifier>tests</classifier>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops.anyshop</groupId>
      |      <artifactId>anyshop-erp</artifactId>
      |      <version>27.0.0-SNAPSHOT</version>
      |      <classifier>pom</classifier>
      |    </dependency>
      |
      |    <dependency>
      |      <groupId>com.novomind.ishop.core</groupId>
      |      <artifactId>ishop-core-all</artifactId>
      |      <type>pom</type>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.core</groupId>
      |      <artifactId>ishop-core-all-tests</artifactId>
      |      <type>pom</type>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.shops-ext</groupId>
      |      <artifactId>ishop-shops-ext-shop-base</artifactId>
      |    </dependency>
      |
      |    <dependency>
      |      <groupId>com.novomind.ishop.backoffice</groupId>
      |      <artifactId>bo-services</artifactId>
      |      <version>${ishop-core.version}</version>
      |    </dependency>
      |    <dependency>
      |      <groupId>com.novomind.ishop.backoffice</groupId>
      |      <artifactId>bo-core</artifactId>
      |      <version>${ishop-core.version}</version>
      |    </dependency>
      |
      |    <dependency>
      |      <groupId>javax.servlet.jsp</groupId>
      |      <artifactId>jsp-api</artifactId>
      |      <version>2.2</version>
      |      <scope>provided</scope>
      |    </dependency>
      |
      |    <dependency>
      |      <groupId>javax.el</groupId>
      |      <artifactId>javax.el-api</artifactId>
      |      <version>3.0.0</version>
      |      <scope>test</scope>
      |    </dependency>
      |
      |  </dependencies>
      |
      |  <properties>
      |    <ishop-plugin.version>27.3.0-SNAPSHOT</ishop-plugin.version>
      |    <bo-client>27.0.0-SNAPSHOT</bo-client>
      |    <allowedProjectDir>${project.basedir}/any</allowedProjectDir>
      |  </properties>
      |</project>
      |""".stripMargin)

  val anyshopTree =
    """com.novomind.ishop.shops:anyshop:war:27.0.0-SNAPSHOT
      @+- javax.servlet.jsp:jsp-api:jar:2.1:provided
      @+- de.jollyday:jollyday:jar:0.4.8:compile
      @|  +- joda-time:joda-time:jar:2.5:compile
      @|  \- javax.xml.bind:jaxb-api:jar:2.2.12:compile
      @+- com.jcraft:jsch:jar:0.1.51:compile
      @+- com.novomind.ishop.shops.anyshop:anyshop-db-migration:jar:27.0.0-SNAPSHOT:compile
      @|  +- com.novomind.ishop.backoffice:bo-resources:jar:28.2.2:compile
      @|  |  +- com.novomind.ishop.exi:ishop-ext-authentication:jar:1.0:compile
      @|  \- mysql:mysql-connector-java:jar:5.1.25:compile
      @+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:tests:27.0.0-SNAPSHOT:test
      @+- com.novomind.ishop.shops.anyshop:anyshop-erp:jar:27.0.0-SNAPSHOT:compile
      @|  +- org.apache.cxf:cxf-rt-frontend-jaxws:jar:3.1.3:compile
      @|  |  \- org.apache.wss4j:wss4j-ws-security-policy-stax:jar:2.1.3:compile
      @|  \- org.jvnet.jaxb2_commons:jaxb2-basics-runtime:jar:0.9.5:compile
      @+- com.novomind.ishop.clients:clients-otto:jar:28.0.2:compile
      @|  +- com.novomind.ishop.shops-ext:ishop-shops-ext-shop-base:jar:28.2.0:compile
      @|  +- com.novomind.ishop.exi:ishop-ext-picalike:jar:28.0.0:compile
      @|  +- net.sf.json-lib:json-lib:jar:jdk15:2.4:compile
      @|  |  \- net.sf.ezmorph:ezmorph:jar:1.0.6:compile
      @|  +- org.springframework:spring-jms:jar:4.0.4.RELEASE:compile
      @|  \- org.glassfish.jaxb:jaxb-runtime:jar:2.2.11:compile
      @+- com.novomind.ishop.exi:ishop-ext-noa:jar:28.0.0:compile
      @|  +- org.springframework.ws:spring-ws-core:jar:2.2.3.RELEASE:compile
      @|  |  \- org.springframework.ws:spring-xml:jar:2.2.3.RELEASE:compile
      @|  \- org.springframework:spring-oxm:jar:4.3.3.RELEASE:compile
      @+- com.google.code.gson:gson:jar:2.6.2:compile
      @+- org.apache.httpcomponents:fluent-hc:jar:4.5.2:test
      @+- org.apache.commons:commons-lang3:jar:3.3.2:compile
      @+- commons-codec:commons-codec:jar:1.10:compile
      @+- org.freemarker:freemarker:jar:2.3.23:compile
      @+- net.logstash.logback:logstash-logback-encoder:jar:4.6:compile
      @|  +- ch.qos.logback:logback-core:jar:1.1.2:compile
      @|  \- com.fasterxml.jackson.core:jackson-databind:jar:2.7.4:compile
      @|     \- com.fasterxml.jackson.core:jackson-annotations:jar:2.7.4:compile
      @+- org.aspectj:aspectjrt:jar:1.8.8:provided
      @+- org.aspectj:aspectjtools:jar:1.8.8:compile
      @+- org.aspectj:aspectjweaver:jar:1.8.8:compile
      @\- com.google.code.findbugs:jsr305:jar:3.0.0:compile
      @""".stripMargin('@')
}
