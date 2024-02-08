package release

import release.ProjectMod.{Dep, SelfRef}

object PomPackagedDeps {
  def selfVersion(_version: String): Seq[Dep] = {
    val version = Some(_version)
    Seq(
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop:ishop-meta-parent:" + version.get),
        groupId = "com.novomind.ishop", artifactId = "ishop-meta-parent", version = version, packaging = "pom",
        pomPath = Seq("project", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop:meta:" + version.get),
        groupId = "com.novomind.ishop", artifactId = "meta", version = Some("29.0.0-SNAPSHOT"), packaging = "pom",
        pomPath = Seq("project", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.core:ishop-core-parent:" + version.get),
        groupId = "com.novomind.ishop.core", artifactId = "ishop-core-parent", version = version,
        pomPath = Seq("project", "artifactId")),
      ProjectModTest.depOf(pomRef = ProjectModTest.parseSelfRef("com.novomind.ishop.shops:ishop-shop-parent:" + version.get),
        groupId = "com.novomind.ishop.shops", artifactId = "ishop-shop-parent", version = version, packaging = "pom",
        pomPath = Seq("project", "artifactId"))
    )
  }

  def self(): Seq[Dep] = selfVersion("29.0.0-SNAPSHOT")
}
