package release

import release.ProjectMod.{Dep, SelfRef}

object PomPackagedDeps {
  def selfVersion(_version: String): Seq[Dep] = {
    val version = Some(_version)
    Seq(
      Dep(SelfRef.parse("com.novomind.ishop:ishop-meta-parent:" + version.get), "com.novomind.ishop", "ishop-meta-parent", version, "", "", "pom", ""),
      Dep(SelfRef.parse("com.novomind.ishop:meta:" + version.get), "com.novomind.ishop", "meta", Some("29.0.0-SNAPSHOT"), "", "", "pom", ""),
      Dep(SelfRef.parse("com.novomind.ishop.core:ishop-core-parent:" + version.get), "com.novomind.ishop.core", "ishop-core-parent", version, "", "", "", ""),
      Dep(SelfRef.parse("com.novomind.ishop.shops:ishop-shop-parent:" + version.get), "com.novomind.ishop.shops", "ishop-shop-parent", version, "", "", "pom", "")
    )
  }

  def self(): Seq[Dep] = selfVersion("29.0.0-SNAPSHOT")
}
