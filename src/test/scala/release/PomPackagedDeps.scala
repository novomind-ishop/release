package release

import release.PomMod.{Dep, PomRef}

object PomPackagedDeps {
  def selfVersion(version:String): Seq[Dep] = {
    Seq(
      Dep(PomRef("com.novomind.ishop:ishop-meta-parent:" + version), "com.novomind.ishop", "ishop-meta-parent", version, "", "", "pom"),
      Dep(PomRef("meta"),"com.novomind.ishop","meta","29.0.0-SNAPSHOT","","", "pom"),
      Dep(PomRef("com.novomind.ishop.core:ishop-core-parent"), "com.novomind.ishop.core", "ishop-core-parent", version, "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:ishop-shop-parent"), "com.novomind.ishop.shops", "ishop-shop-parent", version, "", "", "pom")
)
  }

  def self(): Seq[Dep] = selfVersion("29.0.0-SNAPSHOT")
}
