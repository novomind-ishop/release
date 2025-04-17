package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Gav3, SelfRef}
import release.VersionSkew.SkewResult

class VersionSkewTest extends AssertionsForJUnit {

  @Test
  def testCoreMajorResult(): Unit = {
    val result = VersionSkew.skewResultOf(relevantDeps = Nil, isNoShop = true, release = None)
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Nil, "", Nil), result)
  }

  @Test
  def testCoreMajorResult_Release(): Unit = {
    val result = VersionSkew.skewResultOf(relevantDeps = Nil, isNoShop = true, release = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Nil, "", Nil), result)
  }

  @Test
  def testCoreMajorResult_Release_deps(): Unit = {
    val deps = Seq(Gav3("g", "a", Some("a")).toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOf(relevantDeps = deps, isNoShop = true, release = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Nil, "1", Nil), result)
  }

  @Test
  def testCoreMajorResult_Release_deps_2(): Unit = {
    val deps = Seq(
      Gav3("g", "a", Some("1.2.3")),
      Gav3("g", "aa", Some("1.2.3")),
    ).map(_.toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOf(relevantDeps = deps, isNoShop = true, release = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Seq("1"), "1", Seq(
      ("1", deps(0)),
      ("1", deps(1)),
    )), result)
  }

  @Test
  def testCoreMajorResult_Release_deps_diff(): Unit = {
    val deps = Seq(
      Gav3("g", "a", Some("1.2.3")),
      Gav3("g", "aa", Some("2.2.3")),
      Gav3("g", "aaa", None),
    ).map(_.toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOf(relevantDeps = deps, isNoShop = true, release = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = true, Seq("1", "2"), "1", Seq(
      ("1", deps(0)),
      ("2", deps(1)),
    )), result)
  }

  @Test
  def testCoreMajorResult_Release_deps_diff_noShop(): Unit = {
    val deps = Seq(
      Gav3("g", "a", Some("1.2.3")),
      Gav3("g", "aa", Some("2.2.3")),
      Gav3("g", "aaa", None),
    ).map(_.toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOf(relevantDeps = deps, isNoShop = false, release = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = true, Seq("1", "2"), "2", Seq(
      ("1", deps(0)),
      ("2", deps(1)),
    )), result)
  }

}
