package release

import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit
import release.ProjectMod.{Gav3, SelfRef}
import release.VersionSkew.SkewResult

import java.util.concurrent.atomic.AtomicBoolean

class VersionSkewTest extends AssertionsForJUnit {

  @Test
  def testCoreMajorResult(): Unit = {
    val result = VersionSkew.skewResultOfLayer(relevantDeps = Nil, isNoShop = true, releaseVersion = None)
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Nil, "", Nil, Nil), result)
  }

  @Test
  def testCoreMajorResult_Release(): Unit = {
    val result = VersionSkew.skewResultOfLayer(relevantDeps = Nil, isNoShop = true, releaseVersion = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Nil, "", Nil, Nil), result)
  }

  @Test
  def testCoreMajorResult_Release_deps(): Unit = {
    val deps = Seq(Gav3("g", "a", Some("a")).toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOfLayer(relevantDeps = deps, isNoShop = true, releaseVersion = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Nil, "1", Nil, Nil), result)
  }

  @Test
  def testCoreMajorResult_Release_deps_2(): Unit = {
    val deps = Seq(
      Gav3("g", "a", Some("1.2.3")),
      Gav3("g", "aa", Some("1.2.3")),
    ).map(_.toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOfLayer(relevantDeps = deps, isNoShop = true, releaseVersion = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = false, Seq("1"), "1", Seq(
      ("1", deps(0)),
      ("1", deps(1)),
    ), Nil), result)
  }

  @Test
  def testCoreMajorResult_Release_deps_diff(): Unit = {
    val deps = Seq(
      Gav3("g", "a", Some("1.2.3")),
      Gav3("g", "aa", Some("2.2.3")),
      Gav3("g", "aaa", None),
    ).map(_.toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOfLayer(relevantDeps = deps, isNoShop = true, releaseVersion = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = true, Seq("1", "2"), "1", Seq(
      ("1", deps(0)),
      ("2", deps(1)),
    ), Nil), result)
  }

  @Test
  def testCoreMajorResult_Release_deps_diff_noShop(): Unit = {
    val deps = Seq(
      Gav3("g", "a", Some("1.2.3")),
      Gav3("g", "aa", Some("2.2.3")),
      Gav3("g", "aaa", None),
    ).map(_.toDep(SelfRef.undef))
    val result = VersionSkew.skewResultOfLayer(relevantDeps = deps, isNoShop = false, releaseVersion = Some("1.2.3"))
    Assert.assertEquals(SkewResult(hasDifferentMajors = true, Seq("1", "2"), "2", Seq(
      ("1", deps(0)),
      ("2", deps(1)),
    ), Nil), result)
  }

  @Test
  def testWithOut(): Unit = {
    val wx = Some(new AtomicBooleanFlip())
    val term = TermTest.withOutErr[Unit]()(sys => {
      val deps = Seq(
        Gav3("g", "some-rele-a", Some("1.2.3")),
        Gav3("g", "some-rele-b", Some("1.2.4")),
        Gav3("g", "aa", Some("2.2.3")),
        Gav3("g", "aaa", None),
      ).map(_.toDep(SelfRef.undef))
      val sk = VersionSkew.innerSkewResult(None, warnExit = wx, out = Some(sys.out),
        Opts().copy(colors = false, lintOpts = Opts().lintOpts.copy(skips = Seq("RL1013-2a36fc66"))), isNoShop = false, deps).usedLintSkips
      Assert.assertEquals(Seq("RL1013-2a36fc66"), sk)
    })
    Assert.assertEquals(
      """[WARNING]     Found multiple core major version: »1, 2«, use only one 😬 RL1013-2b9077d7
        |[WARNING]       - 1 -
        |[WARNING]       g:some-rele-a:1.2.3 😬 RL1013-ee567cda
        |[INFO]          g:some-rele-b:1.2.4 🤐 RL1013-2a36fc66
        |[WARNING]       - 2 -
        |[WARNING]       g:aa:2.2.3 😬 RL1013-7e9bf46f""".stripMargin, term.out)
    Assert.assertTrue(wx.get.get())
  }

  @Test
  def testWithOut_skipAll(): Unit = {
    val wx = Some(new AtomicBooleanFlip())
    val term = TermTest.withOutErr[Unit]()(sys => {
      val deps = Seq(
        Gav3("g", "some-rele-a", Some("1.2.3")),
        Gav3("g", "some-rele-b", Some("1.2.4")),
        Gav3("g", "aa", Some("2.2.3")),
        Gav3("g", "aaa", None),
      ).map(_.toDep(SelfRef.undef))
      val sk = VersionSkew.innerSkewResult(None, warnExit = wx, out = Some(sys.out),
        Opts().copy(colors = false, lintOpts = Opts().lintOpts.copy(skips = Seq("RL1013-2b9077d7"))), isNoShop = false, deps).usedLintSkips
      Assert.assertEquals(Seq("RL1013-2b9077d7"), sk)
    })
    Assert.assertEquals(
      """[INFO]        Found multiple core major version: »1, 2«, use only one 🤐 RL1013-2b9077d7
        |[INFO]          - 1 -
        |[INFO]          g:some-rele-a:1.2.3 🤐 RL1013-ee567cda
        |[INFO]          g:some-rele-b:1.2.4 🤐 RL1013-2a36fc66
        |[INFO]          - 2 -
        |[INFO]          g:aa:2.2.3 🤐 RL1013-7e9bf46f""".stripMargin, term.out)
    Assert.assertFalse(wx.get.get())
  }

}
