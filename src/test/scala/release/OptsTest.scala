package release

import com.typesafe.scalalogging.LazyLogging
import org.junit.{Assert, Test}
import org.scalatestplus.junit.AssertionsForJUnit

class OptsTest extends AssertionsForJUnit with LazyLogging {
  def assertArgs(expected: Opts, current: Opts): Unit = {
    Assert.assertEquals(Util.show(expected), Util.show(current))
    Assert.assertEquals(expected, current)
  }
  @Test
  def testEnvRead_empty(): Unit = {
    val result = Opts.envRead(Nil, Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_some(): Unit = {
    val result = Opts.envRead(Seq(("a", "b")), Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_merge(): Unit = {
    val result = Opts.envRead(Seq(("a", "b")), Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_gerrit(): Unit = {
    val result = Opts.envRead(Seq(("RELEASE_NO_GERRIT", "true")), Opts())
    Assert.assertEquals(Opts(useGerrit = false), result)
  }

  @Test
  def testEnvRead_gerrit_false(): Unit = {
    val result = Opts.envRead(Seq(("RELEASE_NO_GERRIT", "false")), Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_gerrit_defect(): Unit = {
    val result = Opts.envRead(Seq(("RELEASE_NO_GERRIT", "fw")), Opts())
    Assert.assertEquals(Opts(), result)
  }

  @Test
  def testEnvRead_gerrit_others(): Unit = {
    val result = Opts.envRead(Seq(("a", "b"), ("RELEASE_NO_GERRIT", "true")), Opts())
    Assert.assertEquals(Opts(useGerrit = false), result)
  }

  @Test
  def testEnvRead_showTimeStamps(): Unit = {
    val result = Opts.envRead(Seq(("RELEASE_LINT_TIMESTAMPS", "true")), Opts())
    Assert.assertEquals(Opts(lintOpts = LintOpts(showTimeStamps = true)), result)
  }

  @Test
  def testEnvRead_showTimeStamps_fail(): Unit = {
    val result = Opts.envRead(Seq(("RELEASE_LINT_TIMESTAMPS", "some")), Opts())
    Assert.assertEquals(Opts(lintOpts = LintOpts(showTimeStamps = false)), result)
  }

  @Test
  def testEnvRead_checkPackages(): Unit = {
    val inOpts = Opts()
    Assert.assertEquals(Opts(lintOpts = LintOpts(checkPackages = true)), inOpts)
    val result = Opts.envRead(Seq(("RELEASE_LINT_CHECKPACKAGES", "false")), inOpts)
    Assert.assertEquals(Opts(lintOpts = LintOpts(checkPackages = false)), result)
  }

  @Test
  def testEnvRead_release_lint(): Unit = {
    val tuples = Seq(
      ("RELEASE_ANY0", "a"),
      ("RELEASE_LINT_SKIP", "a"),
      ("RELEASE_ANY1", "a"),
    )
    val result = Opts.envRead(tuples, Opts())
    Assert.assertEquals(Opts(lintOpts = LintOpts(skips = Seq("a"))), result)
  }


  @Test
  def testArgRead_none(): Unit = {
    Assert.assertEquals(Opts(), Opts.argsAndEnvRead(Nil, Opts(), Map.empty))
  }

  @Test
  def testArgRead_invalids(): Unit = {
    Assert.assertEquals(Opts().focusInvalids().replace(Seq("--bert")), Opts.argsAndEnvRead(Seq("--bert"), Opts(), Map.empty))
    Assert.assertEquals(Opts(showHelp = true, diag = OptsDiag(invalids = Seq("some"))), Opts.argsAndEnvRead(Seq("--help", "some"), Opts(), Map.empty))
    Assert.assertEquals(Opts(showHelp = true, diag = OptsDiag(invalids = Seq("some"))), Opts.argsAndEnvRead(Seq("some", "--help"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_simpleChars(): Unit = {
    Assert.assertEquals(Opts(simpleChars = true), Opts.argsAndEnvRead(Seq("--simple-chars"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_help(): Unit = {
    Assert.assertEquals(Opts(showHelp = true), Opts.argsAndEnvRead(Seq("-h"), Opts(), Map.empty))
    Assert.assertEquals(Opts(showHelp = true), Opts.argsAndEnvRead(Seq("--help"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_updateCmd(): Unit = {
    Assert.assertEquals(Opts(showUpdateCmd = true, showStartupDone = false), Opts.argsAndEnvRead(Seq("--show-update-cmd"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_noGerrit(): Unit = {
    Assert.assertEquals(Opts(useGerrit = false), Opts.argsAndEnvRead(Seq("--no-gerrit"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_noGerrit_skip_property(): Unit = {
    Assert.assertEquals(Opts(useGerrit = false, skipProperties = Seq("a", "b")),
      Opts.argsAndEnvRead(Seq("--no-gerrit", "--skip-property", "a", "--skip-property", "b"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_skip_env(): Unit = {
    Assert.assertEquals(Opts(lintOpts = LintOpts(skips = Seq("a"))),
      Opts.argsAndEnvRead(Seq(), Opts(), envs = Map(
        "RELEASE_ANY0" -> "a",
        "RELEASE_LINT_SKIP" -> "a",
        "RELEASE_ANY1" -> "a",
      )))
  }

  @Test
  def testArgRead_noGerrit_env(): Unit = {
    val read = Opts.argsAndEnvRead(Seq("--skip-property", "a", "--skip-property", "b"), Opts(), Map("RELEASE_NO_GERRIT" -> "true"))
    val expected = Opts(useGerrit = false, skipProperties = Seq("a", "b"))
    Assert.assertEquals(Util.show(expected), Util.show(read))
    Assert.assertEquals(expected, read)
  }

  @Test
  def testArgRead_noGerrit_env_false(): Unit = {
    Assert.assertEquals(Opts(skipProperties = Seq("a", "b")),
      Opts.argsAndEnvRead(Seq("--skip-property", "a", "--skip-property", "b"), Opts(),
        Map(
          "RELEASE_NO_GERRIT" -> "false",
        )))
  }

  @Test
  def testArgRead_noJline(): Unit = {
    Assert.assertEquals(Opts(useJlineInput = false), Opts.argsAndEnvRead(Seq("--no-jline"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_replace(): Unit = {
    Assert.assertEquals(Opts(), Opts.argsAndEnvRead(Seq("--replace"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_noUpdate(): Unit = {
    Assert.assertEquals(Opts(doUpdate = false), Opts.argsAndEnvRead(Seq("--no-update"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_versionSet(): Unit = {
    Assert.assertEquals(Opts().focusInvalids().replace(Seq("versionSet")), Opts.argsAndEnvRead(Seq("versionSet"), Opts(), Map.empty))
    Assert.assertEquals(Opts(versionSet = Some("3")), Opts.argsAndEnvRead(Seq("versionSet", "3"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_shopGaSet(): Unit = {

    Assert.assertEquals(Opts().focusInvalids().replace(Seq("shopGASet")), Opts.argsAndEnvRead(Seq("shopGASet"), Opts(), Map.empty))
    Assert.assertEquals(Opts(shopGA = Some("some")), Opts.argsAndEnvRead(Seq("shopGASet", "some"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_apidiff(): Unit = {
    assertArgs(Opts(apiDiff = OptsApidiff(showApiDiff = true)),
      Opts.argsAndEnvRead(Seq("apidiff"), Opts(), Map.empty))
    assertArgs(Opts(apiDiff = OptsApidiff(showApiDiff = true, left = "a", right = "b")),
      Opts.argsAndEnvRead(Seq("apidiff", "a", "b"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_lint(): Unit = {
    assertArgs(Opts(lintOpts = LintOpts(doLint = true), showStartupDone = false),
      Opts.argsAndEnvRead(Seq("lint"), Opts(), Map.empty))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, showHelp = true), showStartupDone = false),
      Opts.argsAndEnvRead(Seq("lint", "--help"), Opts(), Map.empty))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, showHelp = true), showStartupDone = false),
      Opts.argsAndEnvRead(Seq("lint", "-h"), Opts(), Map.empty))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, skips = Seq("c", "D", "RL1012-5a4ee54d", "a")), showStartupDone = false),
      Opts.argsAndEnvRead(Seq("lint", "--skip-RL1012-5a4ee54d", "--skip-a"), Opts(), envs = Map(
        "RELEASE_LINT_SKIP" -> "c,D,, ,c,,,",
      )))
    assertArgs(Opts(lintOpts = LintOpts(doLint = true, waringsToErrors = true), showStartupDone = false),
      Opts.argsAndEnvRead(Seq("lint", "--strict"), Opts(), envs = Map(
        "RELEASE_LINT_SKIP" -> null,
      )))
  }

  @Test
  def testArgRead_createFeature(): Unit = {
    Assert.assertEquals(Opts(createFeature = true), Opts.argsAndEnvRead(Seq("nothing-but-create-feature-branch"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_showDependencyUpdates(): Unit = {
    Assert.assertEquals(Opts(depUpOpts = OptsDepUp().copy(showDependencyUpdates = true)),
      Opts.argsAndEnvRead(Seq("showDependencyUpdates"), Opts(), Map.empty))
  }

  @Test
  def testArgRead_mixed(): Unit = {
    val opts = Opts(depUpOpts = OptsDepUp().copy(showDependencyUpdates = true, showHelp = true), useGerrit = false)
    Assert.assertEquals(opts, Opts.argsAndEnvRead(Seq("--no-gerrit", "showDependencyUpdates", "--help", "", " ", "\t"), Opts(), Map.empty))
  }

}
