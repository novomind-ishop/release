package release

import org.scalatestplus.junit.AssertionsForJUnit
import com.google.googlejavaformat.java.Formatter
import org.eclipse.aether.repository.RemoteRepository
import org.junit.{Assert, Ignore, Rule, Test}
import org.junit.rules.TemporaryFolder
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatestplus.junit.AssertionsForJUnit
import release.Lint.BranchTagMerge
import release.Repo.ReachableResult
import release.Starter.{LintOpts, Opts}

class LintTest extends AssertionsForJUnit {
  val _temporarayFolder = new TemporaryFolder()

  @Rule def temp = _temporarayFolder

  @Test
  def testVersionMatches(): Unit = {
    Assert.assertTrue(Lint.noVersionMatches("", Some(BranchTagMerge(tagName = None, branchName = None))))
  }

  @Test
  def testGoogleFmt(): Unit = {
    val file = temp.newFile("Demo.java")
    Util.write(file,
      """
        |public class Demo {
        |
        |}
        |""".stripMargin)
    val tuple = Lint.doGoogleFmt(new Formatter(), file)
    println(tuple)
    if (tuple._1.isFailure) {
      tuple._1.get
    }
  }
}
