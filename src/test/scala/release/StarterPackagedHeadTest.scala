package release

import org.junit.{Assert, Rule, Test}
import org.junit.rules.TemporaryFolder

import java.io.File

class StarterPackagedHeadTest {
  val temp = new TemporaryFolder()
  @Rule def temporaryFolder: TemporaryFolder = temp

  private val packagedSha = "0123456789abcdef0123456789abcdef01234567"

  @Test
  def packagedImageReadsHeadWithoutOpeningGit(): Unit = {
    val dir = temp.newFolder()
    FileUtils.write(new File(dir, "git.HEAD"), packagedSha)
    Assert.assertEquals(packagedSha,
      Starter.releaseToolHeadStatus(dir,
        throw new AssertionError("Packaged release must not open a Git repository")))
  }

  @Test
  def sourceCheckoutPreservesWorkingTreeStatus(): Unit = {
    val dir = temp.newFolder()
    Assert.assertTrue(new File(dir, ".git").mkdir())
    FileUtils.write(new File(dir, "git.HEAD"), packagedSha)
    Assert.assertEquals("checkout-sha*", Starter.releaseToolHeadStatus(dir, "checkout-sha*"))
  }

  @Test
  def worktreeUsesGitInsteadOfPackagedMetadata(): Unit = {
    val dir = temp.newFolder()
    FileUtils.write(new File(dir, ".git"), "gitdir: /unused/worktree")
    FileUtils.write(new File(dir, "git.HEAD"), packagedSha)
    Assert.assertEquals("worktree-sha", Starter.releaseToolHeadStatus(dir, "worktree-sha"))
  }

  @Test
  def missingPackagedMetadataPreservesGitFailure(): Unit = {
    val dir = temp.newFolder()
    Assert.assertThrows(classOf[Sgit.MissingGitDirException],
      () => {
        Starter.releaseToolHeadStatus(dir, throw new Sgit.MissingGitDirException("missing Git"))
        ()
      })
  }

  @Test
  def invalidPackagedMetadataIsRejected(): Unit = {
    val dir = temp.newFolder()
    FileUtils.write(new File(dir, "git.HEAD"), "not-a-commit")
    Assert.assertThrows(classOf[IllegalArgumentException],
      () => {
        Starter.releaseToolHeadStatus(dir, throw new AssertionError("Must not open Git"))
        ()
      })
  }
}
