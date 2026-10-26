package release

import org.junit.{Assert, Rule, Test}
import org.junit.rules.TemporaryFolder

import java.io.FileOutputStream
import java.util.jar.{Attributes, JarOutputStream, Manifest}
import scala.util.Using

class StarterBuildCommitTest {
  val temp = new TemporaryFolder()
  @Rule def temporaryFolder: TemporaryFolder = temp

  @Test
  def readsCommitFromJarManifest(): Unit = {
    Seq("0123456789abcdef0123456789abcdef01234567", "a" * 64).foreach { head =>
      val file = temp.newFile()
      val manifest = new Manifest()
      manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      manifest.getMainAttributes.putValue("Git-Commit", head)
      Using.resource(new JarOutputStream(new FileOutputStream(file), manifest))(_ => ())
      Assert.assertEquals(Some(head), Starter.buildCommit(file))
    }
  }

  @Test
  def rejectsMissingOrInvalidCommit(): Unit = {
    Seq(None, Some(""), Some("not-a-commit"), Some("a" * 39)).foreach { head =>
      val file = temp.newFile()
      val manifest = new Manifest()
      manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      head.foreach(value => manifest.getMainAttributes.putValue("Git-Commit", value))
      Using.resource(new JarOutputStream(new FileOutputStream(file), manifest))(_ => ())
      Assert.assertEquals(None, Starter.buildCommit(file))
    }
    Assert.assertEquals(None, Starter.buildCommit(temp.newFile()))
    Assert.assertEquals(None, Starter.buildCommit(temp.getRoot))
  }
}
