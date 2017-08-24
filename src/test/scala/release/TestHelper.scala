package release

import java.io.File
import java.nio.file.{Path, Paths}

import org.junit.Assert

object TestHelper {

  private def findRoot(here: File = new File(".")): File = {
    val hereAbs = here.getAbsoluteFile
    if (new File(hereAbs, ".git").isDirectory) {
      here
    } else {
      findRoot(hereAbs.getParentFile)
    }
  }

  def testResourcesRoot(): Path = {
    val projectRoot = findRoot()
    val result = projectRoot.toPath.resolve(Paths.get("src", "test", "resources"))
    if (!result.toFile.isDirectory) {
      throw new IllegalStateException(result.toString)
    } else {
      result
    }
  }

  def testResources(folderName: String): File = {
    testResourcesRoot().resolve(folderName).toFile.getAbsoluteFile
  }

  def testFail[E >: Exception](expectedMsg: String, e: E, fn: () ⇒ Unit): Unit = {
    try {
      fn.apply()
      Assert.fail("no exception was thrown")
    } catch {
      case e: Exception if e.isInstanceOf[E] ⇒ Assert.assertEquals(expectedMsg, e.getMessage)
      case e: Exception ⇒ Assert.fail(e.getMessage)
    }
  }

  def assertException[T <: Exception](f: () ⇒ Unit, message: String, clazz: Class[T]): Unit = {
    try {
      f.apply()
      Assert.fail("missing exception of type: " + clazz.getCanonicalName)
    } catch {
      case ae: AssertionError ⇒ throw ae
      case e: Throwable ⇒ {
        try {
          Assert.assertEquals(message, e.getMessage)
          Assert.assertEquals(clazz, e.getClass)
        } catch {
          case t: Throwable ⇒ e.printStackTrace(); throw t
        }
      }
      case e: Throwable ⇒ throw e
    }
  }

  private lazy val git = SgitTest.workSgit()

  def localChanges(): Seq[String] = git.localChanges()
}
