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
    testResourcesRoot().resolve(folderName).toFile
  }

  def assertException[T <: Exception](f: () ⇒ Unit, message: String, clazz: Class[T]): Unit = {
    try {
      f.apply()
      Assert.fail()
    } catch {
      case e if e.isInstanceOf[T] ⇒ {
        // TODO T ist hier weg, anders prüfen ob die klasse passt
        try {
          Assert.assertEquals(message, e.getMessage)
        } catch {
          case t: Throwable ⇒ e.printStackTrace(); throw t
        }
      }
      case e: Throwable ⇒ throw e
    }
  }

  private lazy val git = SgitTest.workSgit()

  def gitDiff(): Seq[String] = git.diff()
}
