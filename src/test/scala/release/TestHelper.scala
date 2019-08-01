package release

import java.io.File
import java.nio.file.{Path, Paths}

import org.junit.{Assert, ComparisonFailure}

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

  def assertComparisonFailure(expectedMsg: String, fn: () => Unit): Unit = {
    assertAssertionError(expectedMsg, classOf[ComparisonFailure], fn)
  }

  def assertAssertionError[T <: AssertionError](expectedMsg: String, clazz: Class[T], fn: () => Unit): Unit = {
    try {
      fn.apply()
      Assert.fail("missing exception of type: " + clazz.getCanonicalName)
    } catch {
      case e: AssertionError => {
        try {
          Assert.assertEquals(expectedMsg, e.getMessage)
          Assert.assertEquals(clazz.getCanonicalName, e.getClass.getCanonicalName)
          Assert.assertEquals(clazz, e.getClass)
        } catch {
          case t: Throwable => e.printStackTrace(); throw t
        }
      }
      case e: Throwable => throw e
    }
  }

  def assertExceptionWithCheck[T <: Exception](expection: String => Unit, clazz: Class[T], fn: () => Unit): Unit = {
    try {
      fn.apply()
      Assert.fail("missing exception of type: " + clazz.getCanonicalName)
    } catch {
      case ae: AssertionError => throw ae
      case e: Throwable => {
        try {
          expection.apply(e.getMessage)
          Assert.assertEquals(clazz, e.getClass)
        } catch {
          case t: Throwable => throw t
        }
      }
    }
  }

  def assertException[T <: Exception](expectedMsg: String, clazz: Class[T], fn: () => Unit): Unit = {
    assertExceptionWithCheck(message => Assert.assertEquals(expectedMsg, message), clazz, fn)
  }

  def assertException[T <: Exception](clazz: Class[T], fn: () => Unit): Unit = {
    assertExceptionWithCheck(_: String => Unit, clazz, fn)
  }

  private lazy val git = SgitTest.workSgit()

  def localChanges(): Seq[String] = git.localChanges()
}
