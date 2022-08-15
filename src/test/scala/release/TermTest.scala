package release

import com.typesafe.scalalogging.LazyLogging
import org.junit._
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

import java.io._
import java.security.Permission

class TermTest extends AssertionsForJUnit {

  @Test
  def testRemoveSnapshot_0(): Unit = {
    val out = Term.removeTrailingSnapshots("RC-2009.01")

    Assert.assertEquals("RC-2009.01", out)
  }

  @Test
  def testRemoveSnapshot_1(): Unit = {
    val out = Term.removeTrailingSnapshots("0.1.1-SNAPSHOT")

    Assert.assertEquals("0.1.1", out)
  }

  @Test
  def testRemoveSnapshot_2(): Unit = {
    val out = Term.removeTrailingSnapshots("hallo-SNAPSHOT-SNAPSHOT")

    Assert.assertEquals("hallo", out)
  }

  @Test
  def testRemoveSnapshot_3(): Unit = {
    val out = Term.removeTrailingSnapshots("-SNAPSHOT-hallo-SNAPSHOT-SNAPSHOT")

    Assert.assertEquals("-SNAPSHOT-hallo", out)
  }

  @Test
  def testReadFrom(): Unit = {
    val value = "My string"

    TermTest.testSys(Seq(value), "enter some [word]: ", Nil)((in, out, err) => {
      val result = Term.readFrom(new PrintStream(out), "enter some", "word", Opts(useJlineInput = false), in)
      Assert.assertEquals(value, result)
    })

  }

  @Test
  def testReadNull(): Unit = {
    TermTest.testSys(Nil, "enter some [word]: ", Nil, 14)((in, out, err) => {
      Term.readFrom(new PrintStream(out), "enter some", "word", Opts(useJlineInput = false), in)
    })
  }

  @Test
  def testReadDirect(): Unit = {
    TermTest.testSys(Seq("a", "b"), "", Nil)((in, _, _) => {
      val bin = new BufferedReader(new InputStreamReader(in))
      Assert.assertEquals("a", bin.readLine())
      Assert.assertEquals("b", bin.readLine())
      Assert.assertEquals(null, bin.readLine())
    })
  }

  @Test
  def testThrows(): Unit = {
    TestHelper.assertComparisonFailure("expected:<[a]> but was:<[b]>", () => {
      TermTest.testSys(Nil, "", Nil)((_, _, _) => {
        Assert.assertEquals("a", "b")
      })
    })
  }

  @Test
  def testThrowsAll(): Unit = {
    TestHelper.assertException("hello", classOf[Exception], () => {
      TermTest.testSys(Nil, "", Nil)((_, _, _) => {
        throw new Exception("hello")
      })
    })

  }

}

object TermTest extends LazyLogging {

  def testSys(input: Seq[String], expectedOut: String, expectedErr: Seq[String], expectedExitCode: Int = 0,
              outFn:String => String = a => a)
             (fn: (InputStream, OutputStream, OutputStream) => Unit): Unit = {
    this.synchronized {
      val oldSecurityManager = System.getSecurityManager
      var exitCode = 0
      val manager = new SecurityManager() {
        override def checkPermission(perm: Permission): Unit = {
          val exitPrefix = "exitVM."
          if (perm.getName.startsWith(exitPrefix)) {
            exitCode = perm.getName.substring(exitPrefix.length).toInt
            throw new SecurityException("EXIT NOT ALLOWED")
          }
        }

      }
      System.setSecurityManager(manager)


      val out = new ByteArrayOutputStream()
      val err = new ByteArrayOutputStream()

      val preparedLines = input.mkString("\n")
      val in = new ByteArrayInputStream(preparedLines.getBytes)

      try {
        fn.apply(in, out, err)
      } catch {
        case e: SecurityException => // nothing
        case e: AssertionError => throw e
        case e: Throwable => throw e
      } finally {
        System.setSecurityManager(oldSecurityManager)
      }
      Assert.assertEquals(expectedOut, out.toString.linesIterator.toList
        .map(outFn)
        .mkString("\n"))
      Assert.assertEquals(expectedErr, err.toString.linesIterator.toList)
      Assert.assertEquals(expectedExitCode, exitCode)
    }
  }
}
