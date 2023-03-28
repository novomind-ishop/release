package release

import com.typesafe.scalalogging.LazyLogging
import org.junit._
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts
import release.TermTest.testSys

import java.io._
import java.nio.charset.StandardCharsets
import java.security.Permission
import scala.annotation.nowarn

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

    TermTest.testSys(Seq(value), "enter some [word]: My string", Nil)(sys => {
      val result = Term.readFrom(sys, "enter some", "word", Opts(useJlineInput = false))
      Assert.assertEquals(value, result)
    })

  }

  @Test
  def testReadNull(): Unit = {
    TermTest.testSys(Nil, "enter some [word]: ", Seq("invalid readFrom(..) => exit 14"), 14)(sys => {
      Term.readFrom(sys, "enter some", "word", Opts(useJlineInput = false))
    })
  }

  @Test
  def testReadDirect(): Unit = {
    TermTest.testSys(Seq("a", "b"), "", Nil)(sys => {
      val bin: BufferedReader = new BufferedReader(new InputStreamReader(sys.inS))
      Assert.assertEquals("a", bin.readLine())
      Assert.assertEquals("b", bin.readLine())
      Assert.assertEquals(null, bin.readLine())
    })
  }

  @Test
  def testThrows(): Unit = {
    TestHelper.assertComparisonFailure("expected:<[a]> but was:<[b]>", () => {
      TermTest.testSys(Nil, "", Nil)(_ => {
        Assert.assertEquals("a", "b")
      })
    })
  }

  @Test
  def testThrowsAll(): Unit = {
    TestHelper.assertException("hello", classOf[Exception], () => {
      TermTest.testSys(Nil, "", Nil)(_ => {
        throw new Exception("hello")
      })
    })

  }

  @Test
  def testCenterSome(): Unit = {

    val value = Term.center("[ bert ]")
    Assert.assertEquals(72, value.size)
    Assert.assertEquals("--------------------------------[ bert ]--------------------------------", value)

  }

  @Test
  def testCenterOther(): Unit = {

    val value = Term.center("[ b ]")
    Assert.assertEquals(72, value.size)
    Assert.assertEquals("---------------------------------[ b ]--------------------------------- ", value)

  }

  @Test
  def testWrapText_1(): Unit = {
    Assert.assertEquals(Seq("a"), Term.wrapText("a", 3))
  }

  @Test
  def testWrapText_2(): Unit = {
    Assert.assertEquals(Seq("aaaa"), Term.wrapText("aaaa", 3))
  }

  @Test
  def testWrapText_3(): Unit = {
    Assert.assertEquals(Seq("aa", "  aa"), Term.wrapText("aa aa", 3))
  }

  @Test
  def testWrapText_4(): Unit = {
    Assert.assertEquals(Seq("  aa", "  aa"), Term.wrapText("  aa aa", 5))
  }
}

object TermTest extends LazyLogging {
  def willReadFrom(in: String): InputStream = {
    val stream = new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8))
    stream
  }

  def normalize(in: ByteArrayOutputStream): String = in.toString.trim.replaceAll("\r\n", "\n")

  def withOutErr[T](fn: (PrintStream, PrintStream) => T): OutErr[T] = {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val x = fn.apply(new PrintStream(out), new PrintStream(err))
    OutErr(normalize(out), normalize(err), x)
  }

  def withOutErrIn[T](in: InputStream)(fn: Term.Sys => T): OutErr[T] = {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val x = fn.apply(new Term.Sys(in, out, err))
    OutErr(normalize(out), normalize(err), x)
  }

  def withOutErr[T]()(fn: Term.Sys => T): OutErr[T] = {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val x = fn.apply(new Term.Sys(null, out, err))
    OutErr(normalize(out), normalize(err), x)
  }

  case class OutErr[T](out: String, err: String, value: T)

  def testSys(input: Seq[String], expectedOut: String, expectedErr: Seq[String], expectedExitCode: Int = 0,
              outFn: String => String = a => a)
             (fn: Term.Sys => Unit): Unit = {
    this.synchronized {
      @nowarn
      val oldSecurityManager = System.getSecurityManager
      var exitCode = 0
      @nowarn("cat=deprecation")
      val manager = new SecurityManager() {
        override def checkPermission(perm: Permission): Unit = {
          val exitPrefix = "exitVM."
          if (perm.getName.startsWith(exitPrefix)) {
            exitCode = perm.getName.substring(exitPrefix.length).toInt
            // https://openjdk.org/jeps/411
            // https://bugs.openjdk.org/browse/JDK-8199704
            throw new SecurityException("EXIT NOT ALLOWED")
          }
        }

      }
      @nowarn("cat=deprecation")
      val unit = System.setSecurityManager(manager)

      val out = new ByteArrayOutputStream()
      val err = new ByteArrayOutputStream()

      val preparedLines = input.mkString("\n")
      val in = new ByteArrayInputStream(preparedLines.getBytes)
      try {
        fn.apply(new Term.Sys(in, out, err) {
          override lazy val inReader = new BufferedReader(new InputStreamReader(inS)) {
            override def readLine(): String = {
              val line = super.readLine()
              if (line != null) {
                out.println(line)
              }
              line
            }
          }
        })
      } catch {
        case e: SecurityException => // nothing
        case e: AssertionError => throw e
        case e: Throwable => throw e
      } finally {
        @nowarn("cat=deprecation")
        val unit = System.setSecurityManager(oldSecurityManager)
      }
      Assert.assertEquals(expectedErr.mkString("\n"), err.toString.linesIterator.toList.mkString("\n"))
      Assert.assertEquals(expectedOut, out.toString.linesIterator.toList
        .map(outFn)
        .mkString("\n"))
      Assert.assertEquals(expectedExitCode, exitCode)
    }
  }
}
