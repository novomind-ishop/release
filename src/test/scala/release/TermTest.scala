package release

import com.typesafe.scalalogging.LazyLogging
import org.junit._
import org.scalatestplus.junit.AssertionsForJUnit
import release.TermTest.testSys

import java.io._
import java.nio.charset.StandardCharsets
import java.security.Permission
import scala.annotation.nowarn

class TermTest extends AssertionsForJUnit {

  @Test
  def testReadFrom(): Unit = {
    val value = "My string"

    TermTest.testSys(Seq(value), "enter some [word]: My string", "", expectedExitCode = 0)(sys => {
      val result = Term.readFrom(sys, "enter some", "word", Opts(useJlineInput = false))
      Assert.assertEquals(value, result)
      0
    })

  }

  @Test
  def testReadNull(): Unit = {
    TermTest.testSys(Nil, "enter some [word]: ", "invalid readFrom(..) => exit 14", 14)(sys => {
      Term.readFrom(sys, "enter some", "word", Opts(useJlineInput = false))
      0
    })
  }

  @Test
  def testReadDirect(): Unit = {
    TermTest.testSys(Seq("a", "b"), "", "", expectedExitCode = 0)(sys => {
      val bin: BufferedReader = new BufferedReader(new InputStreamReader(sys.inS))
      Assert.assertEquals("a", bin.readLine())
      Assert.assertEquals("b", bin.readLine())
      Assert.assertEquals(null, bin.readLine())
      0
    })
  }

  @Test
  def testThrows(): Unit = {
    TestHelper.assertComparisonFailure("expected:<[a]> but was:<[b]>", () => {
      TermTest.testSys(Nil, "", "", expectedExitCode = 0)(_ => {
        Assert.assertEquals("a", "b")
        0
      })
    })
  }

  @Test
  def testThrowsAll(): Unit = {
    TestHelper.assertException("hello", classOf[Exception], () => {
      TermTest.testSys(Nil, "", "", expectedExitCode = 0)(_ => {
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
  def testWrap_indent_2(): Unit = {
    val a =
      """
        |[WARNING]   a very long line a very long line a very long line a very long line a
        |[WARNING]     very long line a very long line
        |""".stripMargin.trim
    testSys(Nil, a, "", expectedExitCode = 0)(sys => {
      val value = "  a very long line a very long line a very long line a very long line a very long line a very long line "
      Term.wrap(sys.out, Term.warn, value, Opts(colors = false))
      0
    })
  }

  @Test
  def testWrap_indent_4_2(): Unit = {
    val a =
      """
        |[WARNING]     0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        |[WARNING]       19, 20, 21, 22, 23, 24, 25,26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        |[WARNING]       36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
        |[WARNING]       53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
        |[WARNING]       70, 71, 72, 73, 74
        |""".stripMargin.trim
    testSys(Nil, a, "", expectedExitCode = 0)(sys => {
      val value = "    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,26, " +
        "27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, " +
        "56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74"
      Term.wrap(sys.out, Term.warn, value, Opts(colors = false))
      0
    })
  }

  @Test
  def testWrap_noIndent(): Unit = {
    val a =
      """
        |[WARNING] a very long line a very long line a very long line a very long line a
        |[WARNING]   very long line a very long line
        |""".stripMargin.trim
    testSys(Nil, a, "", expectedExitCode = 0)(sys => {
      val value = "a very long line a very long line a very long line a very long line a very long line a very long line "
      Term.wrap(sys.out, Term.warn, value, Opts(colors = false))
      0
    })
  }

  @Test
  def testWrap_multiline(): Unit = {
    val a =
      """
        |[WARNING] a
        |[WARNING]   b
        |""".stripMargin.trim
    testSys(Nil, a, "", expectedExitCode = 0)(sys => {
      val value =
        """a
          |b""".stripMargin
      Term.wrap(sys.out, Term.warn, value, Opts(colors = false))
      0
    })
  }

  @Test
  def testWrap_multiline_indent(): Unit = {
    val a =
      """
        |[WARNING] a
        |[WARNING]   b
        |[WARNING]     c
        |""".stripMargin.trim
    testSys(Nil, a, "", expectedExitCode = 0)(sys => {
      val value =
        """a
          |b
          |  c""".stripMargin
      Term.wrap(sys.out, Term.warn, value, Opts(colors = false))
      0
    })
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
    Assert.assertEquals(Seq("  aa", "    aa"), Term.wrapText("  aa aa", 5))
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

  def testSys(input: Seq[String], expectedOut: String, expectedErr: String, expectedExitCode: Int,
              outFn: String => String = a => a, outAllFn: Seq[String] => Seq[String] = a => a)
             (fn: Term.Sys => Int): Unit = {
    this.synchronized {

      val out = new ByteArrayOutputStream()
      val err = new ByteArrayOutputStream()

      val preparedLines = input.mkString("\n")
      val in = new ByteArrayInputStream(preparedLines.getBytes)
      val exitCode = try {
        fn.apply(new Term.Sys(in, out, err) {
          override lazy val inReader = new BufferedReader(new InputStreamReader(inS)) {
            val innerOut = new PrintStream(outS)
            override def readLine(): String = {
              val line = super.readLine()
              if (line != null) {
                innerOut.println(line)
              }
              line
            }
          }
        })
      } catch {
        case e: SecurityException => // nothing
        case e: AssertionError => throw e
        case e: Throwable => throw e
      }
      Assert.assertEquals(expectedErr, outAllFn.apply(err.toString.linesIterator.toList
        .map(outFn))
        .mkString("\n"))
      Assert.assertEquals(expectedOut, outAllFn(out.toString.linesIterator.toList
        .map(outFn))
        .mkString("\n"))

      Assert.assertEquals(expectedExitCode, exitCode)
    }
  }
}
