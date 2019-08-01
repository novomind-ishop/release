package release

import java.io._
import java.security.Permission

import com.typesafe.scalalogging.LazyLogging
import org.junit._
import org.scalatestplus.junit.AssertionsForJUnit
import release.Starter.Opts

class TermTest extends AssertionsForJUnit {

  var in: InputStream = null

  @Before
  def before(): Unit = {
    in = System.in
  }

  @After
  def after(): Unit = {
    System.setIn(in)
  }

  @Test
  def testRemoveSnapshot_0(): Unit = {
    val out = Term.removeSnapshot("RC-2009.01")

    Assert.assertEquals("RC-2009.01", out)
  }

  @Test
  def testRemoveSnapshot_1(): Unit = {
    val out = Term.removeSnapshot("0.1.1-SNAPSHOT")

    Assert.assertEquals("0.1.1", out)
  }

  @Test
  def testRemoveSnapshot_2(): Unit = {
    val out = Term.removeSnapshot("hallo-SNAPSHOT-SNAPSHOT")

    Assert.assertEquals("hallo", out)
  }

  @Test
  @Ignore
  def testReadFrom(): Unit = {
    val value = "My string"

    TermTest.testSys(Seq(value), Seq("enter some [word]: "), Nil)((in, out, err) => {
      val result = Term.readFrom(new PrintStream(out), "enter some", "word", Opts(),
        new BufferedReader(new InputStreamReader(in, "UTF-8")))
      Assert.assertEquals(value, result)
    })

  }

  @Test
  @Ignore
  def testReadNull(): Unit = {
    TermTest.testSys(Nil, Seq("enter some [word]: "), Nil, 14)((in, out, err) => {
      Term.readFrom(new PrintStream(out), "enter some", "word", Opts(),
        new BufferedReader(new InputStreamReader(in, "UTF-8")))
    })

  }

}

object TermTest extends LazyLogging {

  def testSys(input: Seq[String], expectedOut: Seq[String], expectedErr: Seq[String], expectedExitCode: Int = 0)
             (fn: (InputStream, OutputStream, OutputStream) => Unit): Unit = {
    this.synchronized {
      val oldSecurityManager = System.getSecurityManager
      var exitCode = 0
      val manager = new SecurityManager() {
        override def checkPermission(perm: Permission): Unit = {
          val exitPrefix = "exitVM."
          if (perm.getName.startsWith(exitPrefix)) {
            exitCode = perm.getName.substring(exitPrefix.length).toInt
            throw new SecurityException
          }
        }

      }
      System.setSecurityManager(manager)

      val oldIn = System.in
      val oldOut = System.out
      val oldErr = System.err

      val out = new ByteArrayOutputStream()
      val err = new ByteArrayOutputStream()
      val in = new ByteArrayInputStream(input.mkString("\n").getBytes)
      System.setIn(in)
      System.setOut(new PrintStream(out))
      System.setErr(new PrintStream(err))

      try {
        fn.apply(System.in, System.out, System.err)
      } catch {
        case e: Throwable => e.printStackTrace(oldErr)
      } finally {
        System.setIn(oldIn)
        System.setOut(oldOut)
        System.setErr(oldErr)
        System.setSecurityManager(oldSecurityManager)
      }
      Assert.assertEquals(expectedOut, out.toString.linesIterator.toList)
      Assert.assertEquals(expectedErr, err.toString.linesIterator.toList)
      Assert.assertEquals(expectedExitCode, exitCode)
    }
  }
}
