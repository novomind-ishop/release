package release

import java.io.{ByteArrayOutputStream, PrintStream}
import org.junit.{Assert, Before, Test}
import org.scalatestplus.junit.AssertionsForJUnit

import scala.annotation.unused
import scala.compiletime.uninitialized

class StatusLineTest extends AssertionsForJUnit {
  var outContent: ByteArrayOutputStream = uninitialized
  var errContent: ByteArrayOutputStream = uninitialized
  var outStream: StatusPrinter = uninitialized
  var errStream: PrintStream = uninitialized

  @Before
  def setUpStreams(): Unit = {
    outContent = new ByteArrayOutputStream()
    errContent = new ByteArrayOutputStream()
    outStream = StatusPrinter.ofPrintStream(new PrintStream(outContent))
    errStream = new PrintStream(errContent)
  }

  @Test
  def onlyStart(): Unit = {
    // GIVEN

    @unused
    val status = StatusLine(72, 94, outStream, enabled = true)
    // THEN
    assertMsg("Progress: (000/072)[                                                                        ]")
  }

  @Test
  def onlyStart_short(): Unit = {
    // GIVEN
    @unused
    val status = StatusLine(72, 1, outStream, enabled = true)
    // THEN
    assertMsg("Progress: (000/072)[ ]")
  }

  @Test
  def startEnd2(): Unit = {
    // GIVEN
    val status = StatusLine(2, 94, outStream, enabled = true)
    status.start()
    status.start()
    status.end()
    // THEN
    assertMsg("" +
      "Progress: (000/002)[                                                                        ]\r" +
      "Progress: (000/002)[------------------------------------                                    ]\r" +
      "Progress: (000/002)[------------------------------------------------------------------------]\r" +
      "Progress: (001/002)[====================================------------------------------------]"
    )
  }

  @Test
  def startEnd72(): Unit = {
    // GIVEN
    val status = StatusLine(72, 94, outStream, enabled = true)
    status.start()
    status.start()
    status.end()
    // THEN
    assertMsg("" +
      "Progress: (000/072)[                                                                        ]\r" +
      "Progress: (000/072)[-                                                                       ]\r" +
      "Progress: (000/072)[--                                                                      ]\r" +
      "Progress: (001/072)[=-                                                                      ]"
    )
  }

  @Test
  def startEnd500(): Unit = {
    // GIVEN
    val status = StatusLine(500, 94, outStream, enabled = true)
    List.range(0, 500).foreach(_ => status.start())
    List.range(0, 500).foreach(_ => status.end())
    status.end()
    // THEN
    assertLine("Progress: (000/500)[                                                                        ]", 0)
    assertLine("Progress: (000/500)[-------                                                                 ]", 50)
    assertLine("Progress: (000/500)[--------------                                                          ]", 100)
    assertLine("Progress: (250/500)[====================================------------------------------------]", 750)
    assertLine("Progress: (499/500)[=======================================================================-]", 999)
    Assert.assertEquals(93, lines.split("\r")(0).length)

  }

  @Test
  def onlyFinish(): Unit = {
    // GIVEN
    val status = StatusLine(3, 94, outStream, enabled = true)
    status.finish()
    // THEN
    assertMsg("Progress: (000/003)[                                                                        ]\n")
  }

  def lines: String = outContent.toString().replace("\r\n", "\n")

  def assertLine(line: String, number: Int): Unit = {
    Assert.assertEquals(line, lines.split("\r")(number))
  }

  def assertMsg(expected: String): Unit = {
    Assert.assertEquals(expected, lines)
  }
}
