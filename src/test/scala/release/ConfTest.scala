package release

import com.typesafe.scalalogging.LazyLogging
import org.junit.{Assert, Test}
import release.Conf.Tracer

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{CountDownLatch, TimeUnit}

class ConfTest extends LazyLogging {

  private class ProgressOutput extends ByteArrayOutputStream {
    val frames = new CountDownLatch(2)
    val stream = new PrintStream(this, true, StandardCharsets.UTF_8) {
      override def print(value: String): Unit = {
        super.print(value)
        if (value.startsWith("\r\u001b[2K")) frames.countDown()
      }
    }

    def awaitFrames(): Unit = {
      Assert.assertTrue("Spinner should advance while the operation is running", frames.await(3, TimeUnit.SECONDS))
    }

    def content: String = toString(StandardCharsets.UTF_8)
  }

  @Test(timeout = 5000)
  def spinnerAdvancesAndFinishesOnSameLine(): Unit = {
    val output = new ProgressOutput
    val result = Tracer.msgAroundWithProgress("release: update release version", logger, output.stream,
      () => { output.awaitFrames(); 42 }, animate = true)

    Assert.assertEquals(42, result)
    Assert.assertTrue(output.content, output.content.contains("⠋ update release version"))
    Assert.assertTrue(output.content, output.content.contains("⠙ update release version"))
    Assert.assertTrue(output.content, output.content.contains("\r\u001b[2K✓ update release version"))
    Assert.assertEquals("Only the completion should end the line", 1, output.content.count(_ == '\n'))
  }

  @Test(timeout = 5000)
  def spinnerReportsFailureAndPreservesException(): Unit = {
    val output = new ProgressOutput
    val failure = new IllegalStateException("test failure")
    try {
      Tracer.msgAroundWithProgress("release: update release version", logger, output.stream,
        () => { output.awaitFrames(); throw failure }, animate = true)
      Assert.fail("Operation should fail")
    } catch {
      case thrown: IllegalStateException => Assert.assertSame(failure, thrown)
    }
    Assert.assertTrue(output.content, output.content.contains("\r\u001b[2K✗ update release version"))
    Assert.assertTrue(output.content, output.content.endsWith(System.lineSeparator()))
  }

  @Test(timeout = 5000)
  def spinnerHonorsSimpleCharacters(): Unit = {
    val output = new ProgressOutput
    Tracer.msgAroundWithProgress("release: update release version", logger, output.stream,
      () => output.awaitFrames(), animate = true, simpleChars = true)
    Assert.assertTrue(output.content, output.content.contains("| update release version"))
    Assert.assertTrue(output.content, output.content.contains("+ update release version"))
    Assert.assertTrue(output.content, output.content.forall(_ <= 127))
  }

  @Test
  def quickOperationRemainsSilent(): Unit = {
    val output = new ProgressOutput
    Tracer.msgAroundWithProgress("release: update release version", logger, output.stream, () => 42, animate = true)
    Assert.assertEquals("", output.content)
  }
}
