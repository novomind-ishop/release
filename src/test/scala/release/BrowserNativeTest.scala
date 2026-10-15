package release

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8

/** Run as a native executable to verify that no AWT browser is needed. */
object BrowserNativeTest {
  def main(args: Array[String]): Unit = {
    require(BrowserSupport.inNativeImage, "This check must run as a native executable")
    val output = new ByteArrayOutputStream()
    val original = System.out
    try {
      System.setOut(new PrintStream(output, true, UTF_8))
      Starter.openInDefaultBrowser("https://example.invalid/native-browser-test")
    } finally {
      System.setOut(original)
    }
    val message = output.toString(UTF_8)
    assert(message.contains("not supported in the native executable"), message)
    assert(message.contains("https://example.invalid/native-browser-test"), message)
    println("PASS: native browser fallback prints the URL without opening AWT")
  }
}
