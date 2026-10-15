package release

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.spi.Configurator
import ch.qos.logback.classic.util.{DefaultJoranConfigurator, LogbackMDCAdapter}
import ch.qos.logback.core.status.Status
import org.junit.Test

import scala.jdk.CollectionConverters.*

class TraceLogbackTest {
  @Test
  def xmlConfigurationStartsAppenders(): Unit = {
    TraceLogbackTest.check("ch.qos.logback.classic.util.DefaultJoranConfigurator")
  }

  @Test
  def basicFallbackStartsAppender(): Unit = {
    TraceLogbackTest.check("ch.qos.logback.classic.BasicConfigurator")
  }
}

/** Also runs in a separate JVM with the Native Image tracing agent. */
object TraceLogbackTest {
  def main(configuratorNames: Array[String]): Unit = {
    require(configuratorNames.nonEmpty, "Pass the configurator class names to trace")
    configuratorNames.foreach { name =>
      check(name)
      println(s"PASS: $name")
    }
  }

  private def check(name: String): Unit = {
    // Keep the application's shared LoggerFactory context untouched during tests.
    val context = new LoggerContext()
    try {
      context.setName("native-build-trace")
      context.setMDCAdapter(new LogbackMDCAdapter())
      val configurator = Class.forName(name).getConstructor().newInstance().asInstanceOf[Configurator]
      configurator.setContext(context)
      configurator match {
        // Exercise the production configuration even on the test classpath,
        // where automatic discovery would select logback-test.xml instead.
        case joran: DefaultJoranConfigurator =>
          joran.configureByResource(getClass.getClassLoader.getResource("logback.xml"))
        case _ => configurator.configure(context)
      }
      assert(context.getLogger("ROOT").iteratorForAppenders().hasNext, s"No appender configured by $name")

      if (name == "ch.qos.logback.classic.util.DefaultJoranConfigurator") {
        assert(Option(context.getLogger("ROOT").getAppender("CONSOLE")).exists(_.isStarted),
          "Console appender did not start")
        assert(Option(context.getLogger("release").getAppender("FILE_IN_HOME")).exists(_.isStarted),
          "File appender did not start")
        context.getLogger("release.native-build").info("Native build logging check")
      }

      context.getStatusManager.getCopyOfStatusList.asScala.foreach { status =>
        assert(status.getEffectiveLevel < Status.WARN, status.toString)
      }
    } finally {
      context.stop()
    }
  }
}
