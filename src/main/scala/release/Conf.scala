package release

import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger

object Conf {

  object Tracer {

    var tracerIndex = Map.empty[String, Int]
    val idxCounter = new AtomicInteger()


    def withFn(logger: Logger, fn: () ⇒ String): Unit = {
      if (logger.underlying.isTraceEnabled) {
        logger.trace(fn.apply())
      }
    }

    private def index(str: String): Int = {
      val idx = tracerIndex.get(str)
      idx match {
        case None ⇒ {
          synchronized {
            tracerIndex = tracerIndex + (str → idxCounter.getAndIncrement())
            index(str)
          }
        }
        case in ⇒ {
          in.get
        }
      }
    }

    def msgAround[T](message: String, logger: Logger, fn: () ⇒ T): T = {
      val start = System.currentTimeMillis()
      val idx = index(message)
      logger.trace("/ started %03d %s".format(idx, message))
      val result = fn.apply()
      val end = System.currentTimeMillis()
      logger.trace("\\ ended %03d  %s".format(idx, message))
      logger.trace("§ duration: %03d ".format(idx) + (end - start) + " " + message)
      result
    }
  }

}

