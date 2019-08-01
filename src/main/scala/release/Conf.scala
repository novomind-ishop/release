package release

import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

object Conf {

  object Tracer {

    val tracerIndex: TrieMap[String, Int] = TrieMap.empty
    val idxCounter = new AtomicInteger()

    def withFn(logger: Logger, fn: () => String): Unit = {
      if (logger.underlying.isTraceEnabled) {
        logger.trace(fn.apply())
      }
    }

    @tailrec
    private def index(str: String): Int = {
      val idx = tracerIndex.get(str)
      idx match {
        case None => {
          tracerIndex.put(str, idxCounter.getAndIncrement())
          index(str)
        }
        case in => in.get
      }
    }

    def msgAround[T](message: String, logger: Logger, fn: () => T): T = {
      val start = System.currentTimeMillis()
      val idx = index(message)
      logger.trace("/ started idx(%03d) %s".format(idx, message))
      val result = fn.apply()
      val end = System.currentTimeMillis()
      logger.trace("\\ ended   idx(%03d)  %s".format(idx, message))
      logger.trace("§ duration idx(%03d): ".format(idx) + (end - start) + "ms " + message)
      result
    }
  }

}

