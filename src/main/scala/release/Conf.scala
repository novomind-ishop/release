package release

import java.util.concurrent.atomic.AtomicInteger
import java.io.PrintStream
import java.util.{Timer, TimerTask}

import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._

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
      val start = System.nanoTime()
      val idx = index(message)
      logger.trace("/ started idx(%03d) %s".format(idx, message))
      try fn.apply()
      finally {
        val elapsedMillis = (System.nanoTime() - start) / 1_000_000
        logger.trace("\\ ended   idx(%03d)  %s".format(idx, message))
        logger.trace("§ duration idx(%03d): ".format(idx) + elapsedMillis + "ms " + message)
      }
    }

    def msgAroundWithProgress[T](message: String, logger: Logger, out: PrintStream, fn: () => T,
        animate: Boolean = false, simpleChars: Boolean = false): T = {
      val start = System.nanoTime()
      val workThread = Thread.currentThread()
      val progressLock = new Object()
      var finished = false
      var succeeded = false
      var displayed = false
      var frame = 0
      var lastStackDump = 0L
      val frames = if (simpleChars) "|/-\\" else "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
      val label = message.stripPrefix("release: ")
      val separator = if (simpleChars) "-" else "·"
      def status(symbol: String, elapsedMillis: Long): String = {
        f"${symbol} ${label} ${separator} ${elapsedMillis / 1000.0}%.1f s"
      }
      val timer = new Timer("release-progress", true)
      timer.schedule(
        new TimerTask {
          override def run(): Unit = progressLock.synchronized {
            if (!finished) {
              val elapsedMillis = (System.nanoTime() - start) / 1_000_000
              if (animate) {
                out.print("\r\u001b[2K" + status(frames.charAt(frame).toString, elapsedMillis))
                frame = (frame + 1) % frames.length
                displayed = true
              } else {
                out.println(s"I: Working: ${label} (${elapsedMillis / 1000}s)")
              }
              out.flush()
              if (elapsedMillis - lastStackDump >= 5000L) {
                lastStackDump = elapsedMillis
                Thread.getAllStackTraces.asScala.foreach { case (thread, stack) =>
                  if (
                    thread == workThread || thread.getName.contains("ForkJoinPool") ||
                    thread.getName.startsWith("scala-execution-context")
                  ) {
                    logger.trace(s"Slow step: ${message}, ${elapsedMillis}ms, thread=${thread.getName}, state=${thread.getState}\n" +
                      stack.mkString("  ", "\n  ", ""))
                  }
                }
              }
            }
          }
        },
        if (animate) 500L else 5000L,
        if (animate) 80L else 5000L
      )
      try {
        val result = msgAround(message, logger, fn)
        succeeded = true
        result
      } finally
        progressLock.synchronized {
          finished = true
          timer.cancel()
          if (displayed) {
            val symbol = if (simpleChars) {
              if (succeeded) "+" else "!"
            } else {
              if (succeeded) "✓" else "✗"
            }
            out.println("\r\u001b[2K" + status(symbol, (System.nanoTime() - start) / 1_000_000))
            out.flush()
          }
        }
    }
  }

}
