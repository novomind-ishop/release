package release

import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger

trait StatusPrinter {
  def print(string: String): Unit

  def println(): Unit
}

object StatusPrinter {
  def ofPrintStream(printStream: PrintStream): StatusPrinter = {
    new StatusPrinter() {
      override def print(string: String): Unit = printStream.print(string)

      override def println(): Unit = printStream.println()
    }
  }
}

case class StatusLine(rawSteps: Int, width: Int, out: StatusPrinter = StatusPrinter.ofPrintStream(System.out), enabled: Boolean) {
  private val scaleLimit = if (width - 22 <= 0) {
    1
  } else {
    width - 22
  }
  private val innerSteps = if (rawSteps <= 0) {
    1
  } else {
    rawSteps
  }
  private val step: Double = scaleLimit.toDouble / innerSteps.toDouble
  private val started = new AtomicInteger(0)
  private val ended = new AtomicInteger(0)

  private def mk(symbol: String, count: Int) = {
    val endOfLine = if (count == 0) {
      0
    } else {
      val in = (count * step).toInt
      if (in == 0) {
        1
      } else {
        in
      }
    }
    val range = List.range(0, endOfLine)
    range.map(_ => symbol).mkString
  }

  private def updateLine(): Unit = {
    this.synchronized {

      val doneL = ended.get
      val wipL = started.get() - doneL
      val openL = rawSteps - wipL - doneL

      val done = mk("=", doneL)
      val wip = mk("-", wipL)
      val open = mk(" ", openL)

      val line = done + wip + open
      if (enabled) {
        out.print(("Progress: (%03d/%03d)[%-" + scaleLimit + "s]").format(doneL, rawSteps, line))
      }
    }
  }

  if (rawSteps > 0) {
    updateLine()
  }

  private def update(counter: AtomicInteger): Unit = {
    counter.addAndGet(1)
  }

  def start(): Unit = {
    this.synchronized {
      update(started)
      if (enabled) {
        out.print("\r")
      }
      updateLine()
    }
  }

  def end(): Unit = {
    this.synchronized {
      update(ended)
      if (enabled) {
        out.print("\r")
      }
      updateLine()
    }
  }

  def finish(): Unit = {
    this.synchronized {
      if (rawSteps > 0) {
        out.println()
      }
    }
  }
}
