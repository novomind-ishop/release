package release

import org.apache.commons.lang3.StringUtils
import org.apache.commons.lang3.text.WordUtils

import java.io.{BufferedReader, IOError, InputStream, InputStreamReader, OutputStream, PrintStream}
import org.jline.reader._
import org.jline.terminal.TerminalBuilder
import release.Starter.Opts

import java.time.{ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.annotation.{nowarn, tailrec}

object Term {

  private def readLineWithPromtJline(prompt: String, sys: Term.Sys): String = {

    val reader = sys.lineReader

    try {
      reader.readLine(prompt)
    } catch {
      case _: EndOfFileException => {
        System.exit(28)
        "EndOfFileException"
      }
      case _: UserInterruptException => {
        System.exit(29)
        "UserInterruptException"
      }
      case _: IOError => {
        System.exit(30)
        "IOError"
      }
    }
  }

  private def readLineWithPrompt(sys: Term.Sys, prompt: String, opts: Opts): String = {
    if (opts.useJlineInput) {
      readLineWithPromtJline(prompt, sys)
    } else {
      sys.out.print(prompt)
      val reader = sys.inReader
      val result = reader.readLine()
      result
    }
  }

  @tailrec
  def removeTrailingSnapshots(str: String): String = {
    val out = str.replaceFirst("-SNAPSHOT$", "").trim
    if (out.endsWith("-SNAPSHOT")) {
      removeTrailingSnapshots(out)
    } else {
      out
    }
  }

  def readFrom(sys: Term.Sys, text: String, defaultValue: String, opts: Opts): String = {
    val line = readLineWithPrompt(sys, text + " [%s]: ".format(defaultValue), opts)
    val result = line match {
      case null => {
        sys.err.println("invalid readFrom(..) => exit 14")
        System.exit(14)
        null
      }
      case "" => defaultValue
      case any: String => Option(any.trim).getOrElse(defaultValue)
    }

    result match {
      case value if value.contains("-UNDEF") => throw new IllegalArgumentException("\"-UNDEF\" is not allowed")
      case value if value.trim.isEmpty => throw new IllegalArgumentException("blank is not allowed")
      case other => other
    }
  }

  def readFromOneOfYesNo(sys: Term.Sys, text: String, opts: Opts): String = readFromOneOf(sys, text, Seq("y", "n"), opts)

  @tailrec
  def readFromOneOf(sys: Term.Sys, text: String, possibleValues: Seq[String], opts: Opts): String = {
    val line = readLineWithPrompt(sys, text + " [%s]: ".format(
      possibleValues.map(line => line.replace(' ', ' ')).mkString("/")), opts)
    line match {
      case null => {
        System.err.println("invalid readFromOneOf(..)")
        System.exit(1)
        null
      }
      case any: String if possibleValues.contains(any.trim) => any.trim
      case _: String => readFromOneOf(sys, text, possibleValues, opts)
    }
  }

  private def printOptions(sys: Term.Sys, possibleValues: Seq[String]): Map[String, String] = {
    possibleValues.zip(LazyList.from(1))
      .map(in => (in._2, in._1)).foreach(p => sys.out.println("[" + p._1 + "] " + p._2))
    possibleValues.zip(LazyList.from(1))
      .map(in => (in._2.toString, in._1)).foldLeft(Map.empty[String, String])(_ + _)
  }

  def readChooseOneOf(sys: Term.Sys, text: String, possibleValues: Seq[String], opts: Opts): String = {
    possibleValues match {
      case Nil => throw new IllegalArgumentException("possible value size must not be empty")
      case values if values.size == 1 => throw new IllegalArgumentException("possible value size must not be one")
      case _ => {
        sys.out.println(text)
        val mapped: Map[String, String] = printOptions(sys, possibleValues)
        val line = readLineWithPrompt(sys, "Enter option [" + mapped.head._1 + "]: ", opts)
        line match {
          case null => {
            System.err.println("invalid readChooseOneOf(..)")
            System.exit(1)
            null
          }
          case "" => mapped.head._2
          case any: String if mapped.contains(any) => mapped(any)
          case other: String => other.trim
        }
      }
    }
  }

  def readChooseOneOfOrType(sys: Term.Sys, text: String, possibleValues: Seq[String], opts: Opts,
                            valSelectF: Seq[String] => String = _.head,
                            mappedF: Map[String, String] => (String, String) = _.head): String = {
    possibleValues match {
      case Nil => throw new IllegalArgumentException("possible value size must not be empty")
      case values if values.size == 1 => readFrom(sys, text, possibleValues.head, opts)
      case _ => {
        sys.out.println(text)
        val mapped: Map[String, String] = printOptions(sys, possibleValues)
        val line = readLineWithPrompt(sys, "Enter option or type [" + valSelectF.apply(possibleValues) + "]: ", opts)
        line match {
          case null => {
            System.err.println("invalid readChooseOneOfOrType(..)")
            System.exit(1)
            null
          }
          case "" => mappedF.apply(mapped)._2
          case any: String if mapped.contains(any) => mapped(any)
          case other: String => other.trim
        }
      }
    }
  }

  def colorB(color: Int, text: String, useColor: Boolean, noColorText: Option[String] = None): String = {
    if (useColor) {
      "[" + "\u001B[" + color + "m" + text + "\u001B[0m" + "] "
    } else {
      "[" + noColorText.getOrElse(text) + "] "
    }

  }

  def checkedLength(length: Int)(in: String): String = {
    if (in.length > length) {
      try {
        throw new IllegalArgumentException(s"reduce length to $length for '${in}'")
      } catch {
        case e: IllegalArgumentException => e.printStackTrace()
      }
    }
    in
  }

  case class ColoredLiner(defaultLimit: Int, fn: (String, Boolean, Int) => String) {
    def ex(text: String, opts: Opts): String = {
      ex(text, opts, defaultLimit)
    }

    def ex(text: String, opts: Opts, limit: Int): String = {
      val timestamp = if (opts.lintOpts.showTimeStamps) {
        val time = ZonedDateTime.now(ZoneOffset.UTC)
        DateTimeFormatter.ofPattern("'['HH:mm:ss.SSX'] '").format(time)
      } else {
        ""
      }

      timestamp + fn.apply(text.stripTrailing(), opts.colors, limit)
    }
  }

  val info = ColoredLiner(82 - 4, (text, useColor, limit) =>
    colorB(34, "INFO", useColor) + checkedLength(limit)(text))
  val warnSoft = ColoredLiner(82 - 7, (text, useColor, limit) =>
    colorB(34, "WARNING", useColor, Some("warning")) + checkedLength(limit)(text))
  val warn = ColoredLiner(82 - 7, (text, useColor, limit) =>
    colorB(33, "WARNING", useColor) + checkedLength(limit)(text))
  val error = ColoredLiner(82 - 5, (text, useColor, limit) =>
    colorB(31, "ERROR", useColor) + checkedLength(limit)(text))

  def wrap(out: PrintStream, coloredLiner: ColoredLiner, text: String, opts: Opts): Unit = {
    wrapText(text, coloredLiner.defaultLimit)
      .foreach(line => out.println(coloredLiner.ex(line, opts)))

  }

  @nowarn("cat=deprecation")
  def wrapText(str: String, limit: Int): Seq[String] = {
    val indent = str.takeWhile(_ == ' ')
    val indent2 = indent + "  "
    val flip = new AtomicBoolean(true)

    val o = str.trim.linesIterator.map(_.split(" ").toSeq).toSeq

    def asdf(words: Seq[String]): Seq[Seq[String]] = {
      val wcount = new AtomicInteger(0)
      if (words.isEmpty) {
        Nil
      } else {
        val taken = words.takeWhile(word => {
          val r = wcount.get() + word.length + 1 <= limit - indent2.length || wcount.get() == 0
          if (r) {
            wcount.addAndGet(word.length + 1)
          }
          r
        }).toList
        val value = words.drop(taken.length)
        Seq(taken) ++ asdf(value)
      }
    }

    val wer: Seq[Seq[String]] = o.flatMap(asdf)
    val l = wer.map(line => {
      if (flip.getAndSet(false)) {
        indent + line.mkString(" ")
      } else {
        indent2 + line.mkString(" ")
      }
    }
    )

    l
  }

  def info(text: String, opts: Opts, limit: Int = info.defaultLimit): String = info.ex(text, opts, limit)

  def warnSoft(text: String, opts: Opts, limit: Int = warnSoft.defaultLimit): String = warnSoft.ex(text, opts, limit)

  def warn(text: String, opts: Opts, limit: Int = warn.defaultLimit): String = warn.ex(text, opts, limit)

  def error(text: String, opts: Opts, limit: Int = error.defaultLimit): String = error.ex(text, opts, limit)

  def center(text: String): String = {
    val lenght = 72
    val fill: Int = (lenght - text.length) / 2
    val w = "-"
    val result = List.fill(fill)(w).mkString("") + text + List.fill(fill)(w).mkString("")
    if (result.length < lenght) {
      result + " "
    } else {
      result
    }
  }

  object Os {
    lazy val getCurrent: Os = {
      System.getProperty("os.name") match {
        case "Windows 11" => Os.Windows
        case "Windows 10" => Os.Windows
        case "Linux" => Os.Linux
        case "Mac OS X" => Os.Darwin
        case other => throw new IllegalStateException("unknown os: " + other)
      }
    }
    val Windows = Os("win")
    val Linux = Os("Linux")
    val Darwin = Os("Darwin")
  }

  sealed case class Os(name: String)

  def select(term: String, os: String, simpleChars: Boolean, isInteractice: Boolean) = term match {
    case "xterm" => Term("xterm", os, simpleChars, isInteractice)
    case "xterm-256color" => Term("xterm-256color", os, simpleChars, isInteractice)
    case "screen-256color" => Term("screen-256color", os, simpleChars, isInteractice)
    case "cygwin" => Term("cygwin", os, simpleChars, isInteractice)
    case "screen" => Term("screen", os, simpleChars, isInteractice)
    case "dumb" => Term("dumb", os, simpleChars, isInteractice)
    case t => throw new IllegalStateException(s"invalid terminal: ${t}")
  }

  class Sys(val inS: InputStream, val outS: OutputStream, val errS: OutputStream) {
    lazy val out = new PrintStream(outS)
    lazy val err = new PrintStream(errS)
    lazy val inReader = new BufferedReader(new InputStreamReader(inS))
    lazy val lineReader: LineReader = {
      val sys: Boolean = inS == System.in && out == System.out
      val terminal = TerminalBuilder.builder()
        .system(sys)
        .streams(inS, out)
        .build()
      LineReaderBuilder.builder()
        .terminal(terminal)
        .option(LineReader.Option.INSERT_TAB, true)
        .build()
    }
  }

  object Sys {
    val default = new Sys(System.in, System.out, System.err)
  }
}

case class Term(term: String, os: String, simpleChars: Boolean, isInteractice: Boolean) {
  val isCygwin: Boolean = os == "Cygwin" || term == "cygwin"
  val isMinGw: Boolean = os.contains("MINGW")
}
