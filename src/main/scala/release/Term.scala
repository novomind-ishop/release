package release

import java.io.{BufferedReader, IOError, PrintStream}

import org.jline.reader._
import org.jline.terminal.TerminalBuilder
import release.Starter.Opts

import scala.annotation.tailrec

object Term {

  private def readLineWithPromtJline(prompt: String): String = {
    val terminal = TerminalBuilder.builder()
      .system(true)
      .dumb(true)
      .streams(System.in, System.out)
      .build()
    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .option(LineReader.Option.INSERT_TAB, true)
      .build()

    try {
      reader.readLine(prompt)
    } catch {
      case _: EndOfFileException => {
        System.exit(28)
        "EndOfFileException"
      }
      case _: UserInterruptException => {
        System.exit(28)
        "UserInterruptException"
      }
      case _: IOError => {
        System.exit(28)
        "IOError"
      }
    }
  }

  private def readLineWithPrompt(in: BufferedReader, out: PrintStream, prompt: String, opts: Opts): String = {
    if (opts.useJlineInput) {
      readLineWithPromtJline(prompt)
    } else {
      out.print(prompt)
      in.readLine()
    }
  }

  @tailrec
  def removeSnapshot(str: String): String = {
    val out = str.replaceFirst("-SNAPSHOT$", "").trim
    if (out.contains("-SNAPSHOT")) {
      removeSnapshot(out)
    } else {
      out
    }
  }

  def readFrom(out: PrintStream, text: String, defaultValue: String, opts: Opts, in: BufferedReader): String = {
    val line = readLineWithPrompt(in, out, text + " [%s]: ".format(defaultValue), opts)
    val result = line match {
      case null => {
        System.err.println("invalid readFrom(..)")
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

  def readFromOneOfYesNo(out: PrintStream, text: String, opts: Opts, in: BufferedReader = Console.in): String = readFromOneOf(out, text, Seq("y", "n"), opts, in)

  @tailrec
  def readFromOneOf(out: PrintStream, text: String, possibleValues: Seq[String], opts: Opts, in: BufferedReader): String = {
    val line = readLineWithPrompt(in, out, text + " [%s]: ".format(
      possibleValues.map(line => line.replace(' ', ' ')).mkString("/")), opts)
    line match {
      case null => {
        System.err.println("invalid readFromOneOf(..)")
        System.exit(1)
        null
      }
      case any: String if possibleValues.contains(any.trim) => any.trim
      case _: String => readFromOneOf(out, text, possibleValues, opts, in)
    }
  }

  private def printOptions(out: PrintStream, possibleValues: Seq[String]): Map[String, String] = {
    possibleValues.zip(LazyList.from(1))
      .map(in => (in._2, in._1)).foreach(p => out.println("[" + p._1 + "] " + p._2))
    possibleValues.zip(LazyList.from(1))
      .map(in => (in._2.toString, in._1)).foldLeft(Map.empty[String, String])(_ + _)
  }

  def readChooseOneOf(out: PrintStream, text: String, possibleValues: Seq[String], opts: Opts, in: BufferedReader): String = {
    possibleValues match {
      case Nil => throw new IllegalArgumentException("possible value size must not be empty")
      case values if values.size == 1 => throw new IllegalArgumentException("possible value size must not be one")
      case _ => {
        out.println(text)
        val mapped: Map[String, String] = printOptions(out, possibleValues)
        val line = readLineWithPrompt(in, out, "Enter option [" + mapped.head._1 + "]: ", opts)
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

  def readChooseOneOfOrType(out: PrintStream, text: String, possibleValues: Seq[String], opts: Opts, in: BufferedReader = Console.in): String = {
    possibleValues match {
      case Nil => throw new IllegalArgumentException("possible value size must not be empty")
      case values if values.size == 1 => readFrom(out, text, possibleValues.head, opts, in)
      case _ => {
        out.println(text)
        val mapped: Map[String, String] = printOptions(out, possibleValues)
        val line = readLineWithPrompt(in, out, "Enter option or type [" + possibleValues.head + "]: ", opts)
        line match {
          case null => {
            System.err.println("invalid readChooseOneOfOrType(..)")
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
}
