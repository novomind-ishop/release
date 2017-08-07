package release

import java.io.PrintStream

import scala.io.StdIn

object Term {

  def removeSnapshot(str: String): String = {
    val out = str.replaceFirst("-SNAPSHOT$", "").trim
    if (out.contains("-SNAPSHOT")) {
      removeSnapshot(out)
    } else {
      out
    }
  }

  def readFrom(out: PrintStream, text: String, defaultValue: String): String = {
    out.print(text + " [%s]: ".format(defaultValue))
    val line = StdIn.readLine()
    line match {
      case null ⇒ System.exit(1); null
      case "" ⇒ defaultValue
      case any: String ⇒ Option(any.trim).getOrElse(defaultValue)
    }
  }

  def readFromOneOfYesNo(out: PrintStream, text: String) = readFromOneOf(out, text, Seq("y", "n"))

  def readFromOneOf(out: PrintStream, text: String, possibleValues: Seq[String]): String = {
    out.print(text + " [%s]: ".format(possibleValues.mkString("/")))
    val line = StdIn.readLine
    line match {
      case null ⇒ System.exit(1); null
      case any: String if possibleValues.contains(any.trim) ⇒ any.trim
      case _: String ⇒ readFromOneOf(out, text, possibleValues)
    }
  }

  def readFromYes(out: PrintStream, text: String) = readFrom(out, text, "y")

  def readChooseOneOfOrType(out: PrintStream, text: String, possibleValues: Seq[String]): String = {
    possibleValues match {
      case vals if vals.size == 1 ⇒ readFrom(out, text, possibleValues.head)
      case vals ⇒ {
        out.println(text)
        val mapped: Map[String, String] = possibleValues.zip(Stream.from(1))
          .map(in ⇒ (in._2.toString, in._1)).foldLeft(Map.empty[String, String])(_ + _)
        possibleValues.zip(Stream.from(1))
          .map(in ⇒ (in._2, in._1)).foreach(p ⇒ out.println("[" + p._1 + "] " + p._2))
        out.print("Enter option or type [" + possibleValues.head + "]: ")
        val line = StdIn.readLine
        line match {
          case null ⇒ System.exit(1); null
          case "" ⇒ mapped.head._2
          case any: String if mapped.contains(any) ⇒ mapped(any)
          case other: String ⇒ other.trim
        }
      }
    }
  }
}
