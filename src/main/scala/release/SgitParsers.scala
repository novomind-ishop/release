package release

import com.typesafe.scalalogging.LazyLogging
import release.Sgit.GitTagWithDate

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.time.ZonedDateTime
import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object SgitParsers {

  def unescape(str: String): String = {
    @tailrec
    def cate(in: Seq[Char], result: Seq[Byte] = Nil): Seq[Byte] = {
      in match {
        case '\\' :: c0 :: c1 :: c2 :: tail => {
          val b = new BigInteger(new String(Array(c0, c1, c2)), 8).byteValue()
          cate(tail, result :+ b)
        }
        case c :: tail => cate(tail, result :+ c.toByte)
        case Nil => result
        case _ => throw new IllegalStateException("not expected")
      }
    }

    if (str.matches(".*\\\\[0-9]{3}.*")) {
      new String(cate(str.toList).toArray, StandardCharsets.UTF_8)
    } else {
      str
    }
  }

  val isoDateR = "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(?:[+-][0-9]{2}:[0-9]{2}|Z)".r


  def parseIsoDate(in: String): ZonedDateTime = {
    ZonedDateTime.parse(in)
  }

  def parseIsoDateOpt(in: String): Option[ZonedDateTime] = {
    try {
      Some(parseIsoDate(in))
    } catch {
      case e: Exception => None
    }
  }

  object LogParser extends RegexParsers with LazyLogging {
    override val skipWhitespace = false

    def parts: Parser[Option[Seq[String]]] = "(" ~> "[^\\)]+".r <~ ") " ^^ (term => {
      Some(term.split(", ").toSeq)
    })

    def isoDate: Parser[Option[ZonedDateTime]] = isoDateR ^^ (term => parseIsoDateOpt(term))

    def sha1: Parser[String] = "[a-f0-9]{40}".r <~ " " ^^ (term => term)

    val comb = rep1(opt("[ \n']+".r) ~> opt(parts) ~ sha1 ~ isoDate <~ opt("[\n']+".r))

    def doParseSimple(s: String): Seq[SlogLine] = {
      doParse(s, s.split("/n").toSeq)
    }

    def doParse(s: String, orgLines: Seq[String]): Seq[SlogLine] = {
      if (s.isBlank) {
        Nil
      } else {
        val p = comb
        parseAll(p, s) match {
          case Success(vp, v) => {
            vp.flatMap(e => {
              val isoDate: Option[ZonedDateTime] = e._2
              val sha1su = e._1._2

              val allRefs:Seq[String] = e._1._1.flatten.getOrElse(Nil)
              val tagNames: Seq[String] = allRefs
                .filter(_.startsWith("tag: "))
                .map(_.replaceFirst("^tag: ", ""))
              val branchNames: Seq[String] = allRefs
                .filterNot(_.startsWith("tag: "))



              if (isoDate.isDefined) {
                Some(SlogLine(branchNames = branchNames, tagNames = tagNames, sha1 = sha1su, date = isoDate.get))
              } else {
                None
              }
            })

          }
          case f: Failure => {
            logger.warn("", new IllegalStateException("failure: " + f.toString() + " >" + orgLines + "<"))
            Nil

          }
          case Error(msg, next) => {
            logger.warn("", new IllegalStateException("Syntax error - " + msg + " >" + orgLines + "<"))
            Nil

          }
        }
      }

    }
  }

  object TagLogParser extends RegexParsers with LazyLogging {
    override val skipWhitespace = false

    def doParse(s: String, orgLines: Seq[String]): Seq[GitTagWithDate] = {
      def parts = " (" ~> "[^\\)]+".r <~ ")" ^^ (term => term.split(", ").toSeq)

      def isoDate = isoDateR ^^ (term => parseIsoDateOpt(term))

      val p = isoDate ~ parts

      parseAll(p, s) match {
        case Success(vp, v) => {
          val isoDate = vp._1

          val tagNames = vp._2
            .filter(_.startsWith("tag: "))
            .map(_.replaceFirst("^tag: ", ""))
          if (isoDate.isDefined) {
            tagNames.map(name => GitTagWithDate(name, isoDate.get))
          } else {
            Nil
          }
        }
        case f: Failure => {
          logger.warn("", new IllegalStateException("failure: " + f.toString() + " >" + orgLines + "<"))
          Nil

        }
        case Error(msg, next) => {
          logger.warn("", new IllegalStateException("Syntax error - " + msg + " >" + orgLines + "<"))
          Nil

        }
      }
    }
  }
}
