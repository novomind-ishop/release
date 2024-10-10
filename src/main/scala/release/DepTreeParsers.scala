package release

import com.typesafe.scalalogging.LazyLogging
import release.ProjectMod.Gav3

import scala.util.parsing.combinator.RegexParsers

object DepTreeParsers {
  def parseGavsOnly(tree: String): Seq[Gav3] = {
    try {
      TagLogParser.doParse(tree)
    } catch {
      case e: Exception => {
        e.printStackTrace()
        Nil
      }
    }

  }

  object TagLogParser extends RegexParsers with LazyLogging {
    override val skipWhitespace = false

    def doParse(s: String): Seq[Gav3] = {
      val w = "[^:\\n\\r]+".r <~ opt("[:]".r) ^^ (term => term)

      def parts = opt("[ |\\+\\-\\\\]+".r) ~> rep1(w) <~ opt("[\\n\\r]+".r) ^^ (term => {
        term.size match {
          case 4 => Gav3(groupId = term(0), artifactId = term(1), version = Some(term(3)))
          case 5 => Gav3(groupId = term(0), artifactId = term(1), version = Some(term(3)))
          case 6 => Gav3(groupId = term(0), artifactId = term(1), version = Some(term(4)))
          case _ => throw new IllegalStateException(s"unknown parts: ${term}")
        }

      })

      val p = rep(parts)

      parseAll(p, s) match {
        case Success(vp, v) => {
          vp
        }
        case f: Failure => {
          logger.warn("", new IllegalStateException("failure: " + f.toString() + " >" + s + "<"))
          Nil

        }
        case Error(msg, next) => {
          logger.warn("", new IllegalStateException("Syntax error - " + msg + " >" + s + "<"))
          Nil

        }
      }
    }
  }
}