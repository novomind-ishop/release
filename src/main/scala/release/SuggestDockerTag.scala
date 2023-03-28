package release

import com.google.common.base.Strings

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object SuggestDockerTag {
  val tagPattern = "^v[0-9]+\\.[0-9]+\\.[0-9]+$"

  def findTagname(refName: String, tagName: String): Try[Try[String]] = {
    val ref = Strings.nullToEmpty(refName)
    val tag = Strings.nullToEmpty(tagName)
    if (ref.isBlank && tag.isBlank) {
      Failure(new IllegalStateException("nor tag nor ref"))
    } else if (!ref.isBlank && !tag.isBlank) {
      if (tag.matches(tagPattern)) {
        Success(Success(tag))
      } else {
        Success(Failure(new IllegalStateException(s"» ${tag} « is no valid tag name. This could lead to build problems later.")))
      }

    } else {
      Failure(new IllegalStateException("no tag"))
    }
  }

  def suggest(inn: String): (String, Int) = {
    Strings.nullToEmpty(inn).trim.toLowerCase() match {
      case "" => ("latest", 0)
      case "main" => ("latest", 0)
      case "master" => ("latest", 0)
      case fe if fe.startsWith("feature/") => (fe.replaceFirst("^feature/", ""), 0)
      case fe if fe.startsWith("release/") => (fe.replaceFirst("^release/", ""), 0)
      case fe if fe.matches(tagPattern) => (fe.substring(1), 0)
      case fe => (fe.replaceAll("[\\:/]+", "-")
        .replaceAll("^[-]+", "")
        .replaceAll("[-]+$", ""), 0)
    }

  }

}
