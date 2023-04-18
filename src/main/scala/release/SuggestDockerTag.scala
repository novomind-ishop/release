package release

import com.google.common.base.Strings

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object SuggestDockerTag {
    def findTagname(refName: String, tagName: String): Try[Try[String]] = {
    val ref = Strings.nullToEmpty(refName)
    val tag = Strings.nullToEmpty(tagName)
    if (ref.isBlank && tag.isBlank) {
      Failure(new IllegalStateException("nor tag nor ref"))
    } else if (!ref.isBlank && !tag.isBlank) {
      if (tag.matches(ProjectMod.Version.semverDockertagPattern.regex)) {
        Success(Success(tag))
      } else {
        Success(Failure(new IllegalStateException(s"» ${tag} « is no valid tag name. This could lead to build problems later. " +
          s"A tag must match the pattern » ${ProjectMod.Version.semverDockertagPattern.regex} «")))
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
      case fe if fe.matches(ProjectMod.Version.semverDockertagPattern.regex) => (fe.substring(1), 0)
      case fe => (fe.replaceAll("[\\:/]+", "-")
        .replaceAll("^[-]+", "")
        .replaceAll("[-]+$", ""), 0)
    }

  }

}
