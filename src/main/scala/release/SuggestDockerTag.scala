package release

import com.google.common.base.Strings

object SuggestDockerTag {
  def suggest(inn: String): (String, Int) = {
    val tagPatter = "^v[0-9]+\\.[0-9]+\\.[0-9]+$"
    Strings.nullToEmpty(inn).trim.toLowerCase() match {
      case "" => ("latest", 0)
      case "main" => ("latest", 0)
      case "master" => ("latest", 0)
      case fe if fe.startsWith("feature/") => (fe.replaceFirst("^feature/", ""), 0)
      case fe if fe.startsWith("release/") => (fe.replaceFirst("^release/", ""), 0)
      case fe if fe.matches(tagPatter) => (fe.substring(1), 0)
      case fe => (fe.replaceAll("[\\:/]+", "-")
        .replaceAll("^[-]+", "")
        .replaceAll("[-]+$", ""), 0)
    }


  }

}
