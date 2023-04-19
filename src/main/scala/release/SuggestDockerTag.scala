package release

import com.google.common.base.Strings
import com.google.common.hash.Hashing

import java.nio.charset.StandardCharsets
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object SuggestDockerTag {
  def findTagname(refName: String, tagName: String, projectVersion: Option[String]): Try[Try[String]] = {
    val ref = Strings.nullToEmpty(refName)
    val tag = Strings.nullToEmpty(tagName)
    if (ref.isBlank && tag.isBlank) {
      Failure(new IllegalStateException("nor tag nor ref"))
    } else if (!ref.isBlank && !tag.isBlank) {
      val suggested = if (projectVersion.isDefined) {
        "v" + suggest(refName, tagName, projectVersion)._1
      } else {
        tag
      }
      if (suggested.matches(ProjectMod.Version.semverDockertagPattern.regex)) {
        Success(Success(tag))
      } else {
        Success(Failure(new IllegalStateException(s"»\u00A0${suggested}\u00A0« is no valid tag name. This could lead to build problems later. " +
          s"A tag must match the pattern »\u00A0${ProjectMod.Version.semverDockertagPattern.regex}\u00A0«")))
      }

    } else {
      Failure(new IllegalStateException("no tag"))
    }
  }

  def suggest(commitRef: String, tagName: String, projectVersion: Option[String]): (String, Int) = {
    suggestInner(commitRef, commitRef, tagName, projectVersion)
  }


  def akaVersion(in:String):String = {
    in.replaceAll("\\W", "_").replaceAll("[_]+", "_")
      .replaceFirst("^_", "").replaceFirst("_$", "")
  }

  def suggestInner(inn: String, org: String, tagName: String, projectVersion: Option[String]): (String, Int) = {
    Strings.nullToEmpty(inn).trim match {
      case "" => ("latest", 0)
      case "main" => ("latest", 0)
      case "master" => ("latest", 0)
      case fe if fe.startsWith("feature/") => suggestInner(fe.replaceFirst("^feature/v?", ""), org, tagName, projectVersion)
      case fe if fe.startsWith("release/") => suggestInner(fe.replaceFirst("^release/v?", ""), org, tagName, projectVersion)
      case fe if fe.matches(ProjectMod.Version.semverDockertagPattern.regex) => {

        val withoutLeadingV = fe.substring(1)
        if (projectVersion.isDefined && withoutLeadingV != projectVersion.get) {
          (withoutLeadingV + "_aka_" + akaVersion(projectVersion.get), 0)
        } else {
          (withoutLeadingV, 0)
        }

      }
      case fe => {
        val suffix = "_" + Hashing.murmur3_32_fixed().hashString(org, StandardCharsets.UTF_8) + "_TEMP"
        (fe.toLowerCase().replaceAll("[\\:/]+", "-")
          .replaceAll("^[-]+", "")
          .replaceAll("[-]+$", "") + suffix, 0)
      }
    }
  }

}
