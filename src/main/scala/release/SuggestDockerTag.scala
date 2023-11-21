package release

import com.google.common.base.Strings
import com.google.common.hash.Hashing
import release.ProjectMod.Version

import java.nio.charset.StandardCharsets
import java.text.Normalizer
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object SuggestDockerTag {
  val masterPattern = "[a-zA-Z0-9][a-zA-Z0-9_\\-\\.]{0,127}".r

  def normlize(input: String): String = Normalizer
    .normalize(input, Normalizer.Form.NFD)
    .replaceAll("[^\\p{ASCII}]", "")
    .replaceAll("[^a-zA-Z0-9_\\-\\.]+", "")

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
      if (suggested.matches(ProjectMod.Version.semverGitTagForDockerTagPattern.regex)) {
        Success(Success(tag))
      } else {
        Success(Failure(new IllegalStateException(s"»\u00A0${suggested}\u00A0« is no valid git tag name. This could lead to build problems later. " +
          s"A git tag must match the pattern »\u00A0${ProjectMod.Version.semverGitTagForDockerTagPattern.regex}\u00A0«")))
      }

    } else {
      Failure(new IllegalStateException("no tag"))
    }
  }

  def suggest(commitRef: String, tagName: String, projectVersion: Option[String], externalTag: String = ""): (String, Int) = {
    if (!Strings.nullToEmpty(externalTag).isBlank) {
      if (masterPattern.matches(externalTag)) {
        (externalTag, 0)
      } else {
        throw new IllegalArgumentException(s"invalid docker tag »\u00A0${externalTag}\u00A0«; docker tags must match pattern ${masterPattern.regex}")
      }
    } else {
      suggestInner(commitRef, commitRef, tagName, projectVersion)
    }
  }

  def akaVersion(in: String): String = {
    in.replaceAll("\\W", "_").replaceAll("[_]+", "_")
      .replaceFirst("^_", "").replaceFirst("_$", "")
  }

  def suggestInner(inn: String, org: String, tagName: String, projectVersion: Option[String]): (String, Int) = {

    def fallback(innn: String): (String, Int) = {
      val suffix = "_" + Hashing.murmur3_32_fixed().hashString(org, StandardCharsets.UTF_8) + "_TEMP"
      val max = 127 - suffix.length
      val str = normlize(innn.replaceAll("[\\:/]+", "-")).toLowerCase()
        .replaceAll("^[-]+", "")
        .replaceAll("[-]+$", "")
      val str1 = str.substring(0, Math.min(max, str.length)) + suffix
      (str1.replaceAll("^[^a-zA-Z0-9]+", ""), 0)
    }

    val trimmed = Strings.nullToEmpty(inn).trim
    val result = trimmed match {
      case "" => ("latest", 0)
      case fe if fe.startsWith("feature/") => suggestInner(fe.replaceFirst("^feature/v?", ""), org, tagName, projectVersion)
      case fe if Version.shopBranchPattern.matches(fe) =>
        (Version.parse(fe.replaceFirst("^release/", "")).formatShop(), 0)
      case fe if fe.startsWith("release/") => suggestInner(fe.replaceFirst("^release/v?", ""), org, tagName, projectVersion)
      case k if masterPattern.matches(k) => {
        k match {
          case "main" => ("latest", 0)
          case "master" => ("latest", 0)
          case fe if fe.matches(ProjectMod.Version.semverGitTagForDockerTagPattern.regex) => {
            val withoutLeadingV = fe.substring(1)
            if (projectVersion.isDefined && withoutLeadingV != projectVersion.get) {
              (withoutLeadingV + "_aka_" + akaVersion(projectVersion.get), 0)
            } else {
              (withoutLeadingV, 0)
            }
          }
          case fe => fallback(fe)
        }
      }
      case fe => fallback(fe)
    }
    if (masterPattern.matches(result._1)) {
      result
    } else {
      fallback(trimmed)
    }

  }

}
