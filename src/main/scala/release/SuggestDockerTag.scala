package release

import com.google.common.base.Strings
import release.Starter.ExitCode

import java.text.Normalizer
import scala.util.{Failure, Success, Try}

object SuggestDockerTag {
  val masterPattern = "[a-zA-Z0-9][a-zA-Z0-9_\\-\\.]{0,127}".r

  def normlize(input: String): String = Normalizer
    .normalize(input, Normalizer.Form.NFD)
    .replaceAll("[^\\p{ASCII}]", "")
    .replaceAll("[^a-zA-Z0-9_\\-\\.]+", "")

  def findTagname(refName: String, tagName: String, projectVersion: Option[String], hasDockerfiles:Boolean): Try[Try[String]] = {
    if (hasDockerfiles) {
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
        if (suggested.matches(Version.semverGitTagForDockerTagPattern.regex)) {
          Success(Success(tag))
        } else {
          Success(Failure(new IllegalStateException(s"auto suggested docker tag »\u00A0${suggested}\u00A0« is no valid docker tag name. " +
            s"This could lead to build problems later. " +
            s"A git tag must match the pattern »\u00A0${Version.semverGitTagForDockerTagPattern.regex}\u00A0« to suggest valid docker tags. " +
            "It is also possible to export an environment variable e.g. HARBOR_TAG")))
        }

      } else {
        Failure(new IllegalStateException("no tag"))
      }
    } else {
      Failure(new IllegalStateException("no Dockerfiles"))
    }

  }

  def suggest(commitRef: String, tagName: String, projectVersion: Option[String], externalTag: String = ""): (String, ExitCode) = {
    if (!Strings.nullToEmpty(externalTag).isBlank) {
      if (masterPattern.matches(externalTag)) {
        (externalTag, 0)
      } else {
        val msg = s"invalid docker tag »${externalTag.replaceAll("\\r", "␍").replaceAll("\\n", "␊")
          .replaceAll("[\\p{Cntrl}\\p{Space}]", "\uFFFD")}«; " +
          s"docker tags must match pattern »${masterPattern.regex}«. This will lead " +
          s"to »Error response from daemon: invalid reference format« from docker"
        throw new IllegalArgumentException(msg)
      }
    } else {
      suggestInner(commitRef, commitRef, tagName, projectVersion)
    }
  }

  def akaVersion(in: String): String = {
    in.replaceAll("\\W", "_").replaceAll("[_]+", "_")
      .replaceFirst("^_", "").replaceFirst("_$", "")
  }

  def suggestInner(inn: String, org: String, tagName: String, projectVersion: Option[String]): (String, ExitCode) = {

    def fallback(innn: String): (String, Int) = {
      val suffix = "_" + Util.hashMurmur3_32_fixed(org) + "_TEMP"
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
          case fe if fe.matches(Version.semverGitTagForDockerTagPattern.regex) => {
            val withoutLeadingV = fe.substring(1)
            if (projectVersion.isDefined && withoutLeadingV != projectVersion.get) {
              (withoutLeadingV + "_aka_" + akaVersion(projectVersion.get), 0)
            } else {
              (withoutLeadingV, 0)
            }
          }
          case fe if tagName != null && fe.matches(Version.shopPattern.regex) => {
            (fe + "_TEMP", 0)
          }
          case fe if tagName != null &&  fe.matches(Version.betaTagPattern.regex) => {
            (fe.replaceFirst("BETA-","") + "_TEMP", 0)
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
