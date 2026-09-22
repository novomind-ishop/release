package release

import release.Starter.ExitCode

object SuggestVersion {
  private val versionPattern = "[a-zA-Z0-9][a-zA-Z0-9._+\\-]*".r

  def suggest(commitRef: String, tagName: String, projectVersion: => Option[String], externalTag: String = "",
      branchNames: Seq[String] = Nil, tagNames: Seq[String] = Nil): (String, ExitCode) = {
    Option(externalTag).filterNot(_.isBlank) match {
      case Some(version) =>
        val normalized = normalizeExplicitVersion(version)
        ensureTagDoesNotExist(normalized, currentTag = null, tagNames)
        (normalized, 0)
      case None =>
        lazy val fallbackProjectVersion = projectVersion.flatMap(nonBlank)
        val suggested = nonBlank(tagName)
          .flatMap(normalizeVersionRef)
          .map { version =>
            ensureTagDoesNotExist(version, tagName, tagNames)
            version
          }
          .orElse(nonBlank(commitRef).filter(isQaMain).flatMap(_ => fallbackProjectVersion))
          .orElse(nonBlank(commitRef).flatMap(snapshotVersionFromBranch))
          .orElse(fallbackProjectVersion)

        suggested.map((_, 0)).getOrElse(("main-SNAPSHOT", 0))
    }
  }

  private def isQaMain(ref: String): Boolean =
    ref.stripPrefix("refs/heads/") == "qa/main"

  private def ensureTagDoesNotExist(version: String, currentTag: String, tagNames: Seq[String]): Unit = {
    val existing = tagNames
      .filterNot(tag => sameGitTagRef(tag, currentTag))
      .find(tag => normalizeVersionRef(tag).contains(version))
    existing.foreach { tag =>
      throw new IllegalArgumentException(s"version $version already exists as Git tag $tag")
    }
  }

  private def sameGitTagRef(left: String, right: String): Boolean =
    Option(left).map(_.stripPrefix("refs/tags/")) == Option(right).map(_.stripPrefix("refs/tags/"))

  private def nonBlank(value: String): Option[String] =
    Option(value).map(_.trim).filter(_.nonEmpty)

  private def normalizeExplicitVersion(value: String): String = {
    val normalized = stripVersionPrefix(value)
    if (versionPattern.matches(normalized)) {
      normalized
    } else {
      val printable = value
        .replaceAll("\\r", "␍")
        .replaceAll("\\n", "␊")
        .replaceAll("[\\p{Cntrl}\\p{Space}]", "\uFFFD")
      throw new IllegalArgumentException(
        s"invalid version »$printable«; versions must match pattern »${versionPattern.regex}«")
    }
  }

  /** A branch name is only used directly as a release version if it explicitly denotes one. Other branch names are converted to snapshot
    * versions.
    */
  private def normalizeVersionRef(value: String): Option[String] = {
    val withoutGitPrefix = value.replaceFirst("^refs/(tags|heads)/", "")
    val candidate =
      if (withoutGitPrefix.startsWith("release/")) {
        Some(withoutGitPrefix.stripPrefix("release/"))
      } else if (withoutGitPrefix.startsWith("v")) {
        Some(withoutGitPrefix)
      } else if (versionPattern.matches(withoutGitPrefix)) {
        Some(withoutGitPrefix)
      } else {
        None
      }

    candidate
      .map(stripVersionPrefix)
      .filter(versionPattern.matches)
      .filterNot(_.matches("[0-9]+(?:\\.x|x)"))
      .filter(Version.parseSloppy(_).isOrdinal)
  }

  private def snapshotVersionFromBranch(value: String): Option[String] = {
    val ref = value.stripPrefix("refs/heads/")
    val parts = ref.split("/").toSeq
    val ticket = "(?<![a-zA-Z0-9])[A-Z]+-[0-9]+(?:_[0-9]+)?(?![0-9])".r
      .findFirstIn(ref).map(_.toUpperCase(java.util.Locale.ROOT))
    val linePattern = "(?i)^(?:(?:feature-|qa|release|support|v|x))?([0-9]+)(?:\\.x|x)?(?:-release)?$".r
    val line = parts.collectFirst { case linePattern(number) => s"${number}x" }
    val context = parts.collectFirst {
      case "qa-backup" => "qa-backup"
      case part if part == "qa" || part == "qamain" || part.matches("qa[0-9]+x?") => "qa"
      case part if part == "release" || part.matches("release[0-9]+x?") => "release"
      case part if part == "support" || part.matches("support[0-9]+x?") => "support"
    }
    val releaseCandidate = parts match {
      case Seq("release", linePattern(number)) => Some(s"${number}x-RC")
      case _ => None
    }
    val name = releaseCandidate.getOrElse {
      ticket match {
        case Some(id) => (context.toSeq ++ line.toSeq :+ id).mkString("-")
        case None => parts.map {
            case linePattern(number) => s"${number}x"
            case part => part
          }.filterNot(part => part == "feature" || part == "frature").mkString("-")
      }
    }
    val branchName = name
      .replaceAll("[^a-zA-Z0-9._+\\-]+", "-")
      .replaceAll("-+", "-")
      .replaceAll("^[^a-zA-Z0-9]+|-+$", "")

    Option(branchName)
      .filter(_.nonEmpty)
      .map(Version.removeTrailingSnapshots)
      .map(Version.applySnapshot)
  }

  private def stripVersionPrefix(value: String): String = {
    val withoutPrefix = value.stripPrefix("v")
    if (withoutPrefix != value && Version.parseSloppy(withoutPrefix).isOrdinal) {
      withoutPrefix
    } else {
      value
    }
  }
}
