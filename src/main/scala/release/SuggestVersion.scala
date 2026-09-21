package release

import release.Starter.ExitCode

object SuggestVersion {
  private val versionPattern = "[a-zA-Z0-9][a-zA-Z0-9._+\\-]*".r

  def suggest(commitRef: String, tagName: String, projectVersion: Option[String], externalTag: String = ""): (String, ExitCode) = {
    Option(externalTag).filterNot(_.isBlank) match {
      case Some(version) => (normalizeExplicitVersion(version), 0)
      case None =>
        val suggested = nonBlank(tagName)
          .flatMap(normalizeVersionRef)
          .orElse(nonBlank(commitRef).flatMap(normalizeVersionRef))
          .orElse(projectVersion.flatMap(nonBlank))
          .orElse(nonBlank(commitRef).flatMap(snapshotVersionFromBranch))

        suggested.map((_, 0)).getOrElse(("main-SNAPSHOT", 0))
    }
  }

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

  /** A branch name is only used directly as a release version if it explicitly denotes one. Other branch names are considered later, after
    * the project version, and converted to snapshot versions.
    */
  private def normalizeVersionRef(value: String): Option[String] = {
    val withoutGitPrefix = value.replaceFirst("^refs/tags/", "")
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
      .filter(Version.parseSloppy(_).isOrdinal)
  }

  private def snapshotVersionFromBranch(value: String): Option[String] = {
    val branchName = value
      .replaceFirst("^refs/heads/", "")
      .replaceAll("[^a-zA-Z0-9._+\\-]+", "-")
      .replaceAll("^-+|-+$", "")

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
