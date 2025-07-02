package release

import release.Lint.{BranchTagMerge, fiCodeVersionMismatch, fiCodeVersionMismatchNoTag, fiError, fiWarn}
import release.Term.{info, warn, warnSoft}

import java.io.PrintStream
import java.util.concurrent.atomic.AtomicBoolean

object LintMaven {
  def lintProjectVersion(out: PrintStream, opts: Opts, version: String, warnExit: AtomicBoolean, errorExit: AtomicBoolean,
                         tagBranchInfo: Option[BranchTagMerge], allGitTags: Seq[String], isShop:Boolean): Seq[Lint.UniqCode] = {
    out.println(info(s"    $version", opts))
    if (PomMod.isUnknownVersionPattern(version) && tagBranchInfo.isDefined && tagBranchInfo.get.branchName.getOrElse("").startsWith("release/")) {

      out.println(warnSoft(s" unknown release/version pattern: $version ${PomMod.trySuggestKnownPattern(version).getOrElse("-")}"
        , opts, limit = Lint.lineMax))
    }
    // TODO check if current version is older then released tags
    val allGitTagVersions = Sgit.stripVersionPrefix(allGitTags)
    val allGitTagVersionsP = allGitTagVersions.map(Version.parseSloppy).filter(_.isOrdinal).sorted.distinct
    val v = Version.parseSloppy(version)
    if (v.isUndef && v.hasNoDigits && Lint.isValidTag(tagBranchInfo)) {
      // TODO describe why this is important
      out.println(warn(s" version »${version}« is not recommended, please use at lease a single digit e.g. 1.0.0 ${fiWarn}"
        , opts, limit = Lint.lineMax))
      // TODO skip
      warnExit.set(true)
    }
    val latestKnownVersion = allGitTagVersionsP.filterNot(_ == v).lastOption
    val usedSkips: Seq[Lint.UniqCode] = if (!Lint.isValidTag(tagBranchInfo)) {
      if (!v.isSnapshot) {
        // TODO non snapshots are only allowed in tags, because if someone install it to its local repo this will lead to problems
        out.println(warn(s" version »${version}« must be a SNAPSHOT; non snapshots are only allowed in tags ${fiWarn}"
          , opts, limit = Lint.lineMax))
        warnExit.set(true)
        // TODO skip
        ()
      }
      val asVersion = v.removeSnapshot()
      if (allGitTagVersions.contains(asVersion.rawInput)) {
        val suggested = PomMod.suggestNextReleaseBy(v.rawInput, v.rawInput) + "-SNAPSHOT"
        val msg = s" tag v${asVersion.rawInput} is already existing. Please increment to next version e.g. ${suggested} ${fiWarn} ${fiCodeVersionMismatchNoTag}"
        if (opts.lintOpts.skips.contains(fiCodeVersionMismatchNoTag)) {
          out.println(warnSoft(msg, opts, limit = Lint.lineMax))
          Seq(fiCodeVersionMismatchNoTag)
        } else {
          out.println(warn(msg, opts, limit = Lint.lineMax))
          warnExit.set(true)
          Nil
        }
      } else {
        Nil
      }
    } else {
      if (v.isOrdinal) {
        val expectedNextMajor = latestKnownVersion.map(_.nextVersionResetZero((1, 0, 0)))
        val expectedNextMinor = latestKnownVersion.map(_.nextVersionResetZero((0, 1, 0)))
        val expectedNextPatch = latestKnownVersion.map(_.nextVersionResetZero((0, 0, 1)))
        val allExpected = Seq(expectedNextPatch, expectedNextMinor, expectedNextMajor).flatten
        val expectedVersions = {
          if (latestKnownVersion.isDefined) {
            "; expected one of: »" + allExpected.map(_.format()).mkString(", ") +
              s"«. Because latest numeric version is: »${latestKnownVersion.get.format()}«"
          } else {
            ""
          }
        }
        if (!allExpected.map(_.primarys).contains(v.primarys) && allExpected.nonEmpty) {
          val msg = s" unexpected version increment: ${v.rawInput}${expectedVersions}" // TODO improve message
          // tag gap v1.2.3 to v5.0.0 has missing versions (2, 3, 4).
          // TODO skip version range? (1,)
          // https://maven.apache.org/enforcer/enforcer-rules/versionRanges.html
          out.println(warnSoft(msg, opts, limit = Lint.lineMax))
        }

      }
      Nil
    }

    val mismatchResult = Lint.versionMismatches(version, tagBranchInfo, isShop)
    if (mismatchResult.isMismatch) {
      val bool = opts.lintOpts.skips.contains(fiCodeVersionMismatch)
      if (bool) {
        out.println(warnSoft(mismatchResult.msg, opts, limit = Lint.lineMax))
        Seq(fiCodeVersionMismatch) ++ usedSkips
      } else {
        out.println(warn(mismatchResult.msg, opts, limit = Lint.lineMax))
        warnExit.set(true)
        usedSkips
      }
    } else {
      usedSkips
    }
  }

}
