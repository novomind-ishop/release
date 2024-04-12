package release

import release.Lint.{BranchTagMerge, fiCodeVersionMismatch, fiWarn}
import release.Term.{info, warn, warnSoft}

import java.io.PrintStream
import java.util.concurrent.atomic.AtomicBoolean

object LintMaven {
  def lintProjectVersion(out: PrintStream, opts: Starter.Opts, version: String, warnExit: AtomicBoolean, errorExit: AtomicBoolean,
                         tagBranchInfo: Option[BranchTagMerge], allGitTags: Seq[String]): Seq[Lint.UniqCode] = {
    out.println(info(s"    $version", opts))
    // TODO non snapshots are only allowed in tags, because if someone install it to its local repo this will lead to problems
    // TODO check if current version is older then released tags
    if (!Lint.isValidTag(tagBranchInfo)) {
      val as = Sgit.stripVersionPrefix(allGitTags)
      val asVersion = Version.parseSloppy(version).removeSnapshot()
      if (as.contains(asVersion.rawInput)) {
        out.println(warn(s" tag v${asVersion.rawInput} is already existing ${fiWarn}", opts, limit = Lint.lineMax))
        warnExit.set(true)
        // TODO skip
      }
    }

    if (Lint.versionMissmatches(version, tagBranchInfo)) {
      val msg = s" $version != ${Util.show(tagBranchInfo)} ${fiWarn} ${fiCodeVersionMismatch}"
      val bool = opts.lintOpts.skips.contains(fiCodeVersionMismatch)
      if (bool) {
        out.println(warnSoft(msg, opts, limit = Lint.lineMax))
        Seq(fiCodeVersionMismatch)
      } else {
        out.println(warn(msg, opts, limit = Lint.lineMax))
        warnExit.set(true)
        Nil
      }
    } else {
      Nil
    }
  }

}
