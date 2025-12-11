package release.lint

import release.Opts
import release.Term.info
import release.lint.Lint.lineMax

import java.io.PrintStream

object LintGit {
  def lintBranchActivity(branchNames: Seq[String], contributorMailboxes: Seq[String],
                         out: PrintStream, opts: Opts): Unit = {
    val branchMsg = if (branchNames == Nil) {
      ""
    } else {
      s" - ${branchNames.mkString(", ")}"
    }
    // TODO limits? maybe 4 branches per mailbox? filter branchnames? group by first nonLetter?
    out.println(info(s"    active branch count: ${branchNames.size}${branchMsg}", opts, limit = lineMax))

  }

}
