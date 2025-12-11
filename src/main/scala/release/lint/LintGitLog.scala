package release.lint

import release.Opts
import release.Term.info
import release.lint.Lint.lineMax

import java.io.PrintStream

object LintGitLog {
  def doLint(subjectLinePattern: String, str: String, out: PrintStream, opts: Opts): Unit = {
    out.println(info(s"    WIP ${subjectLinePattern}", opts))
    str.lines().forEach(l => out.println(info(l, opts, limit = lineMax)))
    out.println(info(s"    WIP ${subjectLinePattern}", opts))
  }

}
