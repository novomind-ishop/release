package release.docker

import com.google.common.base.Strings
import release.lint.{Lint, Lint as opts}
import release.{FileUtils, OneTimeSwitch, Opts, Term, Util}

import java.io.PrintStream
import java.nio.file.Path

object Dockerfile {

  def allowedDockerHostnames(allowedFromHosts: String): Seq[String] = {
    if (Strings.isNullOrEmpty(allowedFromHosts)) {
      Seq(".*")
    } else {
      val sp = Strings.nullToEmpty(allowedFromHosts).split(",").map(_.trim).toSeq
      if (sp.contains("docker.io")) {
        sp :+ ""
      } else {
        sp
      }
    }
  }

  def parseLines(fileLines: Seq[String], allowedFromHosts: String,
                 out: PrintStream, opts: Opts, warnExit: OneTimeSwitch, errorExit: OneTimeSwitch): Seq[Lint.UniqCode] = {
    val fromLines = fileLines.filter(l => l.trim.startsWith("FROM "))
    val alH = allowedDockerHostnames(allowedFromHosts)

    if (fromLines.nonEmpty) {
      fromLines.flatMap(f => {
        val rpl = f.replaceFirst("^FROM ", "").trim
        val left = rpl.replaceFirst(":.*", "")
        val hostname = if (left.contains("/")) {
          left.replaceFirst("/.*", "")
        } else {
          ""
        }

        if (alH.exists(p => hostname.matches(p))) {
          out.println(Term.info(s"          ${Lint.fiFine} ${f}", opts, Lint.lineMax))
          Nil
        } else {
          val cFi = Lint.fiCodeDockerHost(f)
          if (opts.lintOpts.skips.contains(cFi)) {
            out.println(Term.warnSoft(s"       ${Lint.fiWarnMuted} ${f} ${cFi}", opts, Lint.lineMax))
            Seq(cFi)
          } else {
            out.println(Term.warn(s"       ${Lint.fiWarn} ${f} ${cFi}", opts, Lint.lineMax))
            warnExit.trigger()
            Nil
          }
        }
      })
    } else {
      Nil
    }


  }

  def parse(f: Path, allowedFromHostsEnv: String,
            out: PrintStream, opts: Opts, warnExit: OneTimeSwitch, errorExit: OneTimeSwitch): Seq[Lint.UniqCode] = {

    val fileLines = FileUtils.readLines(f.toFile)
    parseLines(fileLines, allowedFromHostsEnv, out, opts, warnExit, errorExit)
  }
}
