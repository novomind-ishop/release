package release.docker

import com.google.common.base.Strings
import release.lint.Lint
import release.{FileUtils, OneTimeSwitch, Opts, Term, Util}

import java.io.PrintStream
import java.nio.file.Path

object Dockerfile {
  def parseLines(fileLines: Seq[String], allowedFromHosts: String,
                 out: PrintStream, opts: Opts, warnExit: OneTimeSwitch, errorExit: OneTimeSwitch): Seq[Lint.UniqCode] = {
    val fromLines = fileLines.filter(l => l.trim.startsWith("FROM "))
    val alH = if (Strings.isNullOrEmpty(allowedFromHosts)) {
      Seq(".*")
    } else {
      val sp = Strings.nullToEmpty(allowedFromHosts).split(",").map(_.trim).toSeq
      if (sp.contains("docker.io")) {
        sp :+ ""
      } else {
        sp
      }
    }

    if (fromLines.nonEmpty) {
      fromLines.foreach(f => {
        val rpl = f.replaceFirst("^FROM ", "").trim
        val left = rpl.replaceFirst(":.*", "")
        val hostname = if (left.contains("/")) {
          left.replaceFirst("/.*", "")
        } else {
          ""
        }

        if (alH.exists(p => hostname.matches(p))) {
          out.println(Term.info(s"          ${Lint.fiFine} ${f}", opts, Lint.lineMax))
        } else {
          out.println(Term.warnSoft(s"       ${Lint.fiWarnMuted} ${f}", opts, Lint.lineMax))
        }

      })

    }
    // docker.io aka Docker Hub is blank
    // TODO parse an check 'FROM' lines for missing hostnames
    // TODO add skip logic
    // TOOD check for FROM lines without prefix server from env
    Nil
  }

  def parse(f: Path, envs: Map[String, String],
            out: PrintStream, opts: Opts, warnExit: OneTimeSwitch, errorExit: OneTimeSwitch): Seq[Lint.UniqCode] = {
    val allowedFromHosts = envs.getOrElse("RELEASE_LINT_DOCKER_FROM_HOSTS", "") // move up

    val fileLines = FileUtils.readLines(f.toFile)
    parseLines(fileLines, allowedFromHosts, out, opts, warnExit, errorExit)


  }
}
