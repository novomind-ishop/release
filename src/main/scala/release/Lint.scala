package release

import com.google.common.base.Stopwatch
import release.Starter.Opts
import release.Term._

import java.io.{File, PrintStream}
import java.util.concurrent.atomic.AtomicBoolean

object Lint {

  def run(out: PrintStream, err: PrintStream, opts: Starter.Opts,
          file: File = new File(".").getAbsoluteFile): Int = {
    out.println()

    // TODO handle --simple-chars
    // TODO handle --no-color
    val color = opts.colors

    out.println(info(center("[ lint ]"), color))
    val stopwatch = Stopwatch.createStarted()
    try {
      // https://github.com/hadolint/hadolint
      // https://polaris.docs.fairwinds.com/infrastructure-as-code/

      val warnExitCode = 42
      val lineLimit = 10_000
      println(info("    " + file.getAbsolutePath, color, lineLimit))
      val warnExit = new AtomicBoolean(false)
      val files = file.listFiles()
      if (files == null || files.isEmpty) {
        out.println(error(s"E: NO FILES FOUND in ${file.getAbsolutePath}", color))
        out.println(error(center("[ end of lint ]"), color))
        return 1
      } else {
        val sgit = Sgit(file, doVerify = false, out = System.out, err = System.err, checkExisting = true, gitBin = None, opts = Opts())
        out.println(info("    ✅ git version: " + sgit.version(), color))
        out.println(info("--- check clone config / no shallow clone @ git ---", color))
        if (sgit.isShallowClone) {
          out.println(warn(" shallow clone detected \uD83D\uDE2C", color))
          out.println(warn(" % git rev-parse --is-shallow-repository # returns true", color))
          out.println(warn("   We do not want shallow clones because the commit id used in runtime", color))
          out.println(warn("   info will not point to a known commit", color))
          out.println(warn("   on Gitlab, change 'Settings' -> 'CI/CD' -> 'General pipelines' ->", color))
          out.println(warn("     'Git shallow clone' to 0 or blank", color))
          warnExit.set(true)
        } else {
          out.println(info("    ✅ NO shallow clone", color))
        }
        out.println(info("--- .gitattributes @ git ---", color))
        out.println(info("--- .gitignore @ git ---", color))
        out.println(info("--- list-remotes @ git ---", color))
        val remotes = sgit.listRemotes()
        if (remotes.isEmpty) {
          out.println(warn(" NO remotes found \uD83D\uDE2C", color))
          out.println(warn(" % git remote -v # returns nothing", color))
          warnExit.set(true)
        } else {
          remotes.foreach(r => out.println(info("      remote: " + r, useColor = color, limit = lineLimit)))
        }

        out.println(info("--- gitlabci.yml @ gitlab ---", color))
        out.println(info("    WIP ci path: " + System.getenv("CI_CONFIG_PATH"), color))

        if (files.toSeq.exists(_.getName == "pom.xml")) {

          out.println(info("--- .mvn @ maven ---", color))
          out.println(info("    WIP", color))
          out.println(info("--- check for snapshots @ maven ---", color))
          out.println(info("    WIP", color))
          out.println(info("--- suggest dependency updates / configurable @ maven ---", color))
          out.println(info("    WIP", color))
          out.println(info("--- dep.tree @ maven ---", color))
          out.println(info("    WIP", color))
        }
        if (files.toSeq.exists(_.getName == "build.sbt")) {
          out.println(info("--- ??? @ sbt ---", color))
          out.println(info("    WIP", color))
        }
        out.println()
        files.toSeq.sortBy(_.toString)
          .take(5).foreach(f => out.println(f.toPath.normalize().toAbsolutePath.toFile.getAbsolutePath))
        val timerResult = if (opts.lintOpts.showTimer) {
          " - " + stopwatch.elapsed().toString
        } else {
          ""
        }
        out.println(info(center("[ end of lint" + timerResult + " ]"), color))

        if (warnExit.get()) {
          out.println(error("exit 42 - because lint found warnings, see above ❌", color))
          return warnExitCode
        } else {
          return 0
        }

      }

    } catch {
      case e: Exception => {
        e.printStackTrace()
        return 2
      }
    }
    1
  }

}
