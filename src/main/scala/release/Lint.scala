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


    out.println(info(center("[ lint ]")))
    val stopwatch = Stopwatch.createStarted()
    try {
      // https://github.com/hadolint/hadolint
      // https://polaris.docs.fairwinds.com/infrastructure-as-code/

      val warnExitCode = 42
      println(info("    " + file.getAbsolutePath, 10_000))
      val warnExit = new AtomicBoolean(false)
      val files = file.listFiles()
      if (files == null || files.isEmpty) {
        out.println(error(s"E: NO FILES FOUND in ${file.getAbsolutePath}"))
        out.println(error(center("[ end of lint ]")))
        return 1
      } else {
        val sgit = Sgit(file, doVerify = false, out = System.out, err = System.err, checkExisting = true, gitBin = None, opts = Opts())
        out.println(info("    ✅ git version: " + sgit.version()))
        out.println(info("--- check clone config / no shallow clone @ git ---"))
        if (sgit.isShallowClone) {
          out.println(warn(" shallow clone detected \uD83D\uDE2C"))
          out.println(warn(" we do not want shallow clones because the commit id used in runtime"))
          out.println(warn(" info will not point to a known commit"))
          out.println(warn(" on Gitlab, change 'Settings' -> 'CI/CD' -> 'General pipelines' ->"))
          out.println(warn("   'Git shallow clone' to 0 or blank"))
          warnExit.set(true)
        } else {
          out.println(info("    ✅ NO shallow clone"))
        }
        out.println(info("--- .gitattributes @ git ---"))
        out.println(info("--- .gitignore @ git ---"))
        out.println(info("--- gitlabci.yml @ gitlab ---"))
        out.println(info("    WIP ci path: " + System.getenv("CI_CONFIG_PATH")))

        if (files.toSeq.exists(_.getName == "pom.xml")) {

          out.println(info("--- .mvn @ maven ---"))
          out.println(info("    WIP"))
          out.println(info("--- check for snapshots @ maven ---"))
          out.println(info("    WIP"))
          out.println(info("--- suggest dependency updates / configurable @ maven ---"))
          out.println(info("    WIP"))
          out.println(info("--- dep.tree @ maven ---"))
          out.println(info("    WIP"))
        }
        if (files.toSeq.exists(_.getName == "build.sbt")) {
          out.println(info("--- ??? @ sbt ---"))
          out.println(info("    WIP"))
        }
        out.println()
        files.toSeq.sortBy(_.toString)
          .take(5).foreach(f => out.println(f.toPath.normalize().toAbsolutePath.toFile.getAbsolutePath))
        val timerResult = if (opts.lintOpts.showTimer) {
          " - " + stopwatch.elapsed().toString
        } else {
          ""
        }
        out.println(info(center("[ end of lint" + timerResult + " ]")))

        if (warnExit.get()) {
          out.println(error("exit 42 - because lint found warnings, see above ❌"))
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