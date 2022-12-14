package release

import com.google.common.base.Stopwatch
import release.Starter.Opts
import release.Term._

import java.io.{File, PrintStream}
import java.util.concurrent.atomic.AtomicBoolean

object Lint {

  def run(out: PrintStream, err: PrintStream, opts: Starter.Opts): Unit = {
    out.println()

    // TODO handle --simple-chars
    // TODO handle --no-color


    out.println(info(center("[ lint ]")))
    val stopwatch = Stopwatch.createStarted()
    try {
      // https://github.com/hadolint/hadolint
      // https://polaris.docs.fairwinds.com/infrastructure-as-code/
      val file = new File(".").getAbsoluteFile
      val warnExitCode = 42
      println(info("    " + file.getAbsolutePath))
      val warnExit = new AtomicBoolean(false)
      val files = file.listFiles()
      if (files == null || files.isEmpty) {
        out.println(error(s"E: NO FILES FOUND in ${file.getAbsolutePath}"))
        out.println(error(center("[ end of lint ]")))
        System.exit(1)
      } else {
        val sgit = Sgit(file, doVerify = false, out = System.out, err = System.err, checkExisting = true, gitBin = None, opts = Opts())
        out.println(info("    ✅ git version: " + sgit.version()))
        out.println(info("--- check clone config / no shallow clone @ git ---"))
        if (sgit.isShallowClone) {
          out.println(warn(" shallow clone detected \uD83D\uDE2C"))
          out.println(warn(" on Gitlab, change 'Settings -> CI/CD -> General pipelines -> Git shallow clone'"))
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
        files.toSeq.take(5).foreach(f => out.println(f.toPath.normalize().toAbsolutePath.toFile.getAbsolutePath))
        out.println(info(center("[ end of lint - " + stopwatch.elapsed().toString + " ]")))
        if (warnExit.get()) {
          out.println(error("exit 42 - because lint found warnings, see above ❌"))
          System.exit(warnExitCode)
        } else {
          System.exit(0)
        }

      }

    } catch {
      case e: Exception => {
        e.printStackTrace()
        System.exit(2)
      }
    }
    System.exit(1)
  }

}
