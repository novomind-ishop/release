package release

import com.google.common.base.Stopwatch
import release.Starter.Opts
import release.Term._

import java.io.{File, PrintStream}

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
      println(info("    " + file.getAbsolutePath))
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
          out.println(warn("    shallow clone detected \uD83D\uDE2C"))
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
        out.println(warn("warn demo"))
        out.println(error("demo chars \uD83D\uDD14 ✅ ❌ \uD83D\uDE2C️ ⛔"))
        out.println()
        files.toSeq.foreach(f => out.println(f.toPath.normalize().toAbsolutePath.toFile.getAbsolutePath))
        out.println(info(center("[ end of lint - " + stopwatch.elapsed().toString + " ]")))
        System.exit(0)
      }

    } catch {
      case e: Exception => {
        e.printStackTrace()
        System.exit(42)
      }
    }
    System.exit(1)
  }

}
