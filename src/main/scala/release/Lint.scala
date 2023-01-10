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
    val fiFine = "✅"
    val fiWarn = "\uD83D\uDE2C"
    val fiError = "❌"
    val color = opts.colors

    out.println(info(center("[ lint ]"), color))
    val stopwatch = Stopwatch.createStarted()
    try {
      // https://github.com/hadolint/hadolint
      // https://polaris.docs.fairwinds.com/infrastructure-as-code/

      val warnExitCode = 42
      val lineLimit = 10_000
      // TODO print $HOME
      println(info("    " + file.getAbsolutePath, color, lineLimit))
      val warnExit = new AtomicBoolean(false)
      val files = file.listFiles()
      if (files == null || files.isEmpty) {
        out.println(error(s"E: NO FILES FOUND in ${file.getAbsolutePath}", color))
        out.println(error(center("[ end of lint ]"), color))
        return 1
      } else {
        val sgit = Sgit(file, doVerify = false, out = out, err = err, checkExisting = true, gitBin = None, opts = Opts())
        out.println(info(s"    ${fiFine} git version: " + sgit.version(), color))
        out.println(info("--- check clone config / no shallow clone @ git ---", color))
        if (sgit.isShallowClone) {
          out.println(warn(s" shallow clone detected ${fiWarn}", color))
          out.println(warn("   % git rev-parse --is-shallow-repository # returns " + sgit.isShallowClone, color))
          out.println(warn("   % git log -n1 --pretty=%H # returns " + sgit.commitIdHeadOpt().getOrElse("n/a"), color, limit = lineLimit))
          out.println(warn("   We do not want shallow clones because the commit id used in runtime", color))
          out.println(warn("   info will not point to a known commit", color))
          out.println(warn("   on Gitlab, change 'Settings' -> 'CI/CD' -> 'General pipelines' ->", color))
          out.println(warn("     'Git shallow clone' to 0 or blank.", color))
          out.println(warn("     If this does not fix this warning, toggle", color))
          out.println(warn("     the .. -> 'Git strategy' to 'git clone' for maybe a", color))
          out.println(warn("     single build to wipe out gitlab caches.", color))
          warnExit.set(true)
        } else {
          out.println(info(s"    ${fiFine} NO shallow clone", color))
        }
        out.println(info("--- .gitattributes @ git ---", color))
        out.println(info("--- .gitignore @ git ---", color))
        if (sgit.hasLocalChanges) {
          out.println(warn(s" Found local changes ${fiWarn}", color))
          warnExit.set(true)
        }
        out.println(info("--- list-remotes @ git ---", color))
        val remotes = sgit.listRemotes()
        if (remotes.isEmpty) {
          out.println(warn(s" NO remotes found ${fiWarn}", color))
          out.println(warn(" % git remote -v # returns nothing", color))
          warnExit.set(true)
        } else {
          remotes.foreach(r => out.println(info("      remote: " + r, useColor = color, limit = lineLimit)))
        }

        val ciconfigpath = System.getenv("CI_CONFIG_PATH")
        val defaultCiFilename = ".gitlab-ci.yml"
        if (ciconfigpath != null) {
          out.println(info("--- gitlabci.yml @ gitlab ---", color))
          if (ciconfigpath != defaultCiFilename) {
            out.println(warn("    ci path: " + ciconfigpath, color))
            out.println(warn(s"   use ${defaultCiFilename} ${fiWarn}", color))
            warnExit.set(true)
          } else {
            out.println(info("    ci path: " + ciconfigpath, color))
          }
        }
        PomChecker.printSnapshotsInFiles(sgit.lsFiles(), out)
        if (files.toSeq.exists(_.getName == "pom.xml")) {
          val pomMod = PomMod.withRepo(file, opts, new Repo(opts))
          out.println(info("--- .mvn @ maven ---", color))
          out.println(info("    WIP", color))
          out.println(info("--- check for snapshots @ maven ---", color))
          out.println(info("    WIP", color))
          out.println(info("--- suggest dependency updates / configurable @ maven ---", color))

          pomMod.showDependencyUpdates(120, Term.select("dumb", "lint", opts.simpleChars), opts.depUpOpts,
            new Sys(null, out, err), printProgress = false) // TODO toggle
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
          out.println(error(s"exit 42 - because lint found warnings, see above ${fiError}", color))
          return warnExitCode
        } else {
          return 0
        }

      }

    } catch {
      case e: Exception => {
        Starter.handleException(err, e)
      }
    }
    1
  }

}
