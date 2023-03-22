package release

import com.google.common.base.Stopwatch
import com.google.common.io.{CharSink, CharSource}
import com.google.googlejavaformat.java.Formatter
import release.Starter.{Opts, PreconditionsException, init}
import release.Term._

import java.io.{File, IOException, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Failure, Try, Success}
import scala.collection.parallel.CollectionConverters._

object Lint {

  def run(out: PrintStream, err: PrintStream, opts: Starter.Opts,
          repo: Repo, file: File = new File(".").getAbsoluteFile): Int = {
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
      val lineMax = 100_000
      // TODO print $HOME
      println(info("    " + file.getAbsolutePath, color, lineMax))
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
          out.println(warn("   % git log -n1 --pretty=%H # returns " + sgit.commitIdHeadOpt().getOrElse("n/a"), color, limit = lineMax))
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

        if (false && System.getenv("CI_CONFIG_PATH") != null) {
          try {
            val allFiles = sgit.lsFilesAbsolute().par
              .take(1)
              .filter(_.getName.toLowerCase().endsWith(".java"))
            val formatter = new Formatter()
            val result = allFiles.map(bFile => {
              doGoogleFmt(formatter, bFile)
            }).filter(_._1.isFailure)

            result.foreach(f => {
              out.println(warn(f._2.toString + " " + f._1.failed.get.getMessage, color, limit = lineMax))
            })
          } catch {
            case e: Throwable => out.println(warn(e.getMessage, color, limit = lineMax))
          }
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
          remotes.foreach(r => out.println(info("      remote: " + r, useColor = color, limit = lineMax)))
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

        out.println(info("--- -SNAPSHOTS in files @ maven ---", color))
        val rootFolderFiles = files.toSeq
        val snapshotsInFiles = PomChecker.getSnapshotsInFiles(sgit.lsFilesAbsolute().map(_.getAbsolutePath))
        if (snapshotsInFiles.nonEmpty) {
          snapshotsInFiles.foreach(f => {
            out.println(warn("  found snapshot in: " + file.toPath.relativize(f._3.normalize()) + s" ${fiWarn}\n" +
              "              " + f._2, color, limit = lineMax))
          })
        } else {
          out.println(info(s"    ${fiFine} NO SNAPSHOTS in other files found", color))
        }
        if (rootFolderFiles.exists(_.getName == "pom.xml")) {
          val pomModTry = PomMod.withRepoTry(file, opts, repo)
          if (pomModTry.isSuccess) {
            out.println(info("    WIP", color))
          } else {
            warnExit.set(true)
            out.println(warn(s"    ${fiWarn} ${pomModTry.failed.get.getMessage}", color, limit = lineMax))
          }
          if (pomModTry.isSuccess) {
            val pomMod = pomModTry.get
            out.println(info("--- .mvn @ maven ---", color))
            out.println(info("    WIP", color))
            out.println(info("--- check for snapshots @ maven ---", color))
            pomMod.listSnapshots.foreach(dep => {
              out.println(warn("  found snapshot: " + dep.gav().formatted + s" ${fiWarn}", color, limit = lineMax))
            })
            out.println(info("--- check for preview releases @ maven ---", color))
            pomMod.listDependeciesForCheck()
              .filter(dep => ProjectMod.isUnwanted(dep.gav().simpleGav()))
              .foreach(dep => {
                out.println(warn("  found preview: " + dep.gav().formatted + s" ${fiWarn}", color, limit = lineMax))
              })
            out.println(info("    WIP", color))
            out.println(info("--- suggest dependency updates / configurable @ maven ---", color))

            val releasenexusworkurl = System.getenv("RELEASE_NEXUS_WORK_URL")
            if (repo.workNexusUrl() == Repo.centralUrl) {
              out.println(warn(s" work nexus points to central ${repo.workNexusUrl()} ${fiWarn}", color, limit = lineMax))
              out.println(info(s"    RELEASE_NEXUS_WORK_URL=${releasenexusworkurl}", color, limit = lineMax))
              warnExit.set(true)
            } else {
              out.println(info(s"    RELEASE_NEXUS_WORK_URL=${repo.workNexusUrl()}", color, limit = lineMax))
            }
            try {

              pomMod.showDependencyUpdates(120, Term.select("dumb", "lint", opts.simpleChars), opts.depUpOpts,
                new Sys(null, out, err), printProgress = false) // TODO toggle
            } catch {
              case pce: PreconditionsException => {
                out.println(warn(pce.getMessage + s"${fiWarn}", color, limit = lineMax))
                warnExit.set(true)
              }
            }

            out.println(info("    WIP", color))
          } else {
            out.println(warn(s"    skipped because of previous problems ${fiWarn}", color))
          }
          out.println(info("--- dep.tree @ maven ---", color))
          out.println(info("    WIP", color))
        }
        if (rootFolderFiles.exists(_.getName == "build.sbt")) {
          out.println(info("--- ??? @ sbt ---", color))
          out.println(info("    WIP", color))
        }

        out.println()
        rootFolderFiles.sortBy(_.toString)
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

  def doGoogleFmt(formatter: Formatter, src: File): (Try[Unit], File) = {
    doGoogleFmt(formatter, src, src)
  }

  def doGoogleFmt(formatter: Formatter, src: File, target: File): (Try[Unit], File) = {
    try {
      val bSrc: CharSource = com.google.common.io.Files.asCharSource(src, StandardCharsets.UTF_8)
      val bSink: CharSink = com.google.common.io.Files.asCharSink(target, StandardCharsets.UTF_8)
      formatter.formatSource(bSrc, bSink)
      (Success(()), src.getAbsoluteFile)
    } catch {
      case e: Throwable => (Failure(e), src.getAbsoluteFile)
    }
  }
}
