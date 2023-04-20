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

  type UniqCode = String
  var codes = Set.empty[UniqCode]

  def uniqCode(i: Int): UniqCode = {
    val result = s"RL$i-WIP"
    if (codes.contains(result)) {
      throw new IllegalStateException(s"code ${result} already defined")
    } else {
      codes = codes + result
      result
    }

  }

  val fiFine = "✅"

  val fiCodeNexusUrlSlash = uniqCode(1001)
  val fiCodeNexusCentral = uniqCode(1002)
  val fiCodeGitLocalChanges = uniqCode(1003)
  val fiCodeGitNoRemotes = uniqCode(1004)
  val fiCodeGitlabCiFilename = uniqCode(1005)
  val fiCodeGitlabCiTagname = uniqCode(1006)
  val fiWarn = "\uD83D\uDE2C"
  val fiError = "❌"

  def run(out: PrintStream, err: PrintStream, opts: Starter.Opts,
          repo: Repo, envs: Map[String, String],
          file: File = new File(".").getAbsoluteFile): Int = {
    out.println()

    // TODO handle --simple-chars

    val color = opts.colors

    out.println(info(center("[ lint ]"), color))
    val stopwatch = Stopwatch.createStarted()
    try {
      // https://github.com/hadolint/hadolint
      // https://polaris.docs.fairwinds.com/infrastructure-as-code/

      val warnExitCode = 42
      val failExitCode = 2
      val lineMax = 100_000
      // TODO print $HOME
      println(info("    " + file.getAbsolutePath, color, lineMax))
      val warnExit = new AtomicBoolean(false)
      val failExit = new AtomicBoolean(false)
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
          Term.wrap(out, Term.warn,
            s""" shallow clone detected ${fiWarn}
               |% git rev-parse --is-shallow-repository # returns ${sgit.isShallowClone}
               |% git log -n1 --pretty=%H # returns
               |  ${sgit.commitIdHeadOpt().getOrElse("n/a")}
               |We do not want shallow clones because the commit id used in runtime
               |info will not point to a known commit
               |on Gitlab, change 'Settings' -> 'CI/CD' -> 'General pipelines' ->
               |  'Git shallow clone' to 0 or blank.
               |  If this does not fix this warning, toggle
               |  the .. -> 'Git strategy' to 'git clone' for maybe a
               |  single build to wipe out gitlab caches.
               |""".stripMargin, color)
          warnExit.set(true)
        } else {
          out.println(info(s"    ${fiFine} NO shallow clone", color))
        }

        if (false && envs.get("CI_CONFIG_PATH").orNull != null) {
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
          out.println(warn(s" Found local changes ${fiWarn} ${fiCodeGitLocalChanges}", color))
          warnExit.set(true)
        }
        out.println(info("--- list-remotes @ git ---", color))
        val remotes = sgit.listRemotes()
        if (remotes.isEmpty) {
          out.println(warn(s" NO remotes found ${fiWarn} ${fiCodeGitNoRemotes}", color))
          out.println(warn(" % git remote -v # returns nothing", color))
          warnExit.set(true)
        } else {
          remotes.foreach(r => out.println(info("      remote: " + r, useColor = color, limit = lineMax)))
        }

        val ciconfigpath = envs.get("CI_CONFIG_PATH").orNull
        val ciCommitRefName = envs.get("CI_COMMIT_REF_NAME").orNull
        val ciCommitTag = envs.get("CI_COMMIT_TAG").orNull
        val rootFolderFiles = files.toSeq
        val pompom = if (rootFolderFiles.exists(_.getName == "pom.xml")) {
          Some(PomMod.withRepoTry(file, opts, repo))
        } else {
          None
        }
        val tag = SuggestDockerTag.findTagname(ciCommitRefName, ciCommitTag, pompom.flatMap(_.toOption.map(_.selfVersion)))
        val defaultCiFilename = ".gitlab-ci.yml"

        if (ciconfigpath != null) {
          out.println(info("--- gitlabci.yml @ gitlab ---", color))
          if (ciconfigpath != defaultCiFilename) {
            out.println(warn("   ci path: " + ciconfigpath, color))
            out.println(warn(s"   use ${defaultCiFilename} ${fiWarn} ${fiCodeGitlabCiFilename}", color))
            warnExit.set(true)
          } else {
            out.println(info("      ci path: " + ciconfigpath, color))
          }

          if (tag.isSuccess) {
            if (tag.get.isSuccess) {
              out.println(info("      CI_COMMIT_TAG : " + tag.get.get, color))
            } else {
              Term.wrap(out, Term.warn, "   CI_COMMIT_TAG : " + tag.get.failed.get.getMessage + s" ${fiWarn} ${fiCodeGitlabCiTagname}", color)
              warnExit.set(true)
            }
          }

        }
        out.println(info("--- -SNAPSHOTS in files @ maven ---", color))

        val snapshotsInFiles = PomChecker.getSnapshotsInFiles(sgit.lsFilesAbsolute().map(_.getAbsolutePath))
        if (snapshotsInFiles.nonEmpty) {
          snapshotsInFiles.foreach(f => {
            out.println(warnSoft("  found snapshot in: " + file.toPath.relativize(f._3.normalize()) + s" ${fiWarn}\n" +
              "              " + f._2, color, limit = lineMax))
          })
        } else {
          out.println(info(s"    ${fiFine} NO SNAPSHOTS in other files found", color))
        }

        if (pompom.isDefined) {
          val pomModTry = pompom.get
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
            pomMod.listGavsForCheck()
              .filter(dep => ProjectMod.isUnwanted(dep.gav().simpleGav()))
              .filter(_.version.endsWith("-SNAPSHOT"))
              .foreach(dep => {
                out.println(warnSoft("  found snapshot: " + dep.gav().formatted + s" ${fiWarn}", color, limit = lineMax))
              })
            out.println(info("--- check for preview releases @ maven ---", color))
            pomMod.listGavsForCheck()
              .filter(dep => ProjectMod.isUnwanted(dep.gav().simpleGav()))
              .filterNot(_.version.endsWith("-SNAPSHOT"))
              .foreach(dep => {
                out.println(warnSoft("  found preview: " + dep.gav().formatted + s" ${fiWarn}", color, limit = lineMax))
              })
            out.println(info("    WIP", color))
            out.println(info("--- suggest dependency updates / configurable @ maven ---", color))

            val releasenexusworkurl: String = envs.get("RELEASE_NEXUS_WORK_URL").orNull
            if (repo.workNexusUrl() == Repo.centralUrl) {
              out.println(warn(s" work nexus points to central ${repo.workNexusUrl()} ${fiWarn} ${fiCodeNexusCentral}", color, limit = lineMax))
              out.println(info(s"    RELEASE_NEXUS_WORK_URL=${releasenexusworkurl}", color, limit = lineMax))
              warnExit.set(true)
            } else {
              out.println(info(s"    RELEASE_NEXUS_WORK_URL=${repo.workNexusUrl()}", color, limit = lineMax))
            }
            if (!repo.workNexusUrl().endsWith("/")) {
              out.println(warn(s" nexus work url must end with a '/' - ${repo.workNexusUrl()} ${fiWarn} ${fiCodeNexusUrlSlash}", color, limit = lineMax))
              warnExit.set(true)
            }
            try {

              pomMod.showDependencyUpdates(120, Term.select("dumb", "lint", opts.simpleChars, isInteractice = false), opts.depUpOpts,
                new Sys(null, out, err), printProgress = false) // TODO toggle
            } catch {
              case pce: PreconditionsException => {
                out.println(warn(pce.getMessage + s"${fiWarn}", color, limit = lineMax))
                warnExit.set(true)
              }
              case pce: Exception => {
                out.println(error(pce.getMessage + s"${fiWarn}", color, limit = lineMax))
                failExit.set(true)
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
        if (failExit.get()) {
          out.println(error(s"exit ${failExitCode} - because lint found errors, see above ${fiError}", color))
          return failExitCode
        } else if (warnExit.get()) {
          out.println(warn(s"exit ${warnExitCode} - because lint found warnings, see above ${fiError}", color))
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
