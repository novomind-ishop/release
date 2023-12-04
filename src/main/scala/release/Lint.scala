package release

import com.google.common.base.{Stopwatch, Strings}
import com.google.common.hash.Hashing
import com.google.common.io.{CharSink, CharSource}
import com.google.googlejavaformat.java.Formatter
import release.ProjectMod.{Gav, Gav3, StaticPrinter, UpdatePrinter, Version}
import release.Starter.{Opts, PreconditionsException, init}
import release.Term._

import java.io.{File, IOException, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.{Failure, Success, Try}
import scala.collection.parallel.CollectionConverters._

object Lint {
  def versionMissmatches(selfVersion: String, tagBranchInfo: Option[BranchTagMerge]): Boolean = {
    val defaultBranchnames = Set("main", "master")
    if (tagBranchInfo.isDefined) {
      if (tagBranchInfo.get.branchName.isDefined) {
        val selfVersionParsed = Version.parseSloppy(selfVersion)
        val branchName = tagBranchInfo.get.branchName.get
        if (defaultBranchnames.contains(branchName)) {
          if (selfVersionParsed.isOrdinal) {
            false
          } else {
            val str = branchName + "-SNAPSHOT"
            selfVersionParsed.rawInput != str
          }
        } else {
          if (selfVersionParsed.isSnapshot) {
            if (selfVersionParsed.isOrdinal) {
              val digitsOnly = branchName
                .replaceAll("[^0-9]+", "|").split("[|]+")
                .toSeq

              val value = digitsOnly.flatMap(_.toIntOption)
              val bool = selfVersionParsed.same(value)
              !bool
            } else {
              false
            }
          } else {
            true
          }
        }
      } else if (tagBranchInfo.get.tagName.isDefined) {
        tagBranchInfo.get.tagName.get.replaceFirst("^v", "") != selfVersion
      } else if (tagBranchInfo.get.isMergeRequest) {
        false
      } else {
        true
      }
    } else {
      true
    }
  }

  private val MERGE_REQUEST = "MERGE_REQUEST"

  case class BranchTagMerge(tagName: Option[String], branchName: Option[String], info: String = "") {
    val isMergeRequest = info == MERGE_REQUEST
  }

  object BranchTagMerge {
    val merge = Some(BranchTagMerge(tagName = None, branchName = None, info = MERGE_REQUEST))
  }

  def toBranchTag(ciCommitRefName: String, ciCommitTag: String, sgit: Sgit, ciCommitBranch: String): Option[BranchTagMerge] = {
    if (Strings.emptyToNull(ciCommitTag) == null && Strings.emptyToNull(ciCommitBranch) == null &&
      Strings.emptyToNull(ciCommitRefName) != null) {
      return BranchTagMerge.merge
    }
    val currentBranchOpt = sgit.currentBranchOpt
    val currentTags = sgit.currentTags.getOrElse(Nil)
    if (ciCommitRefName == ciCommitTag && currentTags.contains(ciCommitTag) && Strings.emptyToNull(ciCommitBranch) == null) {
      Some(BranchTagMerge(tagName = Some(ciCommitTag), branchName = None))
    } else if (Strings.emptyToNull(ciCommitTag) == null &&
      (currentBranchOpt.getOrElse("") == ciCommitRefName || ciCommitRefName == ciCommitBranch)) {
      Some(BranchTagMerge(tagName = None, branchName = Some(ciCommitRefName)))
    } else if (currentBranchOpt.isDefined) {
      Some(BranchTagMerge(tagName = None, branchName = Some(currentBranchOpt.get)))
    } else if (currentTags.nonEmpty) {
      Some(BranchTagMerge(tagName = Some(currentTags.head), branchName = None)) // XXX multiple tags
    } else {
      None
    }
  }

  def isValidMergeRequest(maybeMerge: Option[BranchTagMerge]): Boolean = {
    maybeMerge.isDefined && maybeMerge.get.isMergeRequest
  }

  def isValidBranch(maybeBranch: Option[BranchTagMerge]): Boolean = {
    maybeBranch.isDefined && maybeBranch.get.branchName.isDefined
  }

  def isValidTag(maybeTag: Option[BranchTagMerge]): Boolean = {
    maybeTag.isDefined && maybeTag.get.tagName.isDefined
  }

  type UniqCode = String
  var codes = Set.empty[UniqCode]

  private class CodeGen(code: String) {
    def apply(dyn: Any = null): String = {
      val suffix = if (dyn != ()) {
        "-" + Hashing.murmur3_32_fixed().hashString(dyn.toString, StandardCharsets.UTF_8)
      } else {
        ""
      }
      code + suffix
    }
  }

  def uniqCode(i: Int): PartialFunction[Any, UniqCode] = {
    val result = s"RL$i"
    if (codes.contains(result)) {
      throw new IllegalStateException(s"code ${result} already defined")
    } else {
      codes = codes + result
      x => new CodeGen(result).apply(x)
    }

  }

  val fiFine = "✅"

  val fiCodeNexusUrlSlash = uniqCode(1001)(())
  val fiCodeNexusCentral = uniqCode(1002)(())
  val fiCodeGitLocalChanges = uniqCode(1003)
  val fiCodeGitNoRemotes = uniqCode(1004)(())
  val fiCodeGitlabCiFilename = uniqCode(1005)(())
  val fiCodeGitlabCiTagname = uniqCode(1006)(())
  val fiCodePomModPreconditionsException = uniqCode(1007)(())
  val fiCodePomModException = uniqCode(1008)(())
  val fiCodeNexusFoundRelease = uniqCode(1009)
  val fiCodeUnusualGav = uniqCode(1010)
  val fiCodeSnapshotGav = uniqCode(1011)
  val fiCodeSnapshotText = uniqCode(1012)
  val fiCodeCoreDiff = uniqCode(1013)
  val fiCodeVersionMissmatch = uniqCode(1014)(())
  val fiWarn = "\uD83D\uDE2C"
  val fiError = "❌"

  def run(out: PrintStream, err: PrintStream, opts: Starter.Opts,
          repo: Repo, envs: Map[String, String],
          file: File = new File(".").getAbsoluteFile): Int = {
    out.println()

    // TODO handle --simple-chars


    out.println(info(center("[ lint ]"), opts))
    val stopwatch = Stopwatch.createStarted()
    try {
      // https://github.com/hadolint/hadolint
      // https://polaris.docs.fairwinds.com/infrastructure-as-code/

      val warnExitCode = 42
      val errorExitCode = 2
      val lineMax = 100_000
      // TODO print $HOME
      println(info("    " + file.getAbsolutePath, opts, lineMax))
      val warnExit = new AtomicBoolean(false)
      val errorExit = new AtomicBoolean(false)
      val files = file.listFiles()
      var usedSkips = Seq.empty[String]
      if (files == null || files.isEmpty) {
        out.println(error(s"E: NO FILES FOUND in ${file.getAbsolutePath}", opts))
        out.println(error(center("[ end of lint ]"), opts))
        return 1
      } else {
        if (opts.lintOpts.skips.nonEmpty) {
          out.println(info("--- skip-conf / self ---", opts))
          out.println(info(s"    skips: " + opts.lintOpts.skips.mkString(", "), opts, limit = lineMax))
        }
        val sgit = Sgit(file, doVerify = false, out = out, err = err, checkExisting = true, gitBin = None, opts = Opts())
        out.println(info("--- version / git ---", opts))
        out.println(info(s"    ${fiFine} git version: " + sgit.version(), opts))
        out.println(info("--- check clone config / remote @ git ---", opts))
        val remoteHeadDefinition = sgit.remoteHead()
        if (remoteHeadDefinition.isSuccess && remoteHeadDefinition.get.isDefined) {
          if (remoteHeadDefinition.get.exists(_.contains("(unknown)"))) {
            out.println(warn(s" ${fiWarn} unknown remote HEAD found, corrupted remote -- repair please", opts))
            out.println(warn(s" ${fiWarn} if you use gitlab try to", opts))
            out.println(warn(s" ${fiWarn} choose another default branch; save; use the original default branch", opts))
            warnExit.set(true)
          }
          val commitRef = sgit.logShortOpt(limit = 1, path = Sgit.toRawRemoteHead(remoteHeadDefinition).get.get)
          val commitOf = commitRef.map(_.replaceFirst("^commit ", ""))
          if (commitOf.isDefined) {
            out.println(info(s"    ${remoteHeadDefinition.get.get} - ${commitOf.get}", opts, limit = lineMax))
          } else {
            out.println(warn(s" ${fiWarn} ${remoteHeadDefinition.get.get} - n/a", opts, limit = lineMax))
            warnExit.set(true)
          }
        } else {
          out.println(warn(s" ${fiWarn} no remote HEAD found, corrupted remote -- repair please", opts))
          out.println(warn(s" ${fiWarn} if you use gitlab try to", opts))
          out.println(warn(s" ${fiWarn} choose another default branch; save; use the original default branch", opts))
          if (remoteHeadDefinition.isFailure) {
            val exception = remoteHeadDefinition.failed.get
            out.println(warn(s" ${fiWarn} remote call exception: ${exception.getClass.getName} message: ${exception.getMessage}",
              opts, limit = lineMax))
          }
          warnExit.set(true)
        }
        out.println(info("--- check clone config / no shallow clone @ git ---", opts))
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
               |""".stripMargin, opts)
          warnExit.set(true)
        } else {
          out.println(info(s"    ${fiFine} NO shallow clone", opts))
        }
        val currentGitTags = sgit.currentTags
        if (currentGitTags.isDefined) {
          out.println(info("    current git tags: " + currentGitTags.map(_.mkString(", ")).getOrElse(""), opts, limit = lineMax))
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
              out.println(warn(f._2.toString + " " + f._1.failed.get.getMessage, opts, limit = lineMax))
            })
          } catch {
            case e: Throwable => out.println(warn(e.getMessage, opts, limit = lineMax))
          }
        }

        out.println(info("--- .gitattributes @ git ---", opts))
        out.println(info("--- .gitignore @ git ---", opts))
        if (sgit.hasLocalChanges) {
          var folderMods = Seq.empty[String]
          val names = sgit.localChanges()
            .flatMap(line => {
              val folder = line.replaceAll("/[^/]+$", "/")
              Seq(line, folder)
            })
            .sorted
            .distinct
            .filterNot(name => {

              val code = fiCodeGitLocalChanges(name)
              val bool = opts.lintOpts.skips.contains(code)
              if (bool) {
                usedSkips = usedSkips :+ code
              }
              if (name.endsWith("/") && bool) {
                folderMods = folderMods :+ name
              }
              bool
            })
          val namesParents = names.filterNot(name => {
            folderMods.exists(prefix => name.startsWith(prefix))
          })

          val mainSkip = fiCodeGitLocalChanges(namesParents)
          if (!opts.lintOpts.skips.contains(mainSkip) && namesParents.nonEmpty) {
            out.println(warn(s" Found local changes ${fiWarn} $mainSkip", opts))
            namesParents
              .foreach(filename => out.println(warn(s" ${filename} ${fiWarn} ${fiCodeGitLocalChanges(filename)}", opts, limit = lineMax)))
            warnExit.set(true)
          } else {
            usedSkips = usedSkips :+ mainSkip
          }
        }
        out.println(info("--- list-remotes @ git ---", opts))
        val remotes = sgit.listRemotes()
        if (remotes.isEmpty) {
          out.println(warn(s" NO remotes found ${fiWarn} ${fiCodeGitNoRemotes}", opts))
          out.println(warn(" % git remote -v # returns nothing", opts))
          warnExit.set(true)
        } else {
          remotes.foreach(r => out.println(info("      remote: " + r, opts, limit = lineMax)))
        }

        val ciconfigpath = envs.get("CI_CONFIG_PATH").orNull
        val ciCommitRefName = envs.get("CI_COMMIT_REF_NAME").orNull
        val ciTagEnv = envs.get("CI_COMMIT_TAG")
        val ciCommitTag = ciTagEnv.orNull
        val ciCommitBranchEnv = envs.get("CI_COMMIT_BRANCH") // not present on gitlab merge requests
        val ciCommitBranch = ciCommitBranchEnv.getOrElse("")
        val tagBranchInfo = Lint.toBranchTag(ciCommitRefName, ciCommitTag, sgit, ciCommitBranch)
        val isGitOrCiTag: Boolean = Lint.isValidTag(tagBranchInfo)
        val rootFolderFiles = files.toSeq
        var pomFailures: Seq[Exception] = Nil
        val pompom: Option[Try[ProjectMod]] = if (rootFolderFiles.exists(_.getName == "pom.xml")) {
          Some(PomMod.withRepoTry(file, opts, repo, failureCollector = Some(e => {
            pomFailures = pomFailures :+ e
          })))
        } else {
          None
        }
        val sbt: Option[Try[ProjectMod]] = if (rootFolderFiles.exists(_.getName == "build.sbt")) {
          Some(Success(SbtMod.withRepo(file, opts, repo)))
        } else {
          None
        }
        val dockerTag = SuggestDockerTag.findTagname(ciCommitRefName, ciCommitTag, pompom.flatMap(_.toOption.map(_.selfVersion)))
        val defaultCiFilename = ".gitlab-ci.yml"

        if (ciconfigpath != null) {
          out.println(info("--- gitlabci.yml @ gitlab ---", opts))
          if (ciconfigpath != defaultCiFilename) {
            out.println(warn("   ci path: " + ciconfigpath, opts))
            out.println(warn(s"   use ${defaultCiFilename} ${fiWarn} ${fiCodeGitlabCiFilename}", opts))
            warnExit.set(true)
          } else {
            out.println(info("      ci path: " + ciconfigpath, opts))
          }

          out.println(info("      CI_COMMIT_TAG : " + ciCommitTag, opts))
          out.println(info("      CI_COMMIT_REF_NAME : " + ciCommitRefName, opts))
          out.println(info("      CI_COMMIT_BRANCH : " + ciCommitBranch, opts))

          if (Lint.isValidTag(tagBranchInfo)) {
            out.println(info("      a valid tag : " + ciCommitRefName, opts))
          } else if (Lint.isValidBranch(tagBranchInfo)) {
            out.println(info("      a valid branch : " + ciCommitRefName, opts))
          } else if (Lint.isValidMergeRequest(tagBranchInfo)) {
            out.println(info("      a valid merge request : " + ciCommitRefName, opts))
          } else {
            out.println(warn(s"   an invalid branch/tag: " +
              s"ciRef: ${ciCommitRefName}, " +
              s"ciTag: ${ciCommitTag}, " +
              s"ciBranch: ${ciCommitBranch}, " +
              s"gitTags: ${sgit.currentTags.getOrElse(Nil).mkString(",")}, " +
              s"gitBranch: ${sgit.currentBranchOpt.getOrElse("")}", opts, limit = lineMax))
            warnExit.set(true)
          }
          if (dockerTag.isSuccess) {
            if (dockerTag.get.isSuccess) {
              out.println(info("      docker tag : " + dockerTag.get.get, opts))
            } else {
              val bool = opts.lintOpts.skips.contains(fiCodeGitlabCiTagname)
              if (bool) {
                usedSkips = usedSkips :+ fiCodeGitlabCiTagname
              }
              if (bool) {
                Term.wrap(out, Term.warn, "  docker tag : " + dockerTag.get.failed.get.getMessage + s" ${fiWarn}\u00A0${fiCodeGitlabCiTagname}", opts)
                warnExit.set(true)
              } else {
                Term.wrap(out, Term.error, "     docker tag : " + dockerTag.get.failed.get.getMessage + s" ${fiError}\u00A0${fiCodeGitlabCiTagname}", opts)
                errorExit.set(true)
              }

            }
          }

        }
        out.println(info("--- -SNAPSHOTS in files @ maven/sbt/gradle ---", opts))

        val snapshotsInFiles = PomChecker.getSnapshotsInFiles(sgit.lsFilesAbsolute().map(_.getAbsolutePath))
        if (snapshotsInFiles.nonEmpty) {
          val relFiles: Seq[(Int, String, Path)] = snapshotsInFiles
            .map(t => (t._1, t._2, file.toPath.relativize(t._3.normalize())))
          val code1 = fiCodeSnapshotText(relFiles)
          if (!opts.lintOpts.skips.contains(code1)) {
            relFiles
              .filterNot(f => {
                val code = fiCodeSnapshotText((f._1, f._2, f._3.getFileName))
                val bool = opts.lintOpts.skips.contains(code)
                if (bool) {
                  usedSkips = usedSkips :+ code
                }
                bool
              })
              .foreach(f => {
                val snapMsg = "  found snapshot in: " + f._3 +
                  s" ${fiWarn} ${fiCodeSnapshotText((f._1, f._2, f._3.getFileName))}\n" +
                  "              " + f._2
                if (isGitOrCiTag) {
                  out.println(warn(snapMsg, opts, limit = lineMax))
                  warnExit.set(true)
                } else {
                  out.println(warnSoft(snapMsg, opts, limit = lineMax))
                }

              })
            val snapshotSumMsg = s"  found snapshots: ${fiWarn} $code1"
            if (isGitOrCiTag) {
              out.println(warn(snapshotSumMsg, opts, limit = lineMax))
              warnExit.set(true)
            } else {
              out.println(warnSoft(snapshotSumMsg, opts, limit = lineMax))
            }
          } else {
            usedSkips = usedSkips :+ code1
          }
        } else {
          out.println(info(s"    ${fiFine} NO SNAPSHOTS in other files found", opts))
        }

        if (pompom.isDefined || sbt.isDefined) {
          val modTry = if (pompom.isDefined) {
            pompom.get
          } else if (sbt.isDefined) {
            sbt.get
          } else {
            throw new IllegalStateException("should not happen")
          }
          if (modTry.isSuccess) {
            out.println(info("    WIP", opts))
          } else {
            warnExit.set(true)
            out.println(warn(s"    ${fiWarn} ${modTry.failed.get.getMessage}", opts, limit = lineMax))
          }
          if (modTry.isSuccess) {
            val mod = modTry.get

            out.println(info("--- .mvn @ maven ---", opts))
            out.println(info("    WIP", opts)) // TODO check extentions present
            out.println(info("--- project version @ maven ---", opts))
            out.println(info(s"    ${modTry.get.selfVersion}", opts))
            if (Lint.versionMissmatches(modTry.get.selfVersion, tagBranchInfo)) {
              val msg = s" ${modTry.get.selfVersion} != ${Util.show(tagBranchInfo)} ${fiWarn} ${fiCodeVersionMissmatch}"
              val bool = opts.lintOpts.skips.contains(fiCodeVersionMissmatch)
              if (bool) {
                usedSkips = usedSkips :+ fiCodeVersionMissmatch
                out.println(warnSoft(msg, opts, limit = lineMax))
              } else {
                out.println(warn(msg, opts, limit = lineMax))
                warnExit.set(true)
              }
            }
            out.println(info("--- check for snapshots @ maven ---", opts))
            val snaps = mod.listGavsForCheck()
              .filter(dep => ProjectMod.isUnwanted(dep.gav().simpleGav()))
              .filter(_.version.get.endsWith("-SNAPSHOT"))
            snaps
              .foreach(dep => {
                val snapFound = "  found snapshot: " + dep.gav().formatted + s" ${fiWarn} ${fiCodeSnapshotGav.apply(dep.gav())}"
                if (isGitOrCiTag) {
                  out.println(warn(snapFound, opts, limit = lineMax))
                  warnExit.set(true)
                } else {
                  out.println(warnSoft(snapFound, opts, limit = lineMax))

                }
              })
            out.println(info("--- check for GAV format @ maven ---", opts))
            val unusualGavs = mod.listGavsWithUnusualScope()
            if (unusualGavs.nonEmpty) {
              unusualGavs.foreach(found => {
                out.println(warn(s"${found.formatted} uses unusual format, please repair ${fiWarn} ${fiCodeUnusualGav.apply(found)}", opts, limit = lineMax))
              })
              out.println(info(s"known scopes are: ${ProjectMod.knownScopes.filterNot(_.isEmpty).toSeq.sorted.mkString(", ")}", opts))
              out.println(info(s"version ranges are not allowed", opts))
              out.println(info(s"unstable marker like LATEST and RELEASE are not allowed", opts))
              warnExit.set(true)
            } else {
              out.println(info(s"    ${fiFine} all GAVs scopes looks fine", opts))
            }
            out.println(info("--- check for preview releases @ maven ---", opts))
            val updatePrinter = new StaticPrinter()
            val updateOpts = opts.depUpOpts.copy(hideStageVersions = true)
            val updateResultTry = mod.tryCollectDependencyUpdates(updateOpts, checkOn = true, updatePrinter)
            val lookup: Map[Gav3, Seq[String]] = if (updateResultTry.isSuccess) {
              updateResultTry.get.map(t => (t._1.gav.simpleGav(), t._2._1)).toMap
            } else {
              Map.empty
            }
            mod.listGavsForCheck()
              .filter(dep => ProjectMod.isUnwanted(dep.gav().simpleGav()))
              .filterNot(_.version.get.endsWith("-SNAPSHOT"))
              .foreach(dep => {
                out.println(warnSoft("  found preview: " + dep.gav().formatted + s" ${fiWarn}", opts, limit = lineMax))
                if (updateResultTry.isSuccess) {
                  // FIXME use update for next/previous later
                  val versionRangeFor = lookup.get(dep.gav().simpleGav())
                  out.println(warnSoft("       next     WIP: " + dep.gav().copy(version = Some("1.0.1")).formatted, opts, limit = lineMax))
                  out.println(warnSoft("       previous WIP: " + dep.gav().copy(version = Some("0.99.99")).formatted, opts, limit = lineMax))
                }
              })
            out.println(info("    WIP", opts))
            out.println(info("--- check major versions @ ishop ---", opts))
            val mrc = Release.coreMajorResultOf(mod, None)
            if (mrc.hasDifferentMajors) {
              warnExit.set(true)
              out.println(warn(s"    Found core ${mrc.sortedMajors.mkString(", ")} ${fiWarn} ${fiCodeCoreDiff.apply(mrc)}", opts, limit = lineMax))
              val versions = mrc.coreMajorVersions
              val mrcGrouped: Map[String, Seq[Gav]] = versions.groupBy(_._1)
                .map(e => (e._1, e._2.map(_._2.gav()).distinct))

              mrcGrouped.toSeq
                .sortBy(_._1.toIntOption)
                .foreach(gavE => {
                  out.println(warn(s"      - ${gavE._1} -", opts, limit = lineMax))
                  gavE._2.foreach(gav => {
                    out.println(warn(s"      ${gav.formatted} ${fiWarn} ${fiCodeCoreDiff.apply(gav)}", opts, limit = lineMax))
                  })
                })

            } else {
              out.println(info(s"    ${fiFine} no major version diff", opts))
            }
            out.println(info("--- suggest dependency updates / configurable @ maven ---", opts))

            val releasenexusworkurl: String = envs.get("RELEASE_NEXUS_WORK_URL").orNull
            if (repo.workNexusUrl() == Repo.centralUrl) {
              out.println(warn(s" work nexus points to central ${repo.workNexusUrl()} ${fiWarn} ${fiCodeNexusCentral}", opts, limit = lineMax))
              out.println(info(s"    RELEASE_NEXUS_WORK_URL=${releasenexusworkurl} # (${Util.ipFromUrl(releasenexusworkurl).getOrElse("no ip")})", opts, limit = lineMax))
              warnExit.set(true)
            } else {
              out.println(info(s"    RELEASE_NEXUS_WORK_URL=${repo.workNexusUrl()} # (${Util.ipFromUrl(repo.workNexusUrl()).getOrElse("no ip")})", opts, limit = lineMax))
            }
            if (!repo.workNexusUrl().endsWith("/")) {
              out.println(warn(s" nexus work url must end with a '/' - ${repo.workNexusUrl()} ${fiWarn} ${fiCodeNexusUrlSlash}", opts, limit = lineMax))
              warnExit.set(true)
            }
            val settingsNexusMirrors = mod.listRemoteRepoUrls()
            if (settingsNexusMirrors != Nil) {
              settingsNexusMirrors.foreach(url => {
                out.println(info(s"    settings.xml nexus mirror=${url} # (${Util.ipFromUrl(url).getOrElse("no ip")})", opts, limit = lineMax))
              })
            }

            try {
              out.println(updatePrinter.result.toString.trim)
              val updateResult = updateResultTry.get
              val snapUpdates = updateResult.filter(e => snaps.map(_.gav()).contains(e._1.gav))
              val releaseOfSnapshotPresent = snapUpdates.map(e => (e._1.gav, e._2._1.contains(e._1.gav.version.get.replaceFirst("-SNAPSHOT", ""))))
              if (releaseOfSnapshotPresent.nonEmpty) {
                val releasesFound = releaseOfSnapshotPresent.filter(_._2)
                if (releasesFound.nonEmpty) {
                  releasesFound.map(_._1).foreach(found => {
                    out.println(warn(s"${found.formatted} is already released, remove '-SNAPSHOT' suffix ${fiWarn} ${fiCodeNexusFoundRelease(found)}", opts, limit = lineMax))
                  })
                  warnExit.set(true)
                }
              }

            } catch {
              case pce: PreconditionsException => {
                out.println(warn(pce.getMessage + s"${fiWarn} ${fiCodePomModPreconditionsException}", opts, limit = lineMax))
                warnExit.set(true)
              }
              case pce: Exception => {
                out.println(error(pce.getMessage + s" ${fiWarn} ${fiCodePomModException}", opts, limit = lineMax))
                errorExit.set(true)
              }
            }

            out.println(info("    WIP", opts))
          } else {
            out.println(warn(s"    skipped because of previous problems - ${modTry.failed.get.getMessage} ${fiWarn}", opts, limit = lineMax))
            warnExit.set(true)
          }
          if (pomFailures.nonEmpty) {
            out.println(warn(s"   previous problems - ${pomFailures.mkString("\n")} ${fiWarn}", opts, limit = lineMax))
            warnExit.set(true)
          }
          out.println(info("--- dep.tree @ maven ---", opts))
          out.println(info("    WIP", opts))
        }
        if (sbt.isDefined) {
          out.println(info("--- ??? @ sbt ---", opts))
          out.println(info("    WIP", opts))
        }
        if (opts.lintOpts.skips.nonEmpty) {
          val unusedSkips = opts.lintOpts.skips.diff(usedSkips)
          if (unusedSkips.nonEmpty) {
            out.println(warn("--- skip-conf / self / end ---", opts))
            out.println(warn(s"    found unused skips, please remove from your config: " + unusedSkips.mkString(", "), opts, limit = lineMax))
            warnExit.set(true)
          }
        }

        out.println()
        rootFolderFiles.sortBy(_.toString)
          .take(5).foreach(f => out.println(f.toPath.normalize().toAbsolutePath.toFile.getAbsolutePath))
        val timerResult = if (opts.lintOpts.showTimer) {
          " - " + stopwatch.elapsed().toString // TODO timer score A - F
        } else {
          ""
        }

        out.println(info(center("[ end of lint" + timerResult + " ]"), opts))
        if (errorExit.get()) {
          out.println(error(s"exit ${errorExitCode} - because lint found errors, see above ${fiError}", opts))
          return errorExitCode
        } else if (warnExit.get()) {
          out.println(warn(s"exit ${warnExitCode} - because lint found warnings, see above ${fiWarn}", opts))
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
