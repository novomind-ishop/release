package release

import com.google.common.base.{Stopwatch, Strings}
import com.google.common.io.{CharSink, CharSource}
import com.google.googlejavaformat.java.Formatter
import release.ProjectMod.{Gav, Gav3, StaticPrinter}
import release.Sgit.GitShaRefTime
import release.Starter.{Opts, PreconditionsException}
import release.Term._
import release.Util.pluralize

import java.io.{File, IOException, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.time.temporal.ChronoUnit
import java.time.{Duration, Period, ZonedDateTime}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._
import scala.util.{Failure, Success, Try, Using}

object Lint {

  private def calcFreq(refsr: Seq[Sgit.GitShaRefTime]): Option[Period] = {
    if (refsr.size < 2) {
      None
    } else {
      val k = refsr.sortBy(_.date).sliding(2)
        .map(o => ChronoUnit.DAYS.between(o(0).date.toLocalDate, o(1).date.toLocalDate)).toSeq

      val sum = k.sum
      Some(Period.ofDays((sum.intValue() / k.size)))
    }

  }

  def refFreqBranchTag(refs: Seq[Sgit.GitShaRefTime], currentDate: ZonedDateTime = ZonedDateTime.now()): (Period, Period) = {
    val older = currentDate.minusYears(1)
    val fRefs = refs.filter(_.dateOpt.isDefined).filter(_.date.isAfter(older))
    val branches = fRefs.collect({ case e: GitShaRefTime if e.refName.startsWith("refs/heads/") => e }) // TODO filter branch pattern
    val tags = fRefs.collect({ case e: GitShaRefTime if e.refName.startsWith("refs/tags/") => e })
    val negativ = Period.ofDays(-1)
    (calcFreq(branches).getOrElse(negativ), calcFreq(tags).getOrElse(negativ))
  }

  object PackageResult {
    def timeout(d: Duration, msg: String) = {
      PackageResult(Seq("timeout"), d, "timeout", msg = msg)
    }
  }

  case class PackageResult(names: Seq[String], d: Duration, private val unwantedText: String, msg: String) {

    private def nom(in: String) = {
      in.replaceFirst("package ", "").trim
    }

    val allNames = unwantedText.linesIterator.toSet.map(nom).toSeq
    lazy val unwantedPackages: Seq[String] = {

      val normalized = names.map(nom)
      normalized.filter(e => allNames.exists(k => {
        if (k.endsWith(";")) {
          e.replace(";", "").endsWith(k.replace(";", ""))
        } else {
          e.startsWith(k)
        }

      }))
    }
    lazy val unwantedDefinitionSum: String = {
      Util.hashMurmur3_32_fixed(allNames.sorted.mkString("\n"))
    }
  }

  def findAllPackagenames(rootFile: File, check: Boolean): PackageResult = {
    val packageScanFile = new File(rootFile, ".unwanted-packages")
    if (check && packageScanFile.canRead) {

      def toResult(stopwatch: Stopwatch): PackageResult = {

        val na = ListBuffer.empty[Path]
        val skipDirNames = Set(".git", "target")
        Files.walkFileTree(rootFile.toPath, new FileVisitor[Path] {
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (skipDirNames.contains(dir.getFileName.toString)) {
              FileVisitResult.SKIP_SUBTREE
            } else {
              FileVisitResult.CONTINUE
            }
          }

          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            na += file
            FileVisitResult.CONTINUE

          }

          override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {

            FileVisitResult.CONTINUE
          }
        })
        val suffix = Set("java", "scala")
        PackageResult(na.par
          .filter(p => suffix.contains(com.google.common.io.Files.getFileExtension(p.toFile.getName)))
          .flatMap(f => {
            try {
              Using.resource(scala.io.Source.fromFile(f.toFile)) { r => selectPackage(r.getLines()) }
            } catch {
              case _: Exception => None
            }

          }).distinct.toList.sorted, stopwatch.elapsed().withNanos(0), unwantedText = Util.read(packageScanFile), msg = "")

      }

      Util.timeout(20, TimeUnit.SECONDS, toResult, (e, d) => {
        PackageResult.timeout(d, e.getMessage + ". Big projects with more then 10k files took ~2 seconds")
      })._1

    } else {
      PackageResult(Nil, Duration.ZERO, "", "")
    }

  }

  def selectPackage(value: Iterator[String]): Option[String] = {
    value.find(l => l.trim.startsWith("package"))
  }

  case class NePrLa(next: Option[Gav3], previous: Option[Gav3], latest: Option[Gav3])

  def selectNextAndPrevious(versionRangeFor: Option[Seq[String]], gav: Gav3): Option[NePrLa] = {
    if (versionRangeFor.isDefined && versionRangeFor.get.nonEmpty) {
      val value = versionRangeFor.get
      value.size match {
        case _ => {
          val value1 = gav.version.get
          val al = (value :+ value1).distinct.map(Version.parseSloppy).sorted.map(_.rawInput)
          val index = al.indexOf(value1)

          def toOption(ind: Int): Option[Gav3] = {
            try {
              val n = al(ind)
              Some(gav.copy(version = Some(n)))
            } catch {
              case _: Exception => {
                None
              }
            }
          }

          val maybeNext = toOption(index + 1)
          val la: Option[Gav3] = al.lastOption
            .filterNot(e => {
              val otherVersions = maybeNext.map(_.version).map(_.getOrElse("")).toSeq ++ gav.version.toSeq
              otherVersions.contains(e)
            })
            .map(n => gav.copy(version = Some(n)))
          Some(NePrLa(next = maybeNext, previous = toOption(index - 1), latest = la))

        }
      }

    } else {
      None
    }
  }

  object MismatchResult {

    val valid = MismatchResult(isMismatch = false, msg = "valid")

    def of(invalid: Boolean, msg: String): MismatchResult = {
      if (invalid) {
        problem(msg)
      } else {
        valid
      }
    }

    def problem(msg: String) = MismatchResult(isMismatch = true, msg = msg)
  }

  case class MismatchResult(isMismatch: Boolean, msg: String)

  def versionMismatches(selfVersion: String, tagBranchInfo: Option[BranchTagMerge]): MismatchResult = {
    val defaultBranchnames = Set("main", "master")

    if (tagBranchInfo.isDefined) {
      val branchN = tagBranchInfo.get.branchName
      val tagN = tagBranchInfo.get.tagName
      val tagMsg = if (tagN.isDefined) {
        s" tag: »${tagN.get}«"
      } else {
        ""
      }
      val branchMsg = if (branchN.isDefined) {
        s" branch: »${branchN.get}«"
      } else {
        ""
      }

      if (branchN.filterNot(_ == "HEAD").isDefined) {
        val selfVersionParsed = Version.parseSloppy(selfVersion)
        val branchName = branchN.get
        if (defaultBranchnames.contains(branchName)) {
          if (selfVersionParsed.isOrdinal) {
            MismatchResult.valid
          } else {
            val str = branchName + "-SNAPSHOT"
            val msg = s" project.version »$selfVersion« does not relate to git${branchMsg}. " +
              s"Please use a plausible version marker and git marker combination like: " +
              s"(project.version: $selfVersion -> git branch:${selfVersionParsed.removeSnapshot().rawInput}), " +
              s"... " +
              s"${fiWarn} ${fiCodeVersionMismatch}"
            MismatchResult.of(selfVersionParsed.rawInput != str, msg = msg)
          }
        } else {
          if (selfVersionParsed.isSnapshot) {
            if (selfVersionParsed.isOrdinal) {
              val digitsOnly = branchName
                .replaceAll("[^0-9]+", "|").split("[|]+")
                .toSeq

              val value = digitsOnly.flatMap(_.toIntOption)
              val bool = selfVersionParsed.same(value)
              val textSuggest = if (selfVersionParsed.text.length >= 3) {
                s", (project.version: ${selfVersionParsed.text}-SNAPSHOT -> git branch: feature/${selfVersionParsed.text}), "
              } else {
                ", "
              }
              val msg = s" »$selfVersion« does not relate to git${branchMsg}. " +
                s"Please use a plausible version marker and git marker combination like: " +
                s"(project.version: ${selfVersionParsed.rawInput} -> git branch: feature/${selfVersionParsed.primarys._1}x)" +
                textSuggest +
                s"... " +
                s"${fiWarn} ${fiCodeVersionMismatch}"
              MismatchResult.of(!bool, msg = msg)
            } else {
              val msg = s" »$selfVersion« does not relate to git${branchMsg}. " +
                s"Please use a plausible version marker and git marker combination." +
                // TODO improve suggestions later
                s"${fiWarn} ${fiCodeVersionMismatch}"
              MismatchResult.problem(msg)
            }
          } else {
            val msg = s" »$selfVersion« does not relate to git${tagMsg}${branchMsg}. " +
              s"Please use a plausible version marker and git marker combination like: " +
              s"(project.version: 1.2.3 -> git tag:v1.2.3), " +
              s"... " +
              s"${fiWarn} ${fiCodeVersionMismatch}"
            MismatchResult.problem(msg)
          }
        }
      } else if (tagN.isDefined) {
        val snapi = if (selfVersion.contains("SNAPSHOT")) {
          "(hint: a git tag should not be a SNAPSHOT) "
        } else {
          ""
        }
        val msg = s" »$selfVersion« does not relate to git${tagMsg}${branchMsg}. " +
          s"Please use a plausible version marker and git marker combination like: " +
          s"(project.version: 1.2.3 -> git tag:v1.2.3), " +
          s"... " +
          snapi +
          s"${fiWarn} ${fiCodeVersionMismatch}"
        MismatchResult.of(tagN.get.replaceFirst("^v", "") != selfVersion, msg = msg)
      } else if (tagBranchInfo.get.isMergeRequest) {
        MismatchResult.valid
      } else {
        if (branchN.isEmpty && tagN.isEmpty) {
          val msg = s" project.version »$selfVersion« is detached. Maybe add a ref. ${fiWarn} ${fiCodeVersionMismatch}"
          MismatchResult.problem(msg)
        } else if (branchN.contains("HEAD") && tagN.isEmpty) {
          val msg = s" project.version »$selfVersion« is detached (HEAD). Maybe add a ref. ${fiWarn} ${fiCodeVersionMismatch}"
          MismatchResult.problem(msg)
        } else {
          val msg = s" b»$selfVersion« does not relate to git${tagMsg}${branchMsg}. " +
            s"Please use a plausible version marker and git marker combination like: " +
            s"(project.version: 1.2.3 -> git tag:v1.2.3), " +
            s"(project.version: 1.2.3-SNAPSHOT -> git branch:main), " +
            s"(project.version: main-SNAPSHOT -> git branch:main), " +
            s"(project.version: some-SNAPSHOT -> git branch:feature/some), " +
            s"... " +
            s"${fiWarn} ${fiCodeVersionMismatch}"
          MismatchResult.problem(msg)
        }

      }
    } else {
      val msg = s" project.version »$selfVersion« has no git. Please add some .git folder. ${fiWarn} ${fiCodeVersionMismatch}"
      MismatchResult.problem(msg)
    }
  }

  private val MERGE_REQUEST = "MERGE_REQUEST"

  case class BranchTagMerge(tagName: Option[String], branchName: Option[String], info: String = "") {
    val isMergeRequest = info == MERGE_REQUEST
  }

  object BranchTagMerge {
    val merge = Some(BranchTagMerge(tagName = None, branchName = None, info = MERGE_REQUEST))
  }

  def toBranchTag(ciCommitRefName: String, ciCommitTag: String, currentBranchOpt: Option[String], ciCommitBranch: String,
                  currentTagsIn: Seq[String]): Option[BranchTagMerge] = {
    if (Strings.emptyToNull(ciCommitTag) == null && Strings.emptyToNull(ciCommitBranch) == null &&
      Strings.emptyToNull(ciCommitRefName) != null) {
      return BranchTagMerge.merge
    }
    val currentTags = currentTagsIn
      .filterNot(tagName => tagName.matches(".*-[0-9]+-g[0-9a-f]+$"))
    if (ciCommitRefName == ciCommitTag && currentTags.contains(ciCommitTag) && Strings.emptyToNull(ciCommitBranch) == null) {
      Some(BranchTagMerge(tagName = Some(ciCommitTag), branchName = None))
    } else if (Strings.emptyToNull(ciCommitTag) == null &&
      (currentBranchOpt.getOrElse("") == ciCommitRefName || ciCommitRefName == ciCommitBranch)) {
      Some(BranchTagMerge(tagName = currentTags.headOption, branchName = Some(ciCommitRefName)))
    } else if (currentBranchOpt.isDefined) {
      Some(BranchTagMerge(tagName = currentTags.headOption, branchName = Some(currentBranchOpt.get)))
    } else if (currentTags.nonEmpty) {
      Some(BranchTagMerge(tagName = currentTags.headOption, branchName = None)) // XXX multiple tags
    } else {
      None
    }
  }

  def isValidMergeRequest(maybeMerge: Option[BranchTagMerge]): Boolean = {
    maybeMerge.isDefined && maybeMerge.get.isMergeRequest
  }

  def isValidBranch(maybeBranch: Option[BranchTagMerge]): Boolean = {
    maybeBranch.isDefined && maybeBranch.get.branchName.isDefined && maybeBranch.get.tagName.isEmpty
  }

  def isValidTag(maybeTag: Option[BranchTagMerge]): Boolean = {
    maybeTag.isDefined && maybeTag.get.tagName.isDefined && maybeTag.get.branchName.filterNot(_ == "HEAD").isEmpty
  }

  type UniqCode = String
  var codes = Set.empty[UniqCode]

  private class CodeGen(code: String) {
    def apply(dyn: Any = null): String = {
      val suffix = if (dyn != ()) {
        "-" + Util.hashMurmur3_32_fixed(dyn.toString)
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

  val lineMax = 100_000
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
  val fiCodeNexusFoundOrphanSnapshot = uniqCode(10015)
  val fiCodeUnusualGav = uniqCode(1010)
  val fiCodeSnapshotGav = uniqCode(1011)
  val fiCodeSnapshotText = uniqCode(1012)
  val fiCodeCoreDiff = uniqCode(1013)
  val fiCodeVersionMismatch = uniqCode(1014)(())
  val fiCodeDependencyScopesCopiesOverlapping = uniqCode(1017)
  val fiCodePreviouRelease = uniqCode(1018)
  val fiWarn = "\uD83D\uDE2C"
  val fiError = "❌"

  def run(out: PrintStream, err: PrintStream, inopts: Starter.Opts,
          envs: Map[String, String],
          file: File = new File(".").getAbsoluteFile): Int = {
    val isGitlab: Boolean = envs.get("CI_COMMIT_SHA").isDefined

    val ws: String = {
      if (isGitlab) {
        "\uFEFF\u00A0\u200B"
      } else {
        ""
      }
    }

    out.println(ws)

    // TODO handle --simple-chars
    val opts = inopts.copy(checkOverlapping = false)

    out.println(info(center("[ lint ]"), opts))
    val stopwatch = Stopwatch.createStarted()
    try {
      // https://github.com/hadolint/hadolint
      // https://polaris.docs.fairwinds.com/infrastructure-as-code/

      val warnExitCode = 42
      val errorExitCode = 43
      // TODO print $HOME
      println(info("    " + file.getAbsolutePath, opts, lineMax))
      val warnExit = new AtomicBoolean(false)
      val errorExit = new AtomicBoolean(false)
      val files = file.listFiles()
      var usedSkips = Seq.empty[Lint.UniqCode]
      if (files == null || files.isEmpty) {
        out.println(error(s"E: NO FILES FOUND in ${file.getAbsolutePath}", opts))
        out.println(error(center("[ end of lint ]"), opts))
        return 1
      } else {
        out.println(info("--- skip-conf / self / env: RELEASE_LINT_SKIP, RELEASE_LINT_STRICT ---", opts))
        if (opts.lintOpts.skips.nonEmpty) {
          out.println(info(s"    skips: " + opts.lintOpts.skips.mkString(", "), opts, limit = lineMax))
        } else {
          out.println(info(s"    no skips", opts))
        }
        val sgit = Sgit(file, doVerify = false, out = out, err = err, checkExisting = true, gitBin = None, opts = Opts())
        out.println(info("--- version / git ---", opts))
        out.println(info(s"    ${fiFine} git version: " + sgit.version(), opts))
        out.println(info("--- check clone config / remote @ git ---", opts))
        val remoteHeadDefinition = sgit.remoteHead()
        if (remoteHeadDefinition.isSuccess) {
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
        out.println(info("--- check branches / remote @ git ---", opts))
        val names = sgit.listContributorMailboxes().sorted
        val branchNames = sgit.listBranchNamesAll()

        out.println(info(s"    active contributor count: ${names.size}", opts, limit = lineMax)) // TODO range?
        names.foreach(n => out.println(info(s"      ${n}", opts, limit = lineMax)))
        val branchMsg = if (branchNames == Nil) {
          ""
        } else {
          s" - ${branchNames.mkString(", ")}"
        }
        out.println(info(s"    active branch count: ${branchNames.size}${branchMsg}", opts, limit = lineMax)) // TODO limits?
        val refs = sgit.listRefs(_.commitDate)
        if (refs.nonEmpty) {
          val ssd: (Period, Period) = Lint.refFreqBranchTag(refs)
          out.println(info(s"    approx. a new branch each: ${ssd._1}, approx. a new tag each: ${ssd._2}", opts, limit = lineMax))
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
        val gitTagNames = sgit.currentTags.getOrElse(Nil)
        val tagBranchInfo = Lint.toBranchTag(ciCommitRefName, ciCommitTag, sgit.currentBranchOpt, ciCommitBranch, gitTagNames)
        val isGitOrCiTag: Boolean = Lint.isValidTag(tagBranchInfo)
        val rootFolderFiles = files.toSeq
        var pomFailures: Seq[Exception] = Nil
        val pompom: Option[Try[ProjectMod]] = if (rootFolderFiles.exists(_.getName == "pom.xml")) {
          Some(PomMod.withRepoTry(file, opts, opts.newRepo, failureCollector = Some(e => {
            pomFailures = pomFailures :+ e
          })))
        } else {
          None
        }
        val sbt: Option[Try[ProjectMod]] = if (rootFolderFiles.exists(_.getName == "build.sbt")) {
          Some(Success(SbtMod.withRepo(file, opts, opts.newRepo)))
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
            out.println(warn(s"   an INVALID branch/tag: " +
              s"ciRef: ${ciCommitRefName}, " +
              s"ciTag: ${ciCommitTag}, " +
              s"ciBranch: ${ciCommitBranch}, " +
              s"gitTags: ${gitTagNames.mkString(",")}, " +
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
          } else {
            out.println(warnSoft("   no docker tag : " + dockerTag.failed.get.getMessage, opts))
          }

        }
        out.println(info("--- -SNAPSHOTS in files @ maven/sbt/gradle ---", opts))

        val snapshotsInFiles = PomChecker.getSnapshotsInFiles(sgit.lsFilesAbsolute().map(_.getAbsolutePath))
        if (snapshotsInFiles.nonEmpty) {
          val relFiles: Seq[(Int, String, Path)] = snapshotsInFiles
            .map(t => (t._1, t._2, file.toPath.relativize(t._3.normalize())))
          val code1 = fiCodeSnapshotText(relFiles)
          var allCodes = Seq.empty[String]
          if (!opts.lintOpts.skips.contains(code1)) {
            relFiles
              .filterNot(f => {
                val code = fiCodeSnapshotText((f._1, f._2, f._3.getFileName))
                val bool = opts.lintOpts.skips.contains(code)
                if (bool) {
                  usedSkips = usedSkips :+ code
                } else {
                  allCodes = allCodes :+ code
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
            val snapshotSumMsg = s"  found snapshots: ${fiWarn} $code1 -- ${allCodes.sorted.mkString(", ")}"
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
          out.println(info("--- model read @ maven/sbt/gradle ---", opts))
          if (modTry.isSuccess) {
            out.println(info("    ✅ successfull created", opts))
          } else {
            warnExit.set(true)
            out.println(warn(s"    ${fiWarn} ${modTry.failed.get.getMessage}", opts, limit = lineMax))
          }
          if (modTry.isSuccess) {
            val mod = modTry.get
            out.println(info("--- dependency scopes/copies/overlapping @ maven/sbt ---", opts))
            val msgs = PomChecker.getDepScopeAndOthers(mod.listDependencies, mod.listRawDeps) ++
              PomChecker.getDepVersionsProblems(mod.listDependencies, mod.listRawDeps)

            if (msgs.nonEmpty) {
              val code1 = fiCodeDependencyScopesCopiesOverlapping.apply(msgs)

              out.println(warn(s" found scopes/copies/overlapping ${fiWarn} ${code1}", opts, limit = lineMax))
              msgs.foreach(k => out.println(warn(" " + k, opts, limit = lineMax)))
              if (!opts.lintOpts.skips.contains(code1)) {
                warnExit.set(true)
              } else {
                usedSkips = usedSkips :+ code1
                usedSkips = usedSkips :+ code1
              }
            } else {
              out.println(info("    ✅ no warnings found", opts))
            }
            if (mod.isInstanceOf[PomMod]) {
              out.println(info("--- artifactnames @ maven ---", opts))
              val checkResult = PomChecker.getOwnArtifactNames(mod.depInFiles, mod.file)
              checkResult._1.foreach(name => out.println(info("    " + name.formatted, opts, limit = lineMax)))
              if (checkResult._2.isDefined) {
                Term.wrap(out, Term.warn, "   " + checkResult._2.get, opts)
              }
              out.println(info("--- .mvn @ maven ---", opts))
              out.println(info("    WIP", opts)) // TODO check extensions present
            }
            out.println(info("--- project version @ maven ---", opts))
            usedSkips = usedSkips ++ LintMaven.lintProjectVersion(out, opts, modTry.get.selfVersion, warnExit, errorExit, tagBranchInfo,
              sgit.listAllTags())

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
              out.println(warn(s"known scopes are: ${ProjectMod.knownScopes.filterNot(_.isEmpty).toSeq.sorted.mkString(", ")}", opts))
              out.println(warn(s"version ranges are not allowed", opts))
              out.println(warn(s"unstable marker like LATEST and RELEASE are not allowed", opts))
              warnExit.set(true)
            } else {
              out.println(info(s"    ${fiFine} all GAVs scopes looks fine", opts))
            }
            out.println(info("--- check for preview releases @ maven ---", opts))
            val updatePrinter = new StaticPrinter()
            val updateOpts = opts.depUpOpts.copy(hideStageVersions = true, allowDependencyDowngrades = true, showLibYears = true)
            val resultTry = mod.tryCollectDependencyUpdates(updateOpts, checkOn = true, updatePrinter, ws = ws)
            val lookupUpAndDowngrades: Map[Gav3, Seq[String]] = if (resultTry.isSuccess) {
              resultTry.get._1.map(t => (t._1.gav.simpleGav(), t._2.map(_._1))).toMap
            } else {
              Map.empty
            }
            val warnExitForDepCheck = new AtomicBoolean(false)
            mod.listGavsForCheck()
              .filter(dep => ProjectMod.isUnwanted(dep.gav().simpleGav()))
              .filterNot(_.version.get.endsWith("-SNAPSHOT"))
              .foreach(dep => {
                val code = fiCodePreviouRelease.apply(dep.gav())
                val skipped = opts.lintOpts.skips.contains(code)
                if (skipped) {
                  usedSkips = usedSkips :+ code
                } else {
                  warnExitForDepCheck.set(true)
                }
                out.println(warn("  found preview: " + dep.gav().formatted + s" ${fiWarn} ${code}", opts, limit = lineMax, soft = skipped))
                if (resultTry.isSuccess) {
                  val gav = dep.gav().simpleGav()
                  val versionRangeFor = lookupUpAndDowngrades.get(gav)
                  val npo = Lint.selectNextAndPrevious(versionRangeFor, gav)
                  if (npo.isDefined) {
                    val nextAndPrev = npo.get
                    if (nextAndPrev.next.isDefined) {
                      out.println(warn("       next    : " + nextAndPrev.next.get.formatted, opts, limit = lineMax, soft = skipped))
                    }
                    if (nextAndPrev.latest.isDefined) {
                      out.println(warn("       latest  : " + nextAndPrev.latest.get.formatted, opts, limit = lineMax, soft = skipped))
                    }
                    if (nextAndPrev.previous.isDefined) {
                      out.println(warn("       previous: " + nextAndPrev.previous.get.formatted, opts, limit = lineMax, soft = skipped))
                    }
                  }
                }
              })
            if (warnExitForDepCheck.get()) {
              warnExit.set(true)
            }
            out.println(info("--- check major versions @ ishop ---", opts))
            out.println(info(s"    is shop: ${mod.isShop}", opts))
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

            val releasenexusworkurl: String = ReleaseConfig.releaseNexusEnv(envs).orNull
            val repo = mod.repo
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
              out.println(Util.show(resultTry.get._2.getMetrics)) // TODO improve format
              val updateResult = ProjectMod.removeOlderVersions(resultTry.get._1)
              val snapUpdates = updateResult.filter(e => snaps.map(_.gav().simpleGav()).contains(e._1.gav.simpleGav()))
              val releaseOfSnapshotPresent = snapUpdates
                // TODO flatmap
                .map(e => (e._1.gav, e._2.map(_._1).contains(e._1.gav.version.get.replaceFirst("-SNAPSHOT", ""))))
                .filter(_._2)
              if (releaseOfSnapshotPresent.nonEmpty) {
                // TODO handle filter fiCode
                releaseOfSnapshotPresent.map(_._1).foreach(found => {
                  out.println(warn(s"${found.formatted} is already released, remove '-SNAPSHOT' suffix ${fiWarn} ${fiCodeNexusFoundRelease(found)}", opts, limit = lineMax))
                })
                warnExit.set(true)
              }
              val nextReleaseOfSnapshotPresent = snapUpdates.flatMap(e => {
                val currentVersion = e._1.gav.version.get
                val bool = e._2.map(_._1).contains(currentVersion.replaceFirst("-SNAPSHOT", ""))
                val version = Version.parseSloppy(currentVersion)
                if (!bool && version.isOrdinalOnly) {
                  val otherVersions = (e._2.map(_._1) :+ currentVersion).map(Version.parseSloppy).sorted
                  val wfe = otherVersions.dropWhile(_ == version).headOption
                  if (wfe.isDefined) {
                    Some((e._1.gav, wfe.get))
                  } else {
                    None
                  }
                } else {
                  None
                }
              })
              if (nextReleaseOfSnapshotPresent.nonEmpty) {
                val withCode = nextReleaseOfSnapshotPresent
                  .map(t => (t._1, t._2, fiCodeNexusFoundOrphanSnapshot(t)))
                  .filterNot(t => {
                    val bool = opts.lintOpts.skips.contains(t._3)
                    if (bool) {
                      usedSkips = usedSkips :+ t._3
                    }
                    bool
                  })
                withCode
                  .foreach(found => {
                    out.println(warn(s"${found._1.formatted} is not released, but next release (${found._2.rawInput}) " +
                      s"was found (maybe orphan snapshot) ${fiWarn} ${found._3}", opts, limit = lineMax))
                  })
                if (withCode.nonEmpty) {
                  warnExit.set(true)
                }

              }

            } catch {
              case pce: PreconditionsException => {
                out.println(warn(pce.getMessage + s"${fiWarn} ${fiCodePomModPreconditionsException}", opts, limit = lineMax))
                warnExit.set(true)
              }
              case pce: Exception => {
                val newStackTrace = filteredStacktrace(pce.getStackTrace)
                pce.setStackTrace(newStackTrace)
                pce.printStackTrace(err)
                out.println(error(pce.getMessage + s" ${fiWarn} ${fiCodePomModException}", opts, limit = lineMax))
                if (!opts.lintOpts.skips.contains(fiCodePomModException)) {
                  errorExit.set(true)
                } else {
                  usedSkips = usedSkips :+ fiCodePomModException
                  warnExit.set(true)
                }
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
        val packageNamesDetails = Lint.findAllPackagenames(file, opts.lintOpts.checkPackages)
        if (packageNamesDetails.names.nonEmpty) {
          out.println(info("--- unwanted-packages @ ishop ---", opts))
          if (packageNamesDetails.msg.nonEmpty) {
            out.println(warn(packageNamesDetails.msg, opts, limit = lineMax))
          }
          val nameSize = packageNamesDetails.names.size
          out.println(info(s"    found $nameSize package ${"name".pluralize(nameSize)} in ${packageNamesDetails.d.toString}", opts))
          if (packageNamesDetails.unwantedPackages.nonEmpty) {
            packageNamesDetails.unwantedPackages.foreach(line => {
              out.println(warn(s" package »${line}« is in list of unwanted packages, please avoid this package", opts, limit = lineMax))
            })
          } else {
            out.println(info(s"    ${fiFine} no problematic packages found", opts))
          }
          out.println(info(s"    .unwanted-packages // checksum ${packageNamesDetails.unwantedDefinitionSum}", opts))
        }
        if (sbt.isDefined) {
          out.println(info("--- ??? @ sbt ---", opts))
          out.println(info("    WIP", opts))
        }
        if (opts.lintOpts.skips.nonEmpty) {
          val unusedSkips = opts.lintOpts.skips.diff(usedSkips)
          if (unusedSkips.nonEmpty) {
            out.println(info("--- skip-conf / self / end ---", opts))
            out.println(warn(s"    found unused skips, please remove from your config: " + unusedSkips.sorted.mkString(", "), opts, limit = lineMax))
            out.println(warn(s"    active skips: " + opts.lintOpts.skips.diff(unusedSkips).sorted.mkString(", "), opts, limit = lineMax))
            warnExit.set(true)
          }
        }

        out.println(ws)
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

  def filteredStacktrace(in: Array[StackTraceElement]): Array[StackTraceElement] = {
    in
      .filterNot(_.getClassName.startsWith("jdk.internal."))
      .filterNot(_.getClassName.startsWith("java.lang."))
      .filterNot(_.getClassName.startsWith("java.util."))
      .filterNot(_.getClassName.startsWith("scala."))
      .filterNot(_.getClassName.startsWith("sbt."))
      .filterNot(_.getClassName.startsWith("com.novocode.junit."))
      .filterNot(_.getClassName.startsWith("org.junit."))
      .filterNot(_.getClassName.startsWith("com.intellij."))
      .filterNot(k => k.getClassName.startsWith("release.") &&
        (k.getClassName.endsWith("Test$") || k.getClassName.endsWith("Test")))
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
