package release

import java.io.{File, InputStream, PrintStream}
import java.nio.charset.MalformedInputException
import java.nio.file.{Files, InvalidPathException, Path, Paths}
import java.time.LocalDate
import java.util.concurrent.atomic.AtomicBoolean
import java.util.regex.Pattern
import com.typesafe.scalalogging.LazyLogging
import release.ProjectMod.{Dep, Gav, Version}
import release.Starter.{Opts, PreconditionsException}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Release extends LazyLogging {

  def formatVersionLinesGav(versionLines: Seq[Gav], color: Boolean = false): Seq[String] = {
    def gavLength(in: Gav) = Seq(in.groupId, in.artifactId).mkString(":").length

    val max = versionLines.map(gavLength).max
    lazy val maxString = versionLines.map(_.version).groupBy { i =>
      if (i.contains(".")) {
        i.replaceFirst("\\..*", "")
      } else {
        i
      }
    }.toList.map(in => (in._1, in._2.size, in._2))
      .sortBy(_._1)
      .reverse.sortBy(-_._2).drop(1).flatMap(_._3).distinct

    val lines = versionLines
      .sortBy(gav => Version.parse(gav.version))
      .map { in =>
        val baseLength = gavLength(in)
        val spaces = " " * (max - baseLength)

        val v = if (color && maxString.contains(in.version)) {
          "\u001B[31m" + in.version + "\u001B[0m"
        } else {
          in.version
        }
        "* " + Seq(in.groupId, in.artifactId, spaces).mkString(":") + "  " + v
      }
      .distinct
    lines
  }

  def findBadLines(regexp: Pattern)(aFileName: String): Seq[(Int, String, Path)] = {
    try {
      val path = Paths.get(aFileName)
      val lines = if (Files.isRegularFile(path)) {
        Util.readLines(path.toFile).zipWithIndex
      } else {
        Nil
      }

      lines.par.flatMap(line => {
        val matcher = regexp.matcher("")
        matcher.reset(line._1)
        if (matcher.find) {
          Seq((line._2, line._1, path))
        } else {
          Nil
        }
      }).seq.toList
    } catch {
      case _: MalformedInputException => {
        Nil
      }
      case e: InvalidPathException => {
        println("W: " + e.getMessage + " " + " < " + aFileName)
        Nil
      }

      case e: Exception => {
        println("W: " + e.getClass.getCanonicalName + " " + e.getMessage + " " + " < " + aFileName)
        e.printStackTrace()
        Nil
      }
    }

  }

  def localChangeMessage(sgit: Sgit): String = {
    if (sgit.hasLocalChanges) {
      val changes = sgit.localChanges().take(5)
      changes match {
        case c if c.size <= 5 => c.mkString("\n")
        case c => c.mkString("\n") + "\n..."
      }
    } else {
      ""
    }
  }

  def checkLocalChanges(sgit: Sgit, branch: String) = {
    if (sgit.hasLocalChanges) {
      val changesOut = localChangeMessage(sgit)
      throw new PreconditionsException("Your branch: \"" + branch + "\" has local changes, please commit or reset\n" + changesOut)
    }
  }

  // TODO @tailrec
  def work(workDirFile: File, sys: Term.Sys, rebaseFn: () => Unit, branch: String, sgit: Sgit,
           termOs: Term, shellWidth: Int, releaseToolGitSha1: String, config: ReleaseConfig,
           repo: Repo, opts: Opts): Seq[Unit] = {
    if (sgit.hasLocalChanges) {
      val message = localChangeMessage(sgit)
      sys.out.println(message)
      val changes = Term.readFromOneOfYesNo(sys, "You have local changes. Add changes to stash?", opts)
      if (changes == "y") {
        sgit.stash()
        Starter.addExitFn("cleanup branches", () => {
          sgit.stashPop()
        })
      } else if (changes == "n") {
        checkLocalChanges(sgit, branch)
      } else {
        work(workDirFile, sys, rebaseFn, branch, sgit, termOs, shellWidth, releaseToolGitSha1, config, repo, opts)
      }

    }
    rebaseFn.apply()
    Starter.addExitFn("cleanup branches", () => {
      sgit.checkout(sgit.currentBranch)
    })
    sgit.checkout(branch)
    Starter.chooseUpstreamIfUndef(sys, sgit, branch, opts)

    val mod: ProjectMod = ProjectMod.read(workDirFile, sys, opts, repo)

    sys.out.println(". done")
    if (opts.depUpOpts.showDependencyUpdates) {
      mod.showDependencyUpdates(shellWidth, termOs, opts.depUpOpts, config.workNexusUrl(), sys)
      System.exit(0)
    }

    val wipMod = offerAutoFixForReleaseSnapshots(sys, mod, sgit.lsFiles(), shellWidth, repo, opts)

    @tailrec
    def checkLocalChangesAfterSnapshots(mod: ProjectMod): ProjectMod = {
      if (sgit.hasLocalChanges) {
        sys.out.println(localChangeMessage(sgit))
        val retryLocalChanges = Term.readFromOneOfYesNo(sys, "Found local changes - commit manual please. Retry?", opts)
        if (retryLocalChanges == "n") {
          System.exit(0)
          mod
        } else {
          checkLocalChangesAfterSnapshots(ProjectMod.read(mod.file, sys, opts, repo, showRead = false))
        }
      } else {
        ProjectMod.read(mod.file, sys, opts, repo, showRead = false)
      }

    }

    val newMod = checkLocalChangesAfterSnapshots(wipMod)

    if (newMod.isNoShop) {
      sys.out.println("---------")
      sys.out.println("1. MAJOR version when you make incompatible API changes,")
      sys.out.println("2. MINOR version when you add functionality in a backwards-compatible manner, and")
      sys.out.println("3. PATCH version when you make backwards-compatible bug fixes.")
      sys.out.println("   see also: http://semver.org/")
      sys.out.println("---------")
    } else {
      sys.out.println(s"I: Current week of year: ${PomMod.weekOfYear(LocalDate.now())}")
    }

    val knownTags = sgit.listTagsWithDate().map(_.name)
    val suggestedVersions = newMod.suggestReleaseVersion(sgit.listBranchNamesAll(), knownTags, opts.versionIncrement)

    @tailrec
    def readReleaseVersions: String = {
      val result = PomMod.checkNoSlashesNotEmptyNoZeros(
        Term.readChooseOneOfOrType(sys, "Enter the release version", suggestedVersions, opts, e => e.last, m => m.last))
      if (PomMod.isUnknownVersionPattern(result)) {
        if (mod.isShop) {
          sys.out.println("I: We prefer:")
          sys.out.println("I: RC-{YEAR}.{WEEK OF YEAR} for common releases")
          sys.out.println("I: RC-{YEAR}.{WEEK OF YEAR}.{NUMBER} for intermediate releases")
        }

        val latestTags = knownTags.take(5)
        if (latestTags.nonEmpty) {
          sys.out.println("Latest version tag names are: " + latestTags.mkString(", ")) // TODO sort
          sys.out.println("Latest version tag matching are: " + "latestTags".mkString(", ")) // TODO sort
        }
        val retryVersionEnter = Term.readFromOneOfYesNo(sys, "Unknown release version name: \"" + result + "\".\n" +
          PomMod.trySuggestKnownPattern(result).getOrElse(s"W: suggestion failed for '${result}'\n") +
          "_unknown_ versions may affect creation and cleanup of build jobs and artifacts\n" +
          " and/or publishing of artifacts to partners.\n" +
          " Are you sure to continue with this name?", opts)

        if (retryVersionEnter == "n") {
          readReleaseVersions
        } else {
          result
        }
      } else {
        result
      }
    }

    val releaseWithoutSnapshot = if (opts.versionIncrement.isDefined) {
      suggestedVersions.head
    } else {
      readReleaseVersions
    }

    sys.out.println(s"Selected release is ${releaseWithoutSnapshot}")
    val release = if (mod.isShop) {
      Term.removeTrailingSnapshots(releaseWithoutSnapshot) + "-SNAPSHOT"
    } else {
      releaseWithoutSnapshot
    }
    val releaseWitoutSnapshot = Term.removeTrailingSnapshots(release)
    if (mod.isNoShop && sgit.listTags().contains("v" + releaseWitoutSnapshot)) {
      // TODO fetch remote and try to read new version again
      throw new IllegalStateException("release " + releaseWitoutSnapshot + " already found; check your repository or change version.")
    }

    @tailrec
    def readNextReleaseVersionsWithoutSnapshot: String = {
      val result = Term.removeTrailingSnapshots(PomMod.checkNoSlashesNotEmptyNoZeros(Term.readFrom(sys, "Enter the next version without -SNAPSHOT",
        newMod.suggestNextRelease(release), opts)))
      if (PomMod.isUnknownVersionPattern(result)) {
        val retryVersionEnter = Term.readFromOneOfYesNo(sys, "Unknown next release version \"" + result + "\". Are you sure to continue?", opts)
        if (retryVersionEnter == "n") {
          readNextReleaseVersionsWithoutSnapshot
        } else {
          result
        }
      } else {
        result
      }
    }

    // TODO release a feature branch should not change the next version
    val nextReleaseWithoutSnapshot = readNextReleaseVersionsWithoutSnapshot

    val relevantDeps = if (mod.isNoShop) {
      mod.listDependecies.filter(in => in.groupId.startsWith("com.novomind.ishop.core"))
    } else {
      val selfGavs = mod.selfDepsMod.map(_.gav())
      mod.listDependecies
        .filter(in => in.groupId.startsWith("com.novomind.ishop"))
        .filterNot(in => selfGavs.contains(in.gav()))
    }
    val releaseMajorVersion = if (mod.isNoShop) {
      release.replaceAll("\\..*", "")
    } else {
      relevantDeps.map(_.version).maxBy(Version.parse).replaceAll("\\..*", "")
    }

    val relevantFilteredDeps = if (releaseMajorVersion.matches("[0-9]+")) {
      if (releaseMajorVersion.toInt > 36) {
        relevantDeps
          .filterNot(in => in.groupId == "com.novomind.ishop.shops" &&
            in.artifactId == "ishop-shop-parent" &&
            in.version.startsWith("36")) // TODO improve to .toInt > 36 later
          .filterNot(in => in.groupId == "com.novomind.ishop" &&
            in.artifactId == "ishop-meta-parent" &&
            in.version.startsWith("36")) // TODO improve to .toInt > 36 later
      } else if (releaseMajorVersion.toInt >= 32) {
        relevantDeps
      } else {
        relevantDeps
          .filterNot(in => in.groupId == "com.novomind.ishop.exi" && in.artifactId == "ishop-ext-authentication")
      }
    } else {
      relevantDeps
    }

    val coreVersions: Seq[(String, Dep)] = relevantFilteredDeps
      .map(in => (in.version, in))
      .distinct
      .filter(_._1.nonEmpty)
      .sortBy(_._1)
    val coreMajorVersions: Seq[(String, Dep)] = coreVersions
      .map(in => (in._1.replaceAll("\\..*", "").replaceAll("\\D+", "").trim, in._2))
      .distinct
      .filter(_._1.nonEmpty)
      .sortBy(_._1)

    val sortedMajors = coreMajorVersions.map(_._1).distinct.sortBy(Version.parse)
    if (coreMajorVersions != Nil && sortedMajors != Seq(releaseMajorVersion)) {
      sys.out.println()
      if (opts.colors) {
        sys.out.print("\u001B[30;45m")
      }
      if (mod.isNoShop) {
        sys.out.print(" W: You are trying to release major version " + releaseMajorVersion + " (" + release + ")")
      } else {
        sys.out.print(" W: You are trying to use core major version " + releaseMajorVersion)
      }
      sys.out.print(" but this artifact refers to: " + sortedMajors.mkString(" and ") + ".")
      if (opts.colors) {
        sys.out.print("\u001B[0m")
      }
      sys.out.println()
      if (opts.colors) {
        sys.out.print("\u001B[30;45m")
      }
      sys.out.print("    We prefer consistent major versions. So please update all projects to same major version.")
      if (opts.colors) {
        sys.out.print("\u001B[0m")
      }
      sys.out.println()
      sys.out.println()
      Release.formatVersionLinesGav(coreMajorVersions.map(_._2.gav()).sortBy(_.version), opts.colors)
        .map(in => " " + in)
        .foreach(sys.out.println)
      val continue = Term.readFromOneOfYesNo(sys, "Continue?", opts)
      if (continue == "n") {
        System.exit(1)
      } else {
        val really = Term.readFromOneOf(sys, "Really?", Seq("Yes I'm really sure", "n"), opts)
        if (really == "n") {
          System.exit(1)
        } else {
          val abort = Term.readFromOneOfYesNo(sys, "Abort?", opts)
          if (abort == "y") {
            System.exit(1)
          }
        }
      }
    }

    // TODO hier könnte man jetzt die snapshots aus "mod" in "newMod" suchen und sie auf den folgesnapshot setzen

    val nextSnapshot = nextReleaseWithoutSnapshot + "-SNAPSHOT"
    if (newMod.selfVersion != nextSnapshot) {
      newMod.changeVersion(nextSnapshot)
    }

    @tailrec
    def checkReleaseBranch(): Unit = {
      if (sgit.listBranchNamesLocal().contains("release")) {
        val changes = Term.readFromOneOfYesNo(sys, "You have a local branch with name 'release'. " +
          "We use this name for branch creation. Delete this branch manually. Abort release?", opts)
        if (changes == "y") {
          System.exit(1)
        } else {
          checkReleaseBranch()
        }
      }
    }

    checkReleaseBranch()
    val releaseBrachName = "release/" + releaseWitoutSnapshot
    sgit.createBranch(releaseBrachName)
    if (newMod.selfVersion != nextSnapshot) {
      newMod.writeTo(workDirFile)
    }
    val headCommitId = sgit.commitIdHead()
    val releaseMod = ProjectMod.read(workDirFile, sys, opts, repo, showRead = false)
    val msgs = opts.skipProperties match {
      case Nil => ""
      case found => "\nReleasetool-Prop-Skip: " + found.mkString(", ")
    }
    val changedVersion = if (sgit.hasNoLocalChanges) {
      sys.out.println("skipped release commit on " + branch)
      false
    } else {

      sys.out.print("Committing pom changes ..")
      if (opts.useGerrit) {
        sgit.doCommitPomXmlsAnd(
          """[%s] prepare for next iteration - %s
            |%s
            |Signed-off-by: %s
            |Releasetool-sign: %s
            |Releasetool-sha1: %s""".stripMargin.format(config.releasPrefix(), nextReleaseWithoutSnapshot,
            msgs, config.signedOfBy(), Starter.sign(sgit), releaseToolGitSha1),
          releaseMod.depTreeFilenameList())
      } else {
        sgit.doCommitPomXmlsAnd(
          """[%s] prepare for next iteration - %s
            |%s
            |Releasetool-sign: %s
            |Releasetool-sha1: %s""".stripMargin.format(config.releasPrefix(), nextReleaseWithoutSnapshot,
            msgs, Starter.sign(sgit), releaseToolGitSha1),
          releaseMod.depTreeFilenameList())
      }

      sys.out.println(". done")
      true
    }
    sys.out.print("Checking out " + releaseBrachName + " ..")
    sgit.checkout(releaseBrachName)
    sys.out.println(". done")

    if (releaseMod.selfVersion != release) {
      releaseMod.changeVersion(release)
      releaseMod.writeTo(workDirFile)
    }

    if (sgit.hasNoLocalChanges) {
      sys.out.println("skipped release commit on " + releaseBrachName)
    } else {
      sys.out.print("Commiting pom changes ..")
      if (opts.useGerrit) {
        sgit.doCommitPomXmlsAnd(
          """[%s] perform to - %s
            |%s
            |Signed-off-by: %s
            |Releasetool-sign: %s
            |Releasetool-sha1: %s""".stripMargin.format(config.releasPrefix(), release,
            msgs, config.signedOfBy(), Starter.sign(sgit), releaseToolGitSha1), releaseMod.depTreeFilenameList())
      } else {
        sgit.doCommitPomXmlsAnd(
          """[%s] perform to - %s
            |%s
            |Releasetool-sign: %s
            |Releasetool-sha1: %s""".stripMargin.format(config.releasPrefix(), release,
            msgs, Starter.sign(sgit), releaseToolGitSha1), releaseMod.depTreeFilenameList())
      }
      sys.out.println(". done")
    }
    if (releaseMod.isNoShop) {
      sgit.doTag(release)
    }
    sys.out.print("Checking out " + branch + " ..")
    sgit.checkout(branch)
    sys.out.println(". done")
    if (newMod.isNoShop) {
      sgit.deleteBranch(releaseBrachName)
    }
    sys.out.println(sgit.graph())

    val selectedBranch = sgit.findUpstreamBranch().getOrElse(branch)

    def showManual(): Unit = {
      val pushTagOrBranch = if (newMod.isNoShop) {
        "git push origin tag v" + releaseWitoutSnapshot + "; "
      } else {
        "git push -u origin release/" + releaseWitoutSnapshot + ":release/" + releaseWitoutSnapshot + ";"
      }
      val deleteTagOrBranch = if (newMod.isNoShop) {
        "git tag -d v" + release + "; "
      } else {
        "git branch -D release/" + releaseWitoutSnapshot + ";"
      }
      val resetCmd = if (changedVersion) {
        s"git reset --hard ${headCommitId};"
      } else {
        ""
      }

      val pushCmd = if (changedVersion && sgit.isNotDetached) {
        s"git push origin ${branch}:refs/for/${selectedBranch};"
      } else {
        ""
      }

      sys.out.println(
        ("""commands for local rollback:
           |  """ + (resetCmd + " " + deleteTagOrBranch).trim +
          """
            |
            |commands for sending to remote:
            |  """ + (pushCmd + " " + pushTagOrBranch).trim +
          """
            |NOTE: the "send to remote" command pushes the HEAD via Gerrit Code Review, this might not be needed for branches != master""").stripMargin)
    }

    if (opts.useGerrit) {
      val pushed = new AtomicBoolean(false)
      val sendToGerrit = Term.readFromOneOfYesNo(sys, "Push to Gerrit and publish release?", opts)
      if (sendToGerrit == "y") {
        try {
          if (sgit.hasChangesToPush || sgit.hasTagsToPush) {
            if (sgit.isNotDetached) {
              // ask to push version change to review;
              val pushOut = sgit.pushFor(srcBranchName = branch, targetBranchName = selectedBranch)
              pushOut.foreach(sys.err.println)
              pushed.set(true)
              if (pushOut == Nil) {
                logger.trace(s"pushed ${branch} to refs/for/${selectedBranch}")
              } else {
                logger.trace(s"maybe pushed ${branch} to refs/for/${selectedBranch} - ${pushOut.mkString(",")}")
              }
              if (config.openGerritInBrowser) {
                // TODO hier gerrit öffnen da man submit klicken muss
                // TODO wenn man genau den change öffnen könnte wär noch cooler
                // TODO try to exctract gerrit url from repo
                Starter.openInDefaultBrowser(config.gerritBaseUrl() + "#/q/status:open")
              }
            }
            if (newMod.isNoShop) {
              val pushOut = sgit.pushTag(release)
              pushed.set(true)
              pushOut.foreach(sys.err.println)
              if (pushOut == Nil) {
                logger.trace(s"pushed tag ${release}")
              } else {
                logger.trace(s"maybe pushed tag ${release} - ${pushOut.mkString(",")}")
              }
              if (config.openJenkinsInBrowser) {
                val jenkinsBase = config.jenkinsBaseUrl()
                val tagUrl = Starter.tagBuildUrl(sgit, jenkinsBase, release)
                // TODO hier erstmal nur den browser auf machen damit man build tag klicken kann
                Starter.openInDefaultBrowser(tagUrl.getOrElse(jenkinsBase + "/search/?q=-tag&max=1000"))
                // try to notify jenkins about tag builds
                // TODO try to wait for successful tag builds ... subscribe to logs
              }
            }
          }
          if (newMod.isShop) {
            val pushOut = sgit.pushHeads(srcBranchName = "release/" + releaseWitoutSnapshot,
              targetBranchName = "release/" + releaseWitoutSnapshot)
            pushOut.foreach(sys.err.println)
            pushed.set(true)
            if (pushOut == Nil) {
              logger.trace(s"pushed release ${releaseWithoutSnapshot}")
            } else {
              logger.trace(s"maybe pushed release ${releaseWithoutSnapshot} - ${pushOut.mkString(",")}")
            }
            // TODO try to trigger job updates for jenkins
            // TODO try to trigger job execution in loop with abort
          }
          if (pushed.get()) {
            sys.out.println("done.")
          } else {
            sys.out.println("maybe nothing done.")
            showManual()
          }
        } catch {
          case e: RuntimeException => {
            sys.err.println("E: Push failed - try manual - " + e.getMessage)
            showManual()
          }
        }
      } else {
        showManual()
      }
    } else {
      sys.err.println("W: No gerrit -> push is not implemented")
    }

    Nil
  }

  // TODO @tailrec
  def offerAutoFixForReleaseSnapshots(sys: Term.Sys, mod: ProjectMod, gitFiles: Seq[String], shellWidth: Int,
                                      repo: Repo, opts: Opts): ProjectMod = {
    val plugins = mod.listPluginDependencies
    if (mod.isShop) {
      // TODO check if core needs this checks too
      PomChecker.checkPlugins(plugins)
    }
    PomChecker.checkGavFormat(mod.listDependecies ++ mod.listPluginDependencies.map(_.fakeDep()), sys.out)

    val snapsF = gitFiles.par
      .filterNot(in => in.endsWith(".list"))
      .filterNot(in => in.endsWith(".tree"))
      .filterNot(in => in.endsWith(".java"))
      .filterNot(in => in.endsWith(".png"))
      .filterNot(in => in.endsWith(".potx"))
      .filterNot(in => in.endsWith(".jpg"))
      .filterNot(in => in.matches(".*[/\\\\]src[/\\\\]test[/\\\\]resources[/\\\\]app\\-manifest.*"))
      .filterNot(in => in.endsWith("pom.xml"))
      .flatMap(findBadLines(Pattern.compile("-SNAPSHOT")))
      .seq
      .sortBy(_._3)

    if (snapsF != Nil) {
      println()
      println("Warning: Found SNAPSHOT occurrences in following files")
      snapsF.foreach(in => {
        println(in._3.toFile.getAbsolutePath + ":" + in._1)
        println("  " + in._2.trim())
      })
      println()
    }

    case class ReleaseInfo(gav: String, released: Boolean)

    val noShops: (Gav => Boolean) = if (mod.isNoShop) {
      gav: Gav => gav.groupId.contains("com.novomind.ishop.shops")
    } else {
      _ => false
    }

    val boClientVersion = mod.listProperties
      .filter(_._1 == "bo-client")
      .filter(_._2.contains("-SNAPSHOT")).values

    val snaps: Seq[Gav] = mod.listSnapshotsDistinct
      .map(_.gav())
      .filterNot(noShops) ++ plugins.map(_.fakeDep().gav())
      .filter(_.version.contains("SNAPSHOT")) ++
      boClientVersion.map(in => Gav("com.novomind.ishop.backoffice", "bo-client", in, "war"))

    val aetherStateLine = StatusLine(snaps.size, shellWidth)
    val snapState: Seq[ReleaseInfo] = snaps
      .par
      .map(in => {
        aetherStateLine.start()
        val released = repo.existsGav(in.groupId, in.artifactId, in.version.replace("-SNAPSHOT", ""))
        aetherStateLine.end()
        ReleaseInfo(in.formatted, released)
      }).seq
    aetherStateLine.finish()

    val snapshotProperties = mod.listProperties
      .filter(_._2.contains("-SNAPSHOT"))
      .filterNot(_._1 == "project.version")

    if (snapState.nonEmpty || snapshotProperties.nonEmpty) {
      if (snapshotProperties.nonEmpty) {
        sys.out.println("")
        sys.out.println("Snapshot properties found for (please fix manually in pom.xml (remove -SNAPSHOT in most cases)):")
        snapshotProperties.toList.map(in => "Property: " + in).foreach(println)
      }
      if (snapState.nonEmpty) {
        sys.out.println("")
        // TODO later autofix
        // mod.changeDependecyVersion()
        sys.out.println("Snapshots found for (please fix manually in pom.xml (remove -SNAPSHOT in most cases)):")
      }

      def info(rel: Boolean): String = if (rel) {
        "  Release found for "
      } else {
        "  No Release for    "
      }

      snapState
        .sortBy(_.toString)
        .map(in => info(in.released) + in.gav)
        .foreach(println)
      sys.out.println("")

      val again = Term.readFromOneOfYesNo(sys, "Try again?", opts)
      if (again == "n") {
        System.exit(1)
      } else {
        offerAutoFixForReleaseSnapshots(sys, ProjectMod.read(mod.file, sys, opts, repo, showRead = false), gitFiles, shellWidth, repo, opts)
      }
    }
    mod
  }

}
