package release

import java.io.{File, PrintStream}
import java.nio.charset.MalformedInputException
import java.nio.file.{InvalidPathException, Path, Paths}
import java.util.regex.Pattern

import release.PomMod.{Dep, Gav}
import release.Starter.{PreconditionsException, TermOs}

object Release {
  def formatVersionLines(versionLines: Seq[String]): Seq[String] = {
    val maxLenght = versionLines.map(_.replaceFirst("[^:]*$", "")).map(_.length).max + 2
    versionLines.map(in ⇒ {
      val baseLenght = in.replaceFirst("[^:]*$", "").length
      val spaces = List.fill(maxLenght - baseLenght)(" ").mkString("")
      in.replaceFirst("^(.*:)([^:]*$)", "$1" + spaces + "$2")
    })

  }

  def findBadLines(regexp: Pattern)(aFileName: String): Seq[(Int, String, Path)] = {
    try {
      val path = Paths.get(aFileName)
      val lines = Util.readLines(path.toFile).zipWithIndex

      lines.par.flatMap(line ⇒ {
        val matcher = regexp.matcher("")
        matcher.reset(line._1)
        if (matcher.find) {
          Seq((line._2, line._1, path))
        } else {
          Nil
        }
      }).seq.toList
    } catch {
      case _: MalformedInputException ⇒ {
        Nil
      }
      case e: InvalidPathException ⇒ {
        println("W: " + e.getMessage + " " + " < " + aFileName)
        Nil
      }

      case e: Exception ⇒ {
        println("W: " + e.getClass.getCanonicalName + " " + e.getMessage + " " + " < " + aFileName)
        e.printStackTrace()
        Nil
      }
    }

  }

  def checkLocalChanges(sgit: Sgit, branch: String) = {
    if (sgit.hasLocalChanges) {
      val changes = sgit.localChanges().take(5)
      val changesOut = changes match {
        case c if c.size <= 5 ⇒ c.mkString("\n")
        case c ⇒ c.mkString("\n") + "\n..."
      }
      throw new PreconditionsException("Your branch: \"" + branch + "\" has local changes, please commit or reset\n" + changesOut)
    }
  }

  def checkNoSlashesNotEmpty(s: String): String = {
    if (s.contains("/")) {
      throw new IllegalArgumentException("no slashes are allowed in " + s)
    } else if (s.trim.isEmpty) {
      throw new IllegalArgumentException("empty is not allowed")
    } else {
      s
    }
  }

  def work(workDirFile: File, out: PrintStream, err: PrintStream, rebaseFn: () ⇒ Unit, branch: String, sgit: Sgit,
           dependencyUpdates: Boolean, termOs: TermOs, shellWidth: Int, releaseToolGitSha1: String): Seq[Unit] = {
    checkLocalChanges(sgit, branch)
    rebaseFn.apply()
    Starter.addExitFn("cleanup branches", () ⇒ {
      sgit.checkout(sgit.currentBranch)
    })
    sgit.checkout(branch)
    Starter.chooseUpstreamIfUndef(out, sgit, branch)
    val mod = PomMod(workDirFile)

    if (dependencyUpdates) {
      val showUpdates = Term.readFromYes(out, "Show dependency updates?")
      if (showUpdates == "y") {
        mod.showDependecyUpdates(shellWidth, termOs, out)
      }
    }

    val newMod = offerAutoFixForReleaseSnapshots(out, mod, sgit.lsFiles(), shellWidth)
    if (newMod.hasNoShopPom) {
      out.println("---------")
      out.println("1. MAJOR version when you make incompatible API changes,")
      out.println("2. MINOR version when you add functionality in a backwards-compatible manner, and")
      out.println("3. PATCH version when you make backwards-compatible bug fixes.")
      out.println("   see also: http://semver.org/")
      out.println("---------")
    }

    val releaseRead = checkNoSlashesNotEmpty(Term.readChooseOneOfOrType(out, "Enter the release version", newMod.suggestReleaseVersion(sgit.branchNamesAll())))
    val release = if (mod.hasShopPom) {
      Term.removeSnapshot(releaseRead) + "-SNAPSHOT"
    } else {
      releaseRead
    }
    val releaseWitoutSnapshot = Term.removeSnapshot(release)
    if (mod.hasNoShopPom && sgit.listTags().contains("v" + releaseWitoutSnapshot)) {
      // TODO fetch remote and try to read new version again
      throw new IllegalStateException("release " + releaseWitoutSnapshot + " already found; check your repository or change version.")
    }
    val nextReleaseWithoutSnapshot = Term.readFrom(out, "Enter the next version without -SNAPSHOT", newMod.suggestNextRelease(release))

    if (mod.hasNoShopPom) {
      val coreVersions: Seq[(String, Dep)] = mod.listDependecies
        .filter(_.groupId.startsWith("com.novomind.ishop.core"))
        .map(in ⇒ (in.version, in))
        .distinct
        .filter(_._1.nonEmpty)
        .sortBy(_._1)
      val coreMajorVersions: Seq[(String, Dep)] = coreVersions
        .map(in ⇒ (in._1.replaceAll("\\..*", "").trim, in._2))
        .distinct
        .filter(_._1.nonEmpty)
        .sortBy(_._1)
      val releaseMajorVersion = release.replaceAll("\\..*", "")
      if (coreMajorVersions != Nil && coreMajorVersions.map(_._1).distinct.sorted != Seq(releaseMajorVersion)) {

        out.println("W: You are trying to release major version " + releaseMajorVersion + " (" + release +
          ") but this artifact refers to:")

        val t = coreMajorVersions
          .sortBy(a ⇒ a._2.version)
          .map(in ⇒ "  * " + Seq(in._2.groupId, in._2.artifactId, in._2.version).mkString(":"))
          .distinct

        Release.formatVersionLines(t).foreach(out.println)
        val continue = Term.readFromOneOfYesNo(out, "Continue?")
        if (continue == "n") {
          System.exit(1)
        }
      }
    }

    val nextSnapshot = nextReleaseWithoutSnapshot + "-SNAPSHOT"
    if (newMod.selfVersion != nextSnapshot) {
      newMod.changeVersion(nextSnapshot)
    }

    val releaseBrachName = "release/" + releaseWitoutSnapshot
    sgit.createBranch(releaseBrachName)
    if (newMod.selfVersion != nextSnapshot) {
      newMod.writeTo(workDirFile)
    }
    val toolSh1 = releaseToolGitSha1
    val headCommitId = sgit.commitIdHead()
    val releaseMod = PomMod(workDirFile)
    if (sgit.hasNoLocalChanges) {
      out.println("skipped release commit on " + branch)
    } else {
      out.print("Committing pom changes ..")
      sgit.doCommitPomXmlsAnd(
        """[ishop-release] prepare for next iteration - %s
          |
          |Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>
          |Releasetool-sign: %s
          |Releasetool-sha1: %s""".stripMargin.format(nextReleaseWithoutSnapshot, Starter.sign(), toolSh1), releaseMod.depTreeFilenameList())

      out.println(". done")
    }
    out.print("Checking out " + releaseBrachName + " ..")
    sgit.checkout(releaseBrachName)
    out.println(". done")

    if (releaseMod.selfVersion != release) {
      releaseMod.changeVersion(release)
      releaseMod.writeTo(workDirFile)
    }

    if (sgit.hasNoLocalChanges) {
      out.println("skipped release commit on " + releaseBrachName)
    } else {
      out.print("Commiting pom changes ..")
      sgit.doCommitPomXmlsAnd(
        """[ishop-release] perform to - %s
          |
          |Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>
          |Releasetool-sign: %s
          |Releasetool-sha1: %s""".stripMargin.format(release, Starter.sign(), toolSh1), releaseMod.depTreeFilenameList())
      out.println(". done")
    }
    if (releaseMod.hasNoShopPom) {
      sgit.doTag(release)
    }
    out.print("Checking out " + branch + " ..")
    sgit.checkout(branch)
    out.println(". done")
    if (newMod.hasNoShopPom) {
      sgit.deleteBranch(releaseBrachName)
    }
    out.println(sgit.graph())

    val sendToGerrit = Term.readFromOneOfYesNo(out, "Push to Gerrit?")
    val selectedBranch = sgit.findUpstreamBranch().getOrElse(branch)
    if (sendToGerrit == "y") {

      if (sgit.hasChangesToPush) {
        val result = sgit.pushFor(srcBranchName = branch, targetBranchName = selectedBranch)
        if (Starter.isInNovomindNetwork) {
          // TODO hier gerrit öffnen da man submit klicken muss
          // TODO wenn man genau den change öffnen könnte wär noch cooler
          Starter.openInDefaultBrowser("https://git-ishop.novomind.com:9091/#/q/status:open")
        }
        if (newMod.hasNoShopPom) {
          sgit.pushTag(release)
          if (Starter.isInNovomindNetwork) {
            val jenkinsBase = "https://build-ishop.novomind.com"
            val tagUrl = Starter.tagBuildUrl(sgit, jenkinsBase)
            // TODO hier erstmal nur den browser auf machen damit man build tag klicken kann
            Starter.openInDefaultBrowser(tagUrl.getOrElse(jenkinsBase + "/search/?q=-tag"))
            // try to notify jenkins about tag builds
            // TODO try to wait for successful tag builds ... subscribe to logs
          }
        }
      }
      if (newMod.hasShopPom) {
        val result = sgit.pushHeads(srcBranchName = "release/" + releaseWitoutSnapshot,
          targetBranchName = "release/" + releaseWitoutSnapshot)
        // TODO try to trigger job updates for jenkins
        // TODO try to trigger job execution in loop with abort
      }
      out.println("done.")

    } else {
      val pushTagOrBranch = if (newMod.hasNoShopPom) {
        "git push origin tag v" + releaseWitoutSnapshot + "; "
      } else {
        "git push -u origin release/" + releaseWitoutSnapshot + ":release/" + releaseWitoutSnapshot + ";"
      }
      val deleteTagOrBranch = if (newMod.hasNoShopPom) {
        "git tag -d v" + release + "; "
      } else {
        "git branch -D release/" + releaseWitoutSnapshot + ";"
      }
      out.println(
        ("""commands for local rollback:
           |  git reset --hard """ + headCommitId +
          """; """ + deleteTagOrBranch +
          """
            |
            |commands for sending to remote:
            |  git push origin """ + branch +
          """:refs/for/""" + selectedBranch +
          """; """ + pushTagOrBranch +
          """
            |NOTE: the "send to remote" command pushes the HEAD via Gerrit Code Review, this might not be needed for branches != master""").stripMargin)
    }
    Nil
  }

  def offerAutoFixForReleaseSnapshots(out: PrintStream, mod: PomMod, gitFiles: Seq[String], shellWidth: Int): PomMod = {
    val plugins = mod.listPluginDependecies
    if (mod.hasShopPom) {
      // TODO check if core needs this checks too
      PomChecker.check(plugins)
    }

    val snapsF = gitFiles.par
      .filterNot(in ⇒ in.endsWith(".list"))
      .filterNot(in ⇒ in.endsWith(".tree"))
      .filterNot(in ⇒ in.endsWith(".java"))
      .filterNot(in ⇒ in.endsWith(".png"))
      .filterNot(in ⇒ in.endsWith(".potx"))
      .filterNot(in ⇒ in.endsWith(".jpg"))
      .filterNot(in ⇒ in.matches(".*[/\\\\]src[/\\\\]test[/\\\\]resources[/\\\\]app\\-manifest.*"))
      .filterNot(in ⇒ in.endsWith("pom.xml"))
      .flatMap(findBadLines(Pattern.compile("-SNAPSHOT")))
      .seq
      .sortBy(_._3)

    if (snapsF != Nil) {
      println()
      println("Warning: Found SNAPSHOT occurrences in following files")
      snapsF.foreach(in ⇒ {
        println(in._3.toFile.getAbsolutePath + ":" + in._1)
        println("  " + in._2.trim())
      })
      println()
    }

    case class ReleaseInfo(gav: String, released: Boolean)

    val noShops: (Gav ⇒ Boolean) = if (mod.hasNoShopPom) {
      gav: Gav ⇒ gav.groupId.contains("com.novomind.ishop.shops")
    } else {
      _ ⇒ false
    }

    val boClientVersion = mod.listProperties
      .filter(_._1 == "bo-client")
      .filter(_._2.contains("-SNAPSHOT")).values

    val snaps: Seq[Gav] = mod.listSnapshotsDistinct
      .map(_.gav())
      .filterNot(noShops) ++ plugins.map(_.gav())
      .filter(_.version.contains("SNAPSHOT")) ++
      boClientVersion.map(in ⇒ Gav("com.novomind.ishop.backoffice", "bo-client", in, "war"))

    val aetherStateLine = StatusLine(snaps.size, shellWidth)
    val snapState: Seq[ReleaseInfo] = snaps
      .par
      .map(in ⇒ {
        aetherStateLine.start()
        val released = Aether.existsGav(in.groupId, in.artifactId, in.version.replace("-SNAPSHOT", ""))
        aetherStateLine.end()
        ReleaseInfo(in.formatted, released)
      }).seq
    aetherStateLine.finish()

    val snapshotProperties = mod.listProperties
      .filter(_._2.contains("-SNAPSHOT"))
      .filterNot(_._1 == "project.version")

    if (snapState.nonEmpty || snapshotProperties.nonEmpty) {
      if (snapshotProperties.nonEmpty) {
        out.println("")
        out.println("Snapshot properties found for (fix manually):")
        snapshotProperties.map(in ⇒ "Property: " + in).foreach(println)
      }
      if (snapState.nonEmpty) {
        out.println("")
        // TODO later autofix
        out.println("Snapshots found for (fix manually):")
      }

      def info(rel: Boolean): String = if (rel) {
        "Release found for "
      } else {
        "No Release for    "
      }

      snapState
        .sortBy(_.toString)
        .map(in ⇒ info(in.released) + in.gav)
        .foreach(println)
      out.println("")

      val again = Term.readFromOneOfYesNo(out, "Try again?")
      if (again == "n") {
        System.exit(1)
      } else {
        offerAutoFixForReleaseSnapshots(out, PomMod(mod.file), gitFiles, shellWidth)
      }
    }
    mod
  }

}
