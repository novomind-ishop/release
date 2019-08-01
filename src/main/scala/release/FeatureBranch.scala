package release

import java.io.{File, PrintStream}

import release.Starter.Opts

import scala.annotation.tailrec

object FeatureBranch {
  def work(workDirFile: File, out: PrintStream, err: PrintStream, sgit: Sgit, branch: String, rebaseFn: () => Unit,
           toolSh1: String, config: ReleaseConfig, opts: Opts): Unit = {
    Release.checkLocalChanges(sgit, branch)
    rebaseFn.apply()
    sgit.checkout(branch)

    Starter.chooseUpstreamIfUndef(out, sgit, branch, opts, Console.in)

    val featureName = PomMod.checkNoSlashesNotEmptyNoZeros(Term.readFrom(out, "Enter the feature name", "", opts, Console.in))
    val featureWitoutSnapshot = featureName.replaceFirst("-SNAPSHOT$", "")
    val featureSnapshot = featureWitoutSnapshot + "-SNAPSHOT"

    @tailrec
    def checkFeatureBranch(): Unit = {
      if (sgit.listBranchNamesLocal().contains("feature")) {
        val changes = Term.readFromOneOfYesNo(out, "You have a local branch with name 'feature'. " +
          "We use this name for branch creation. Delete this branch manually. Abort release?", opts)
        if (changes == "y") {
          System.exit(1)
        } else {
          checkFeatureBranch()
        }
      }
    }
    checkFeatureBranch()
    val featureBranchName = "feature/" + featureWitoutSnapshot
    sgit.createBranch(featureBranchName)
    sgit.checkout(featureBranchName)

    val mod = PomMod.of(workDirFile, err, opts)
    mod.changeVersion(featureSnapshot)
    mod.writeTo(workDirFile)
    out.print("Committing pom changes ..")
    val msgs = ""
    sgit.doCommitPomXmlsAnd(
      """[%s] prepare - %s
        |%s
        |Signed-off-by: %s
        |Releasetool-sign: %s
        |Releasetool-sha1: %s""".stripMargin.format(config.branchPrefix(), featureWitoutSnapshot,
        msgs,
        config.signedOfBy(), Starter.sign(), toolSh1), mod.depTreeFilenameList())

    out.println(". done")
    out.println("To push this branch to remote: $ git push --set-upstream origin " + featureBranchName)
  }

}
