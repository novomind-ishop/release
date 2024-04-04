package release

import java.io.{File, InputStream, PrintStream}
import release.Starter.Opts

import scala.annotation.tailrec

object FeatureBranch {
  def work(workDirFile: File, sys: Term.Sys, sgit: Sgit, branch: String, rebaseFn: () => Unit,
           toolSh1: String, config: ReleaseConfig, opts: Opts): Unit = {
    Release.checkLocalChanges(sgit, branch)
    rebaseFn.apply()
    sgit.checkout(branch)

    Starter.chooseUpstreamIfUndef(sys, sgit, branch, opts)
    sys.out.println("Featurebranches are NOT recommended if you don't like merge conflicts.")
    val featureName = PomMod.checkNoSlashesNotEmptyNoZeros(Term.readFrom(sys, "Enter the feature name", "", opts))
    val featureWitoutSnapshot = Version.removeTrailingSnapshots(featureName)
    val featureSnapshot = Version.applySnapshot(featureWitoutSnapshot)

    @tailrec
    def checkFeatureBranch(sys: Term.Sys): Unit = {
      if (sgit.listBranchNamesLocal().contains("feature")) {
        val changes = Term.readFromOneOfYesNo(sys, "You have a local branch with name 'feature'. " +
          "We use this name for branch creation. Delete this branch manually. Abort release?", opts)
        if (changes == "y") {
          System.exit(1)
        } else {
          checkFeatureBranch(sys)
        }
      }
    }

    checkFeatureBranch(sys)
    val featureBranchName = "feature/" + featureWitoutSnapshot
    sgit.createBranch(featureBranchName)
    sgit.checkout(featureBranchName)

    val mod = PomMod.of(workDirFile, opts, failureCollector = None)
    mod.changeVersion(featureSnapshot)
    mod.writeTo(workDirFile)
    sys.out.print("Committing pom changes ..")
    val msgs = ""
    sgit.doCommitPomXmlsAnd(
      """[%s] prepare - %s
        |%s
        |Signed-off-by: %s
        |Releasetool-sign: %s
        |Releasetool-sha1: %s""".stripMargin.format(config.branchPrefix(), featureWitoutSnapshot,
        msgs,
        config.signedOfBy(), Starter.sign(sgit), toolSh1), mod.depTreeFilenameList())

    sys.out.println(". done (h)")
    sys.out.println("To push this branch to remote: $ git push --set-upstream origin " + featureBranchName)
  }

}
