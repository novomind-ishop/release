package release

import java.io.{File, PrintStream}

object FeatureBranch {
  def work(workDirFile: File, out: PrintStream, err: PrintStream, sgit: Sgit, branch: String, rebaseFn: () ⇒ Unit, toolSh1: String): Unit = {
    Release.checkLocalChanges(sgit, branch)
    rebaseFn.apply()
    sgit.checkout(branch)

    Starter.chooseUpstreamIfUndef(out, sgit, branch)

    val featureName = PomMod.checkNoSlashesNotEmptyNoZeros(Term.readFrom(out, "Enter the feature name", ""))
    val featureWitoutSnapshot = featureName.replaceFirst("-SNAPSHOT$", "")
    val featureSnapshot = featureWitoutSnapshot + "-SNAPSHOT"

    val featureBranchName = "feature/" + featureWitoutSnapshot
    sgit.createBranch(featureBranchName)
    sgit.checkout(featureBranchName)

    val mod = PomMod(workDirFile)
    mod.changeVersion(featureSnapshot)
    mod.writeTo(workDirFile)
    out.print("Committing pom changes ..")
    sgit.doCommitPomXmlsAnd(
      """[ishop-branch] prepare - %s
        |
        |Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>
        |Releasetool-sign: %s
        |Releasetool-sha1: %s""".stripMargin.format(featureWitoutSnapshot, Starter.sign(), toolSh1), mod.depTreeFilenameList())

    out.println(". done")
  }

}
