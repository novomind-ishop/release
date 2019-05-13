package release

import java.io.{File, PrintStream}

import release.PomMod.{Dep, PluginDep}
import release.Starter.{OptsDepUp, TermOs}

trait ProjectMod {
  val file: File
  val selfVersion: String

  val listDependecies: Seq[Dep]
  val listPluginDependencies: Seq[PluginDep]

  val listProperties: Map[String, String]

  def showDependencyUpdates(shellWidth: Int, termOs: TermOs, depUpOpts: OptsDepUp, workNexusUrl: String,
                            out: PrintStream, err: PrintStream): Unit

  def hasNoShopPom: Boolean = {
    !hasShopPom
  }

  def hasShopPom: Boolean

  def selfDepsMod: Seq[Dep]

  def suggestReleaseVersion(branchNames: Seq[String] = Nil): Seq[String]

  def suggestNextRelease(releaseVersion: String): String

  def listSnapshotsDistinct: Seq[Dep]

  def writeTo(targetFolder: File): Unit

  def changeVersion(newVersion: String): Unit
}
