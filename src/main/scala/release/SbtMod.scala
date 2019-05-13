package release

import java.io.File

import release.Starter.Opts

case class SbtMod(file: File, aether: Aether, opts: Opts) extends ProjectMod {
  val selfVersion: String = throw new UnsupportedOperationException()
  val listDependecies: Seq[ProjectMod.Dep] = throw new UnsupportedOperationException()
  val listPluginDependencies: Seq[ProjectMod.PluginDep] = throw new UnsupportedOperationException()
  val listProperties: Map[String, String] = throw new UnsupportedOperationException()

  def isShop: Boolean = throw new UnsupportedOperationException()

  def selfDepsMod: Seq[ProjectMod.Dep] = throw new UnsupportedOperationException()

  def suggestReleaseVersion(branchNames: Seq[String]): Seq[String] = throw new UnsupportedOperationException()

  def suggestNextRelease(releaseVersion: String): String = throw new UnsupportedOperationException()

  def listSnapshotsDistinct: Seq[ProjectMod.Dep] = throw new UnsupportedOperationException()

  def writeTo(targetFolder: File): Unit = throw new UnsupportedOperationException()

  def changeVersion(newVersion: String): Unit = throw new UnsupportedOperationException()
}
