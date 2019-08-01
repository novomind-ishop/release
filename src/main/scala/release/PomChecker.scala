package release

import release.ProjectMod.PluginDep

object PomChecker {

  private val path = Seq("plugin", "plugins", "build", "project")

  def check(plugins: Seq[PluginDep]): Unit = {
    checkIshopMaven(plugins)
    val depPlugins = PomMod.dependecyPlugins(plugins)
    depPlugins.foreach(plugin => {
      if (plugin.execs == Nil) {
        throw new ValidationException("please add at least one execution to you maven-dependency-plugin")
      } else {
        val treeOrLists = plugin.execs.filter(in => in.goals.contains("tree") || in.goals.contains("list"))
        val invalidPhase = treeOrLists.filter(_.phase != "validate")
        if (invalidPhase != Nil) {
          throw new ValidationException("maven-dependency-plugin goals " +
            invalidPhase.flatMap(_.goals).mkString(", ") + " must be executed on phase \"validate\"")
        } else {
          val defectconfig = treeOrLists.filter(in => {
            val outputFileValue = in.config.get("outputFile")
            outputFileValue.isEmpty || outputFileValue.get.contains("/")
          })
          if (defectconfig != Nil) {
            throw new ValidationException("Please check your pom.xml's. The maven-dependency-plugin execution with id " +
              defectconfig.map(_.id).mkString(", ") + " has no configuration-element or the outputFile-tag contains slashes")
          }
          checkDefaultPath(plugin)
        }
      }
    })
  }

  private[release] def checkIshopMaven(_plugins: Seq[PluginDep]): Unit = {
    val plugins = PomMod.findPluginsByName(_plugins, "ishop-maven-plugin")
    // TODO ishop maven plugin muss definiert sein
    plugins.foreach(ishopMaven => {
      val execs = Util.only(ishopMaven.execs, ishopMaven.pomRef.id + " - a single execution section for ishop maven plugin please required")
      if (!execs.goals.contains("check-for-changes-before")) {
        throw new ValidationException("please add \"check-for-changes-before\" to your ishop maven plugin")
      }
      if (!execs.goals.contains("check-for-changes-package")) {
        throw new ValidationException("please add \"check-for-changes-package\" to your ishop maven plugin")
      }

      checkDefaultPath(ishopMaven)
    })
  }

  private def checkDefaultPath(plugin: PluginDep): Unit = {
    if (plugin.pomPath != path) {
      throw new ValidationException("please check your pom.xml's and move your " + plugin.artifactId + " to " +
        path.reverse.mkString("/") + " your path is " + plugin.pomPath.reverse.mkString("/"))
    }
  }

  class ValidationException(msg: String) extends RuntimeException(msg)

}
