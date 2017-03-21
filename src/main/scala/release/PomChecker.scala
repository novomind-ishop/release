package release

import release.PomMod.PluginDep

object PomChecker {

  def check(plugins: Seq[PluginDep]): Unit = {
    val ishopMavenOpt = plugins
      .filter(_.artifactId == "ishop-maven-plugin")
      .filterNot(_.pomPath.contains("pluginManagement"))
    if (ishopMavenOpt.nonEmpty) {
      ishopMavenOpt.foreach(ishopMaven ⇒ {
        val execs = Util.only(ishopMaven.execs, "A single execution section for ishop maven plugin please required, but was")
        if (!execs.goals.contains("check-for-changes-before")) {
          throw new ValidationException("please add \"check-for-changes-before\" to your ishop maven plugin")
        }
        if (!execs.goals.contains("check-for-changes-package")) {
          throw new ValidationException("please add \"check-for-changes-package\" to your ishop maven plugin")
        }
        val path = Seq("plugin", "plugins", "build", "project")
        if (ishopMaven.pomPath != path) {
          throw new ValidationException("please move your ishop-maven-plugin to " +
            path.mkString("/") + " your path is " + ishopMaven.pomPath.mkString("/"))
        }
      })
    }
  }

  class ValidationException(msg: String) extends RuntimeException(msg)

}
