package release

import release.Util.toScalaMapNonBlank

object Envs {
  def systemEnvs(): Map[String, String] = Util.toScalaMapNonBlank(System.getenv())
}
