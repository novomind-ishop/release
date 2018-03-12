package release

class ReleaseConfig() {

  def workNexusUrl(): String = {
    "http://nexus-ishop.novomind.com:8881/nexus/content/repositories/public"
  }

  def mirrorNexusUrl(): String = {
    "https://partner-nexus-ishop.novomind.com/content/groups/public/"
  }

  def branchPrefix(): String = {
    "ishop-branch"
  }

  def releasPrefix(): String = {
    "ishop-release"
  }

  def signedOfBy(): String = {
    "Ishop-Dev-Infra <ishop-dev-infra@novomind.com>"
  }

  def gerritPort(): String = "19418"

  def gerritHostname(): String = {
    "git-ishop.novomind.com"
  }

  def gerritBaseUrl(): String = {
    "https://git-ishop.novomind.com:9091/"
  }

  def isInNovomindNetwork: Boolean = {
    System.getenv("USERDNSDOMAIN") == "NOVOMIND.COM"
  }

  def jenkinsBaseUrl(): String = {
    "https://build-ishop.novomind.com"
  }
}

object ReleaseConfig {

  def default(): ReleaseConfig = new ReleaseConfig()

}
