package release

import java.io.{ByteArrayInputStream, File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.util.Map.Entry
import java.util.Properties
import java.util.concurrent.TimeUnit
import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.util.EntityUtils

import java.net.URI
import java.util.regex.{Matcher, Pattern}
import scala.io.Source

sealed class ReleaseConfig(map: Map[String, String]) {

  def workNexusUrl(envs: Map[String, String]): String = {
    ReleaseConfig.releaseNexusEnv(envs)
      .getOrElse(map(ReleaseConfig.keyNexusWorkUrl))
  }

  def mirrorNexusUrl(): String = {
    map(ReleaseConfig.keyNexusMirrorUrl)
  }

  def branchPrefix(): String = {
    map(ReleaseConfig.keyBranchPrefix)
  }

  def releasPrefix(): String = {
    map(ReleaseConfig.keyReleasePrefix)
  }

  def signedOfBy(): String = {
    map(ReleaseConfig.keyGerritSignedOfBy)
  }

  def gerritPort(): String = {
    map(ReleaseConfig.keyGerritPort)
  }

  def gerritHostname(): String = {
    map(ReleaseConfig.keyGerritHostname)
  }

  def gerritBaseUrl(): String = {
    map(ReleaseConfig.keyGerritUrl)
  }

  def jenkinsBaseUrl(): String = {
    map(ReleaseConfig.keyJenkinsBase)
  }

  def openJenkinsInBrowser: Boolean = {
    jenkinsBaseUrl() != ReleaseConfig.defaults(ReleaseConfig.keyJenkinsBase)
  }

  def openGerritInBrowser: Boolean = {
    gerritBaseUrl() != ReleaseConfig.defaults(ReleaseConfig.keyGerritUrl)
  }

  def gitBinEnv(): Option[String] = {
    Util.systemEnvs().get("RELEASE_GIT_BIN")
  }

  def getUserHome(shellHome: String): String = {
    val userHomeFromJava = System.getProperty("user.home")
    if (userHomeFromJava == shellHome) {
      userHomeFromJava
    } else {
      userHomeFromJava + " # shellHome: " + shellHome
    }

  }
}

object ReleaseConfig extends LazyLogging {
  trait Get[K, V] {
    def get(key: K): Option[V]
  }

  private val RELEASE_NEXUS_WORK_URL = "RELEASE_NEXUS_WORK_URL"

  def releaseNexusGet(get: Get[String, String]): Option[String] = {
    get.get(RELEASE_NEXUS_WORK_URL)
  }
  
  def releaseNexusEnv(envs: Map[String, String]): Option[String] = releaseNexusGet(k => envs.get(k))

  case class WorkAndMirror(workUrl: String, mirrorUrl: String)

  private val home = new File(System.getProperty("user.home"))
  private val defaultConfigFile: File = new File(home, ".ishop-release")
  private val defaultUpdateFile: File = new File(home, ".ishop-release-remote-update")

  private val keyJenkinsBase = "ishop-release.jenkins.base"
  private val keyGerritUrl = "ishop-release.gerrit.url"
  private val keyGerritHostname = "ishop-release.gerrit.hostname"
  private val keyGerritPort = "ishop-release.gerrit.port"
  private val keyGerritSignedOfBy = "ishop-release.gerrit.signedOfBy"
  private val keyReleasePrefix = "ishop-release.release.prefix"
  private val keyBranchPrefix = "ishop-release.branch.prefix"
  private val keyNexusWorkUrl = "ishop-release.nexus.work.url"
  private val keyNexusMirrorUrl = "ishop-release.nexus.mirror.url"

  private val defaults: Map[String, String] = Map(
    keyJenkinsBase -> "https://jenkins",
    keyGerritUrl -> "https://gerrit/",
    keyGerritHostname -> "gerrit",
    keyGerritPort -> "29418",
    keyGerritSignedOfBy -> "Ishop Release <release@example.org>",
    keyReleasePrefix -> "release",
    keyBranchPrefix -> "branch",
    keyNexusWorkUrl -> Repo.centralUrl, // "https://nexus-work/"
    keyNexusMirrorUrl -> Repo.centralUrl //"https://nexus-mirror/"
  )

  def parseConfig(str: String): Map[String, String] = {
    import scala.jdk.CollectionConverters._
    try {
      val properties = new Properties()
      properties.load(new ByteArrayInputStream(str.getBytes(StandardCharsets.ISO_8859_1)))
      val entries: Set[Entry[String, String]] = properties.entrySet().asScala.toSet.asInstanceOf[Set[Entry[String, String]]]
      entries.foldLeft(Map.empty[String, String])((a, b) => a ++ Map(b.getKey -> b.getValue))
    } catch {
      case e: Exception => {
        logger.warn("invalid config")
        Map.empty
      }
    }

  }

  private def writeConfig(file: File, map: Map[String, String]): Unit = {
    try {
      val properties = new Properties()
      map.foreach(t => properties.put(t._1, t._2))
      properties.store(new FileOutputStream(file), "")
    } catch {
      case e: Exception => {
        logger.warn("invalid config")
        Map.empty
      }
    }
  }

  private def fileConfig(file: File): Map[String, String] = {
    try {
      val configText = Source.fromFile(file).mkString
      parseConfig(configText)
    } catch {
      case e: Exception => {
        logger.warn("invalid config from file")
        Map.empty
      }
    }
  }

  private def remoteConfig(url: String): Map[String, String] = {
    val timeout = 1
    try {
      val config = RequestConfig.custom()
        .setConnectTimeout(timeout * 1000)
        .setConnectionRequestTimeout(timeout * 1000)
        .setSocketTimeout(timeout * 1000)
        .build()
      val client = HttpClientBuilder.create().setDefaultRequestConfig(config).build()
      val response = client.execute(new HttpGet(url))
      val remoteConfigText = EntityUtils.toString(response.getEntity)
      parseConfig(remoteConfigText)
    } catch {
      case e: Exception => {
        // TODO debug
        Map.empty
      }
    }
  }

  def extractWorkAndMirror(in: String): Option[WorkAndMirror] = {
    try {
      val doc = Xpath.newDocument(in)
      val servers = Xpath.toSeq(doc, "//server")
      val serverCredentials: Map[String, (Option[String], Option[String])] = servers.flatMap(n => {
        val id = Xpath.nodeElementValue(n, "id")
        if (id.isDefined) {
          val username = Xpath.nodeElementValue(n, "username")
          val password = Xpath.nodeElementValue(n, "password")
          Some((id.getOrElse(""), (username, password)))
        } else {
          None
        }
      }).toMap
      val value = Xpath.toSeq(doc, "//repository | //pluginRepository | //mirror")
      val distinct = value.flatMap(n => {
          val xUrl = Xpath.nodeElementValue(n, "url")
          val url = xUrl.getOrElse("")
          if (url.contains("0.0.0.0")) {
            None
          } else if (url.contains("//central")) {
            None
          } else {
            val id = Xpath.nodeElementValue(n, "id")
            if (id.isDefined) {
              val credentials = serverCredentials.get(id.get)

              val username = credentials.get._1.getOrElse("")
              val pw = credentials.get._2.getOrElse("")
              val userinfo = Seq(username, pw).filterNot(_.isBlank).mkString(":")
              Some(Util.setUserinfo(URI.create(url), userinfo).toString)
            } else {
              Some(s"$url")
            }
          }

        })
        .distinct
      val uEl: Seq[String] = distinct
      if (uEl.size == 1) {
        Some(WorkAndMirror(workUrl = uEl(0), mirrorUrl = uEl(0)))
      } else if (uEl.size == 2) {
        Some(WorkAndMirror(workUrl = uEl(1), mirrorUrl = uEl(0)))
      } else {
        None
      }
    } catch {
      case e: Exception => None
    }

  }

  def default(useDefaults: Boolean): ReleaseConfig = {
    if (useDefaults) {
      new ReleaseConfig(defaults)
    } else {
      val localConfig = if (defaultConfigFile.canRead) {
        fileConfig(defaultConfigFile)
      } else {
        Map.empty[String, String]
      }
      val refresh = defaultUpdateFile.canRead && {
        val millis = System.currentTimeMillis() - defaultUpdateFile.lastModified()
        val update = millis > TimeUnit.MINUTES.toMillis(1)
        update
      }
      val work = if (localConfig == Map.empty || refresh) {
        val removeConfigUrl = "https://release-ishop.novomind.com/ishop-release.conf"
        val rc = remoteConfig(removeConfigUrl)
        FileUtils.handleWindowsFilesystem { _ =>
          defaultUpdateFile.delete()
          defaultUpdateFile.createNewFile()
        }
        if (rc != Map.empty) {
          writeConfig(defaultConfigFile, rc)
          rc
        } else {
          localConfig
        }
      } else {
        localConfig
      }
      if (work == Map.empty) {
        fromSettings()
      } else {
        new ReleaseConfig(work)
      }
    }

  }

  def replaceInSettings(settings: String, envs: Map[String, String]): String = {
    envs.toSeq.foldLeft(settings)((str, key) => {
      str.replaceAll("\\$\\{env\\." + Pattern.quote(key._1) + "\\}", Matcher.quoteReplacement(key._2))
    })
  }

  def fromSettings(root: File = new File("."), envs: Map[String, String] = Util.systemEnvs()): ReleaseConfig = {
    val settings = new File(root, "settings.xml")
    if (settings.canRead) {
      val str = FileUtils.read(settings)
      val mir = ReleaseConfig.extractWorkAndMirror(replaceInSettings(str, envs))
      if (mir.isDefined) {
        new ReleaseConfig(defaults + (keyNexusWorkUrl -> mir.get.workUrl) + (keyNexusMirrorUrl -> mir.get.mirrorUrl))
      } else {
        new ReleaseConfig(defaults)
      }
    } else {
      new ReleaseConfig(defaults)
    }

  }

  def isJenkinsK(): Boolean = {
    scala.util.Properties.envOrNone("JENKINS_AGENT_WORKDIR").isDefined
  }

  def isDocker(): Boolean = {
    scala.util.Properties.envOrNone("RELEASE_DOCKER").isDefined
  }

  def isTravisCi(): Boolean = {
    scala.util.Properties.envOrNone("TRAVIS").exists(_.toBoolean)
  }

}
