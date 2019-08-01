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

import scala.io.Source

sealed class ReleaseConfig(map: Map[String, String]) {

  def workNexusUrl(): String = {
    map(ReleaseConfig.keyNexusWorkUrl)
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
    scala.util.Properties.envOrNone("RELEASE_GIT_BIN")
  }

  def getUserNome(): String = {
    System.getProperty("user.home")
  }
}

object ReleaseConfig extends LazyLogging {

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
    keyNexusWorkUrl -> Aether.centralUrl, // "https://nexus-work/"
    keyNexusMirrorUrl -> Aether.centralUrl //"https://nexus-mirror/"
  )

  def parseConfig(str: String): Map[String, String] = {
    try {
      val properties = new Properties()
      properties.load(new ByteArrayInputStream(str.getBytes(StandardCharsets.ISO_8859_1)))
      val entries: Set[Entry[String, String]] = Util.toSet(properties.entrySet()).asInstanceOf[Set[Entry[String, String]]]
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

  def default(useDefaults: Boolean): ReleaseConfig = {
    if (useDefaults) {
      new ReleaseConfig(defaults)
    } else {
      val removeConfigUrl = "https://release-ishop.novomind.com/ishop-release.conf"
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
        val rc = remoteConfig(removeConfigUrl)
        Util.handleWindowsFilesystem { _ =>
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
        new ReleaseConfig(defaults)
      } else {
        new ReleaseConfig(work)
      }
    }

  }

  def isTravisCi(): Boolean = {
    scala.util.Properties.envOrNone("TRAVIS").exists(_.toBoolean)
  }

}
