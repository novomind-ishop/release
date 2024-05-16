package release

import com.google.common.collect.ImmutableList
import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.HttpResponseException
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.HttpClients
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.metadata.DefaultMetadata
import org.eclipse.aether.metadata.Metadata.Nature
import org.eclipse.aether.repository.{LocalRepository, RemoteRepository}
import org.eclipse.aether.resolution.{ArtifactRequest, MetadataRequest, MetadataResult, VersionRangeRequest, VersionRangeResolutionException}
import org.eclipse.aether.supplier.RepositorySystemSupplier
import org.eclipse.aether.transfer._
import org.eclipse.aether.util.repository.AuthenticationBuilder
import org.eclipse.aether.version.Version
import org.eclipse.aether.{DefaultRepositorySystemSession, RepositorySystem}
import release.ProjectMod.Gav3

import java.io.{File, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle, TextStyle}
import java.time.temporal.ChronoField
import java.time.temporal.ChronoField._
import java.time.{ZoneOffset, ZonedDateTime}
import java.util
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

trait RepoZ {
  def createAll(allUrls: Seq[String]): Seq[RepoZ] = {
    allUrls.distinct.map(Repo.ofUrl) // TODO cache?
  }

  def tryResolveReqWorkNexus(request: String): Try[(File, String)]

  def getRelocationOf(groupID: String, artifactId: String, version: String): Option[Gav3]

  def newerAndPrevVersionsOf(groupID: String, artifactId: String, version: String): Seq[String]

  def depDate(groupId: String, artifactId: String, version: String): Option[ZonedDateTime]

  def workNexusUrl(): String

  def allRepoUrls(): Seq[String]

  def isReachable(showTrace: Boolean = true): Repo.ReachableResult

  def existsGav(groupID: String, artifactId: String, version: String): Boolean

  def latestGav(groupID: String, artifactId: String, version: String): Option[Gav3]
}

class RepoProxy(_repos: Seq[RepoZ]) extends RepoZ {
  val repos = {
    val t = _repos.filter(r => {
      Util.ipFromUrl(r.workNexusUrl()).isDefined
    })
    if (t.isEmpty) {
      _repos
    } else {
      t
    }
  }

  override def allRepoUrls(): Seq[String] = {
    repos.flatMap(_.allRepoUrls())
  }

  override def getRelocationOf(groupID: String, artifactId: String, version: String): Option[Gav3] = {
    repos.flatMap(_.getRelocationOf(groupID, artifactId, version)).headOption
  }

  override def newerAndPrevVersionsOf(groupID: String, artifactId: String, version: String): Seq[String] = {
    repos.flatMap(_.newerAndPrevVersionsOf(groupID, artifactId, version)).distinct
  }

  override def depDate(groupId: String, artifactId: String, version: String): Option[ZonedDateTime] = {
    repos.flatMap(_.depDate(groupId, artifactId, version)).headOption
  }

  override def workNexusUrl(): String = {
    s"RepoProxy: ${repos.map(_.workNexusUrl()).mkString(", ")}"
  }

  override def isReachable(showTrace: Boolean): Repo.ReachableResult = {
    Util.only(repos.map(_.isReachable(showTrace)).distinct, "mutliple results")
  }

  override def existsGav(groupID: String, artifactId: String, version: String): Boolean = {
    ???
  }

  override def latestGav(groupID: String, artifactId: String, version: String): Option[Gav3] = {
    ???
  }

  override def tryResolveReqWorkNexus(request: String): Try[(File, String)] = {
    repos.map(_.tryResolveReqWorkNexus(request)).headOption.getOrElse(Failure(new IllegalStateException("nothing found")))
  }

}

class Repo(_mirrorNexus: RemoteRepository, _workNexus: RemoteRepository) extends RepoZ with LazyLogging {

  private val mirrorNexus: RemoteRepository = _mirrorNexus

  private val workNexus: RemoteRepository = _workNexus

  def workNexusUrl(): String = workNexus.getUrl

  private val allRepos: Seq[RemoteRepository] = Seq(workNexus, mirrorNexus)

  override def allRepoUrls(): Seq[String] = allRepos.map(_.getUrl)

  private def getVersionsOf(req: String) = Repo.getVersions(workNexus)(req)

  def depDate(groupId: String, artifactId: String, version: String): Option[ZonedDateTime] =
    Repo.depDate(workNexus)(groupId, artifactId, version)

  def isReachable(showTrace: Boolean = true): Repo.ReachableResult = {
    val config = RequestConfig.custom()
      .setConnectTimeout(1000)
      .setConnectionRequestTimeout(1000)
      .setSocketTimeout(1000)
      .build();
    val httpclient = HttpClients.custom()
      .setDefaultRequestConfig(config)
      .build()
    var response: CloseableHttpResponse = null
    val code: Int = try {
      val httpGet = new HttpGet(workNexusUrl())
      response = httpclient.execute(httpGet)
      response.getStatusLine.getStatusCode
    } catch {
      case any: Throwable => {
        if (showTrace) {
          any.printStackTrace()
        }
        return Repo.ReachableResult(online = false, any.getClass.getCanonicalName +
          ": " + any.getMessage + " requestConfig: " + config.toString)
      }
    } finally {
      if (response != null) {
        response.close()
      }

    }
    Repo.ReachableResult(code != 0, code.toString)

  }

  def existsGav(groupID: String, artifactId: String, version: String): Boolean = {
    val versions = getVersionsOf(Seq(groupID, artifactId, "[" + version + "," + version + "]").mkString(":"))
    versions.nonEmpty
  }

  def latestGav(groupID: String, artifactId: String, version: String): Option[Gav3] = {
    val in = release.Version.parseSloppy(version)
    val start = if (in.isMajor()) {
      in.major - 1
    } else {
      in.major
    }
    val versions = getVersionsOf(Seq(groupID, artifactId, "[" + start + "," + version + "]").mkString(":"))
    versions
      .filterNot(_.toString.endsWith(release.Version.snapshot))
      .lastOption.map(v => Gav3(groupID, artifactId, Option(v.toString)))
  }

  def newerAndPrevVersionsOf(groupID: String, artifactId: String, version: String): Seq[String] = {
    Repo.convertNewerAndPrefVersions(groupID, artifactId, version, request => getVersionsOf(request).map(_.toString))
  }

  def getRelocationOf(groupID: String, artifactId: String, version: String): Option[Gav3] = {
    // TODO https://maven.apache.org/guides/mini/guide-relocation.html
    None
  }

  override def tryResolveReqWorkNexus(request: String): Try[(File, String)] = Repo.tryResolveReqWorkNexus(this)(request)

}

object Repo extends LazyLogging {

  private val fmtbNexus: DateTimeFormatter = {
    val dow: util.Map[Long, String] = new util.HashMap[Long, String]
    dow.put(1L, "Mon")
    dow.put(2L, "Tue")
    dow.put(3L, "Wed")
    dow.put(4L, "Thu")
    dow.put(5L, "Fri")
    dow.put(6L, "Sat")
    dow.put(7L, "Sun")
    val moy: util.Map[Long, String] = new util.HashMap[Long, String]
    moy.put(1L, "Jan")
    moy.put(2L, "Feb")
    moy.put(3L, "Mar")
    moy.put(4L, "Apr")
    moy.put(5L, "May")
    moy.put(6L, "Jun")
    moy.put(7L, "Jul")
    moy.put(8L, "Aug")
    moy.put(9L, "Sep")
    moy.put(10L, "Oct")
    moy.put(11L, "Nov")
    moy.put(12L, "Dec")
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive
      .parseLenient

      .optionalStart
      .appendText(DAY_OF_WEEK) // TODO dow
      .appendLiteral(" ")
      .optionalEnd

      .appendText(ChronoField.MONTH_OF_YEAR, TextStyle.SHORT) // TODO moy
      .appendLiteral(' ')
      .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
      .appendLiteral(' ')
      .appendValue(HOUR_OF_DAY, 2)
      .appendLiteral(':')
      .appendValue(MINUTE_OF_HOUR, 2)
      .appendLiteral(':')
      .appendValue(SECOND_OF_MINUTE, 2)
      .appendLiteral(' ')
      .appendZoneText(TextStyle.FULL)
      .appendLiteral(' ')
      .appendValue(YEAR, 4)
      .toFormatter
  }

  private val fmtbCentral: DateTimeFormatter = new DateTimeFormatterBuilder()
    .parseCaseInsensitive
    .parseLenient
    .appendValue(YEAR, 4)
    .appendLiteral('-')
    .appendValue(ChronoField.MONTH_OF_YEAR)
    .appendLiteral('-')
    .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
    .appendLiteral(' ')
    .appendValue(HOUR_OF_DAY, 2)
    .appendLiteral(':')
    .appendValue(MINUTE_OF_HOUR, 2)

    .optionalStart
    .appendLiteral(":")
    .appendText(SECOND_OF_MINUTE)
    .optionalEnd

    .appendLiteral(' ')
    .appendZoneText(TextStyle.FULL)

    .toFormatter

  def convertNewerAndPrefVersions(groupID: String, artifactId: String, version: String,
                                  function: String => Seq[String]): Seq[String] = {
    val innerV = release.Version.parseSloppy(version)
    val request = Seq(groupID, artifactId, "[" + (innerV.major - 1) + ",)").mkString(":")
    val result = function.apply(request).map(release.Version.parseSloppy).sorted

    val dropped = result.dropWhile(v => {
      v.primarys != innerV.primarys
    })
    val option = result.filterNot(dropped.contains).lastOption.map(_.primarys)
    val value = (result.filter(pv => pv.primarysOpt == option) ++ dropped).sorted
    value.map(_.rawInput)
  }

  def ofUrl(url: String): Repo = {
    val repositorySystem: RemoteRepository = Repo.newDefaultRepository(url)
    new Repo(_mirrorNexus = repositorySystem, _workNexus = repositorySystem)
  }

  case class ReachableResult(online: Boolean, msg: String)

  logger.debug("init aether to suppress replayed slf4j logging - See also http://www.slf4j.org/codes.html#replay")

  val centralUrl = "https://repo1.maven.org/maven2/"

  def newDefaultRepository(url: String, username: Option[String] = None, password: Option[String] = None): RemoteRepository = {
    val builder = new RemoteRepository.Builder("central", "default", url)
    if (username.isDefined && password.isDefined) {
      builder
        .setAuthentication(new AuthenticationBuilder()
          .addUsername(username.get)
          .addPassword(password.get)
          .build())
        .build
    } else {
      builder.build
    }
  }

  @throws[VersionRangeResolutionException]
  private def getVersions(repository: RemoteRepository)(request: String): Seq[Version] = {
    val system = ArtifactRepos.newRepositorySystem
    val session = ArtifactRepos.newRepositorySystemSession(system)
    val artifact = new DefaultArtifact(request)
    val rangeRequest = new VersionRangeRequest
    rangeRequest.setArtifact(artifact)
    rangeRequest.setRepositories(Util.toJavaList(Seq(repository)))
    val rangeResult = system.resolveVersionRange(session, rangeRequest)
    rangeResult.getVersions.asScala.toList
  }

  def parseArtifactoryDate(line: String): Option[ZonedDateTime] = {

    val fmtbArtifactory: DateTimeFormatter = new DateTimeFormatterBuilder()
      .parseCaseInsensitive
      .parseLenient
      .appendValue(DAY_OF_MONTH, 1, 2, SignStyle.NOT_NEGATIVE)
      .appendLiteral('-')
      .appendText(ChronoField.MONTH_OF_YEAR, TextStyle.SHORT)
      .appendLiteral('-')
      .appendValue(YEAR, 4)
      .appendLiteral(' ')
      .appendValue(HOUR_OF_DAY, 2)
      .appendLiteral(':')
      .appendValue(MINUTE_OF_HOUR, 2)

      .appendLiteral(' ')
      .appendZoneText(TextStyle.FULL)

      .toFormatter

    if (line.matches(".*[0-9]{2}-[A-Za-z]+-[1-9][0-9]{3}.*")) {
      try {
        // 15-May-2024 11:59 Z
        val spl = line.replaceAll("[ ]+", " ")
          .replaceAll("^[^ ]+", "")
          .split(" ")
        val input = spl(1) + " " + spl(2) + " Z"
        Some(ZonedDateTime.parse(input, fmtbArtifactory).withZoneSameInstant(ZoneOffset.UTC))
      } catch {
        case e: Exception => None
      }
    } else {
      None
    }

  }

  def parseCentralDate(line: String): Option[ZonedDateTime] = {
    if (line.matches(".*[0-9]{4}-[0-9]{2}-[0-9]{2}.*")) {
      try {
        val spl = line.replaceAll("[ ]+", " ").split(" ")
        val input = spl(1) + " " + spl(2) + " Z"
        Some(ZonedDateTime.parse(input, fmtbCentral).withZoneSameInstant(ZoneOffset.UTC))
      } catch {
        case e: Exception => None
      }
    } else {
      None
    }
  }

  def parseNexusDate(input: String): Option[ZonedDateTime] = {
    try {
      Some(ZonedDateTime.parse(input, fmtbNexus).withZoneSameInstant(ZoneOffset.UTC))
    } catch {
      case e: Exception => None
    }
  }

  def httpCause(mr: MetadataResult): Option[HttpResponseException] = {
    try {
      Some(mr.getException.getCause.asInstanceOf[HttpResponseException])
    } catch {
      case e: Exception => None
    }
  }

  private[release] def depDate(repository: RemoteRepository)(groupId: String, artifactId: String, version: String, retry: Boolean = true): Option[ZonedDateTime] = {
    val system = ArtifactRepos.newRepositorySystem
    val session = ArtifactRepos.newRepositorySystemSession(system, silent = false)
    val req = new MetadataRequest()
    req.setMetadata(new DefaultMetadata(groupId, artifactId, version, "", Nature.RELEASE_OR_SNAPSHOT))
    req.setRepository(repository)

    val owet = system.resolveMetadata(session, ImmutableList.of(req))
    owet.asScala.flatMap(e => {
      if (!e.isResolved) {
        val httpResponse = httpCause(e)
        if (httpResponse.isDefined && httpResponse.get.getStatusCode == 401) {
          Nil
        } else if (retry) {
          tryResolveReq(repository)(groupId + ":" + artifactId + ":pom:" + version)
          depDate(repository)(groupId, artifactId, version, retry = false)
        } else {
          Nil
        }
      } else {
        val file = e.getMetadata.getFile
        Util.readLines(file)
          .flatMap(extractDate)
          .distinct
      }
    }).toSeq.minOption
  }

  def extractDate(line: String): Option[ZonedDateTime] = {
    val newline = line.replaceAll("\\<.*?\\>", "").trim

    if (newline.isBlank) {
      None
    } else {
      if (newline.matches(".*[0-9].*") && newline.matches(".*[a-zA-Z].*")) {
        parseNexusDate(newline)
          .orElse(parseArtifactoryDate(newline))
          .orElse(parseCentralDate(newline))
      } else {
        None
      }
    }
  }

  type VersionString = String

  def tryResolveReqWorkNexus(repo: Repo)(request: String): Try[(File, VersionString)] = {
    tryResolveReq(repo.workNexus)(request)
  }

  def tryResolveReq(repository: RemoteRepository)(request: String): Try[(File, VersionString)] = {
    try {
      Success(doResolve(repository)(request))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  def resolve(repository: RemoteRepository)(groupID: String, artifactId: String, version: String): (File, VersionString) = {
    val request = Seq(groupID, artifactId, version).mkString(":")
    doResolve(repository)(request)
  }

  def doResolve(repository: RemoteRepository)(request: String): (File, VersionString) = {
    // expected format is <groupId>:<artifactId>[:<extension>[:<classifier>]]:<version>
    val system = ArtifactRepos.newRepositorySystem
    val session = ArtifactRepos.newRepositorySystemSession(system, silent = false)
    val artifact = new DefaultArtifact(request)
    val artifactRequest = new ArtifactRequest
    artifactRequest.setArtifact(artifact)
    artifactRequest.setRepositories(Util.toJavaList(Seq(repository)))
    val result = system.resolveArtifact(session, artifactRequest)

    (result.getArtifact.getFile, result.getArtifact.getVersion)
  }

  private object ManualRepositorySystemFactory {
    def newRepositorySystem: RepositorySystem = {
      new RepositorySystemSupplier().get()
    }
  }

  private class ArtifactRepos {}

  private object ArtifactRepos {
    def newRepositorySystem: RepositorySystem = ManualRepositorySystemFactory.newRepositorySystem

    def newRepositorySystemSession(system: RepositorySystem): DefaultRepositorySystemSession =
      newRepositorySystemSession(system, silent = true)

    private def delete(path: Path): Unit = {
      try
        Files.walkFileTree(path, new FileVisitor[Path]() {
          @throws[IOException]
          def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          }

          @throws[IOException]
          def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = FileVisitResult.CONTINUE

          @throws[IOException]
          def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.delete(file)
            FileVisitResult.CONTINUE
          }

          @throws[IOException]
          def visitFileFailed(file: Path, exc: IOException): FileVisitResult = {
            if (!exc.toString.contains("local-repo")) {
              System.out.println(classOf[ArtifactRepos].getCanonicalName + " -> " + exc.toString)
            }
            FileVisitResult.CONTINUE
          }
        })

      catch {
        case e: IOException => {
          e.printStackTrace()
        }
      }
    }

    def newRepositorySystemSession(system: RepositorySystem, silent: Boolean): DefaultRepositorySystemSession = {
      val session = MavenRepositorySystemUtils.newSession
      val localFile = {
        val systemFile = new File("/tmp/ishop-release/target/local-repo/")
        val repo = new File("target/local-repo/")
        if (repo.isDirectory) {
          repo
        } else if (repo.mkdirs()) {
          repo.delete()
          repo
        } else {
          systemFile
        }
      }

      val localFileRandom = new File(localFile, Util.hashMd5Random())
      val localRepo = new LocalRepository(localFileRandom)
      localRepo.getBasedir.getAbsoluteFile.getParentFile.mkdir()
      localRepo.getBasedir.getAbsoluteFile.mkdir()
      delete(localRepo.getBasedir.getAbsoluteFile.toPath)
      session.setLocalRepositoryManager(system.newLocalRepositoryManager(session, localRepo))
      if (!silent) {
        session.setTransferListener(new TraceTransferListener)
      }
      session
    }

  }

  private class TraceTransferListener extends AbstractTransferListener with LazyLogging {
    override def transferInitiated(event: TransferEvent): Unit = {
      val message = if (event.getRequestType eq TransferEvent.RequestType.PUT) {
        "Uploading"
      } else {
        "Downloading"
      }
      logger.trace(message + ": " + event.getResource.getRepositoryUrl + event.getResource.getResourceName)
    }
  }

}

