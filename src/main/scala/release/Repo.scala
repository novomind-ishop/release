package release

import com.google.common.collect.ImmutableList
import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.HttpClients
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.metadata.DefaultMetadata
import org.eclipse.aether.metadata.Metadata.Nature
import org.eclipse.aether.repository.{LocalRepository, RemoteRepository}
import org.eclipse.aether.resolution.{ArtifactRequest, MetadataRequest, VersionRangeRequest, VersionRangeResolutionException}
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transfer._
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.eclipse.aether.version.Version
import org.eclipse.aether.{DefaultRepositorySystemSession, RepositorySystem}
import release.ProjectMod.Gav3
import release.Starter.Opts

import java.io.{File, IOException, PrintStream}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.time.format.{DateTimeFormatterBuilder, SignStyle, TextStyle}
import java.time.temporal.ChronoField
import java.time.temporal.ChronoField._
import java.time.{ZoneOffset, ZonedDateTime}
import java.util
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

class Repo private(opts: Opts) extends LazyLogging {

  private lazy val newRepositoriesCentral: RemoteRepository = Repo.newDefaultRepository(Repo.centralUrl)

  private lazy val mirrorNexus: RemoteRepository = Repo.newDefaultRepository(ReleaseConfig.default(opts.useDefaults).mirrorNexusUrl())

  private lazy val workNexus: RemoteRepository = Repo.newDefaultRepository(ReleaseConfig.default(opts.useDefaults).workNexusUrl())

  def workNexusUrl(): String = workNexus.getUrl

  private lazy val allRepos: Seq[RemoteRepository] = Seq(workNexus, mirrorNexus)

  private def getVersionsOf(req: String) = Repo.getVersions(workNexus)(req)

  def depDate(groupId: String, artifactId: String, version: String) =
    Repo.depDate(workNexus)(groupId, artifactId, version)

  private[release] def isReachable(showTrace: Boolean = true): Repo.ReachableResult = {
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
        return Repo.ReachableResult(false, any.getClass.getCanonicalName +
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

  def newerVersionsOf(groupID: String, artifactId: String, version: String): Seq[String] = {
    val request = Seq(groupID, artifactId, "[" + version + ",)").mkString(":")
    val result = getVersionsOf(request).map(_.toString)
    result
  }

  // https://maven.apache.org/guides/mini/guide-relocation.html
  def getRelocationOf(groupID: String, artifactId: String, version: String): Option[Gav3] = {
    None
  }

}

object Repo extends LazyLogging {

  def of(opts: Opts): Repo = new Repo(opts)

  case class ReachableResult(online: Boolean, msg: String)

  logger.debug("init aether to suppress replayed slf4j logging - See also http://www.slf4j.org/codes.html#replay")

  val centralUrl = "https://repo1.maven.org/maven2/"

  def newDefaultRepository(url: String) = new RemoteRepository.Builder("central", "default", url).build

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

  def parseCentralDate(line: String): Option[ZonedDateTime] = {
    if (line.matches(".*[0-9]{4}-[0-9]{2}-[0-9]{2}.*")) {
      try {
        val spl = line.replaceAll("[ ]+", " ").split(" ")
        val input = spl(1) + " " + spl(2) + " Z"
        val fmtb = new DateTimeFormatterBuilder()
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
        Some(ZonedDateTime.parse(input, fmtb).withZoneSameInstant(ZoneOffset.UTC))
      } catch {
        case e: Exception => None
      }
    } else {
      None
    }
  }

  def parseNexusDate(input: String): Option[ZonedDateTime] = {
    try {

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
      val fmtb = new DateTimeFormatterBuilder()
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
      Some(ZonedDateTime.parse(input, fmtb).withZoneSameInstant(ZoneOffset.UTC))
    } catch {
      case e: Exception => None
    }
  }

  private def depDate(repository: RemoteRepository)(groupId: String, artifactId: String, version: String, retry: Boolean = true): Option[ZonedDateTime] = {
    val system = ArtifactRepos.newRepositorySystem
    val session = ArtifactRepos.newRepositorySystemSession(system, silent = false)
    val req = new MetadataRequest()
    req.setMetadata(new DefaultMetadata(groupId, artifactId, version, "", Nature.RELEASE_OR_SNAPSHOT))
    req.setRepository(repository)

    val owet = system.resolveMetadata(session, ImmutableList.of(req))
    owet.asScala.flatMap(e => {
      if (!e.isResolved) {
        if (retry) {
          tryResolveReq(repository)(groupId + ":" + artifactId + ":pom:" + version)
          depDate(repository)(groupId, artifactId, version, retry = false)
        } else {
          Nil
        }
      } else {
        val file = e.getMetadata.getFile
        Util.readLines(file)
          .map(l => l.replaceAll("\\<.*?\\>", ""))
          .filter(l => l.matches(".*[0-9].*") && l.matches(".*[a-zA-Z].*"))
          .map(_.trim)
          .filterNot(_.isEmpty)
          .flatMap(line => {
            parseNexusDate(line)
              .orElse(parseCentralDate(line))
          })
          .distinct
      }
    }).toSeq.minOption
  }

  type VersionString = String

  def tryResolve(repository: RemoteRepository)(groupID: String, artifactId: String, version: String): Try[(File, VersionString)] = {
    try {
      Success(resolve(repository)(groupID, artifactId, version))
    } catch {
      case e: Exception => Failure(e)
    }
  }

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

  private class ManualRepositorySystemFactory {

  }

  private object ManualRepositorySystemFactory {
    def newRepositorySystem: RepositorySystem = {
      val locator = MavenRepositorySystemUtils.newServiceLocator
      locator.addService(classOf[RepositoryConnectorFactory], classOf[BasicRepositoryConnectorFactory])
      locator.addService(classOf[TransporterFactory], classOf[FileTransporterFactory])
      locator.addService(classOf[TransporterFactory], classOf[HttpTransporterFactory])
      locator.setErrorHandler(new DefaultServiceLocator.ErrorHandler() {
        override def serviceCreationFailed(`type`: Class[_], impl: Class[_], exception: Throwable): Unit = {
          if (exception.isInstanceOf[ArtifactNotFoundException]) {
            System.err.println(classOf[ManualRepositorySystemFactory].getCanonicalName + " -> " + exception.getMessage)
          } else {
            exception.printStackTrace()
          }
        }
      })
      locator.getService(classOf[RepositorySystem])
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
      if (!silent) session.setTransferListener(new TraceTransferListener)
      //   session.setRepositoryListener(new ConsoleRepositoryListener());
      // uncomment to generate dirty trees
      // session.setDependencyGraphTransformer( null );
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

  private class ConsoleTransferListener(_out: PrintStream = null) extends AbstractTransferListener {
    private var out: PrintStream = null
    this.out = if (_out != null) {
      _out
    } else {
      System.out
    }
    private val downloads = new ConcurrentHashMap[TransferResource, Long]
    private var lastLength = 0

    override def transferInitiated(event: TransferEvent): Unit = {
      val message = if (event.getRequestType eq TransferEvent.RequestType.PUT) {
        "Uploading"
      } else {
        "Downloading"
      }
      out.println(message + ": " + event.getResource.getRepositoryUrl + event.getResource.getResourceName)
    }

    override def transferProgressed(event: TransferEvent): Unit = {
      val resource = event.getResource
      downloads.put(resource, event.getTransferredBytes)
      val buffer = new StringBuilder(64)
      for (entry <- downloads.asScala) {
        val total = entry._1.getContentLength
        val complete = entry._2.longValue
        buffer.append(getStatus(complete, total)).append("  ")
      }
      val padV = lastLength - buffer.length
      lastLength = buffer.length
      pad(buffer, padV)
      buffer.append('\r')
      out.print(buffer)
    }

    override def transferCorrupted(event: TransferEvent): Unit = {
      event.getException.printStackTrace(out)
    }

    override def transferSucceeded(event: TransferEvent): Unit = {
      transferCompleted(event)
      val resource = event.getResource
      val contentLength = event.getTransferredBytes
      if (contentLength >= 0) {
        val action = if (event.getRequestType eq TransferEvent.RequestType.PUT) {
          "Uploaded"
        } else {
          "Downloaded"
        }
        val len = if (contentLength >= 1024) {
          s"${toKB(contentLength)} KB"
        } else {
          s"${contentLength} B"
        }
        var throughput = ""
        val duration = System.currentTimeMillis - resource.getTransferStartTime
        if (duration > 0) {
          val bytes = contentLength - resource.getResumeOffset
          val format = new DecimalFormat("0.0", new DecimalFormatSymbols(Locale.ENGLISH))
          val kbPerSec = (bytes / 1024.0) / (duration / 1000.0)
          throughput = " at " + format.format(kbPerSec) + " KB/sec"
        }
        out.println(action + ": " + resource.getRepositoryUrl + resource.getResourceName + " (" + len + throughput + ")")
      }
    }

    override def transferFailed(event: TransferEvent): Unit = {
      transferCompleted(event)
      if (logStacktrace(event)) {
        event.getException.printStackTrace(out)
      }
    }

    private def logStacktrace(event: TransferEvent) = {
      val of = Util.toJavaList(Seq(classOf[MetadataNotFoundException], classOf[ArtifactNotFoundException]))
      if (of.contains(event.getException.getClass)) false
      else true
    }

    private def getStatus(complete: Long, total: Long): String = if (total >= 1024) {
      s"${toKB(complete)} / ${toKB(total)} KB "
    } else if (total >= 0) {
      s"$complete / $total B "
    } else if (complete >= 1024) {
      s"${toKB(complete)} KB "
    } else {
      s"$complete B "
    }

    private def pad(buffer: StringBuilder, _spaces: Int): Unit = {
      var spaces = _spaces
      val block = "                                        "
      while (spaces > 0) {
        val n: Int = Math.min(spaces, block.length)
        buffer.append(block, 0, n)
        spaces = spaces - n
      }
    }

    private def transferCompleted(event: TransferEvent): Unit = {
      downloads.remove(event.getResource)
      val buffer: StringBuilder = new StringBuilder(64)
      pad(buffer, lastLength)
      buffer.append('\r')
      out.print(buffer)
    }

    protected def toKB(bytes: Long): Long = {
      (bytes + 1023) / 1024
    }
  }

}

