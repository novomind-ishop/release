package release

import java.io.{IOException, PrintStream}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.HttpClients
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.repository.{LocalRepository, RemoteRepository}
import org.eclipse.aether.resolution.{VersionRangeRequest, VersionRangeResolutionException}
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transfer._
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.eclipse.aether.version.Version
import org.eclipse.aether.{DefaultRepositorySystemSession, RepositorySystem}
import release.Aether.getVersions
import release.Starter.Opts

import scala.jdk.CollectionConverters._


class Aether(opts: Opts) extends LazyLogging {

  lazy val newRepositoriesCentral: RemoteRepository = Aether.newDefaultRepository(Aether.centralUrl)

  lazy val mirrorNexus: RemoteRepository = Aether.newDefaultRepository(ReleaseConfig.default(opts.useDefaults).mirrorNexusUrl())

  lazy val workNexus: RemoteRepository = Aether.newDefaultRepository(ReleaseConfig.default(opts.useDefaults).workNexusUrl())

  lazy val allRepos: Seq[RemoteRepository] = Seq(workNexus, mirrorNexus)

  private def getVersionsOf(req: String) = getVersions(workNexus)(req)

  private[release] def isReachable(showTrace: Boolean = true): Boolean = {
    val httpclient = HttpClients.createDefault
    val httpGet = new HttpGet(workNexus.getUrl)
    var response: CloseableHttpResponse = null
    val code: Int = try {

      response = httpclient.execute(httpGet)
      response.getStatusLine.getStatusCode
    } catch {
      case any: Throwable => {
        if (showTrace) {
          any.printStackTrace()
        }
        return false
      }
    } finally {
      if (response != null) {
        response.close()
      }

    }
    code != 0

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

}

object Aether extends LazyLogging {
  logger.debug("init aether to suppress replayed slf4j logging - See also http://www.slf4j.org/codes.html#replay")

  val centralUrl = "http://central.maven.org/maven2/"

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

    private def newRepositorySystemSession(system: RepositorySystem, silent: Boolean): DefaultRepositorySystemSession = {
      val session = MavenRepositorySystemUtils.newSession
      val localRepo = new LocalRepository("target/local-repo/" + Util.hashMd5Random())
      localRepo.getBasedir.getAbsoluteFile.getParentFile.mkdir()
      localRepo.getBasedir.getAbsoluteFile.mkdir()
      delete(localRepo.getBasedir.getAbsoluteFile.toPath)
      session.setLocalRepositoryManager(system.newLocalRepositoryManager(session, localRepo))
      if (!silent) session.setTransferListener(new ConsoleTransferListener)
      //   session.setRepositoryListener(new ConsoleRepositoryListener());
      // uncomment to generate dirty trees
      // session.setDependencyGraphTransformer( null );
      session
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

