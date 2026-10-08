package release

import com.typesafe.scalalogging.LazyLogging
import japicmp.cmp.{JApiCmpArchive, JarArchiveComparator, JarArchiveComparatorOptions}
import japicmp.config.Options
import japicmp.exception.JApiCmpException
import japicmp.model.{AccessModifier, JApiChangeStatus}
import japicmp.output.OutputFilter
import japicmp.output.semver.SemverOut
import release.ProjectMod.Gav
import org.eclipse.aether.transfer.ArtifactNotFoundException

import java.io.PrintStream
import java.nio.file.Files
import java.util.concurrent.{CancellationException, ExecutionException, FutureTask, TimeUnit, TimeoutException}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object SemverSuggester extends LazyLogging {
  sealed trait Result {
    def message: String
  }

  case class Recommendation(baseVersion: String, snapshotVersion: String, increment: String,
      nextVersion: String, modules: Int, apiChanged: Boolean, notes: Seq[String] = Nil,
      incomplete: Boolean = false) extends Result {
    def message: String = {
      val detail = if (apiChanged) "API changes detected" else "no API changes detected; feature changes may still require MINOR"
      val qualification = if (!incomplete) "" else if (increment == "patch") "provisional " else "at least "
      val prefix = if (incomplete || notes.nonEmpty) "W" else "I"
      s"${prefix}: japicmp suggests ${qualification}${increment.toUpperCase}: ${nextVersion} " +
        s"(${baseVersion} -> ${snapshotVersion}, ${modules} modules; ${detail}; Nexus snapshot, not the local working tree)." +
        (if (notes.isEmpty) "" else " " + notes.mkString("; "))
    }
  }

  case class Unavailable(reason: String) extends Result {
    def message: String = "W: No SemVer suggestion from japicmp: " + reason
  }

  final class Running private[SemverSuggester] (task: FutureTask[Result]) {
    private var reported = false

    // Report only at prompt boundaries, so the worker never interrupts terminal input.
    def reportIfReady(out: PrintStream, waitMillis: Long = 0L): Boolean = {
      if (reported) true
      else {
        try {
          val result = task.get(waitMillis, TimeUnit.MILLISECONDS)
          out.println(result.message)
          reported = true
          true
        } catch {
          case _: TimeoutException => false
          case _: CancellationException => reported = true; true
          case error: ExecutionException =>
            out.println(Unavailable("background analysis failed: " + error.getCause.getClass.getSimpleName).message)
            reported = true
            true
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
            false
        }
      }
    }

    def cancel(): Unit = { task.cancel(true); () }

    def finish(out: PrintStream): Unit = {
      if (!reportIfReady(out, 1000L)) {
        out.println("W: No SemVer suggestion: the background analysis has not finished; cancelling it.")
        cancel()
      }
    }
  }

  def start(currentVersion: String, tags: Seq[String], modules: Seq[Gav], repo: RepoZ,
      releaseModules: Option[String => Seq[Gav]] = None): Running = {
    val task = new FutureTask[Result](() => {
      val result = Conf.Tracer.msgAround("suggest SemVer from Nexus artifacts", logger,
        () => calculate(currentVersion, tags, modules, repo, releaseModules))
      logger.trace(result.message)
      result
    })
    val worker = new Thread(task, "release-semver-suggester")
    worker.setDaemon(true)
    worker.start()
    new Running(task)
  }

  private[release] def baseline(currentVersion: String, tags: Seq[String]): Option[String] = {
    baselines(currentVersion, tags).headOption
  }

  private def baselines(currentVersion: String, tags: Seq[String]): Seq[String] = {
    val major = "^([0-9]+)(?:[.x-].*)?$".r.findFirstMatchIn(currentVersion).flatMap(_.group(1).toIntOption)
    val candidates = tags.map(_.stripPrefix("v")).filter(_.matches("[0-9]+\\.[0-9]+\\.[0-9]+"))
      .flatMap(version => {
        val parts = version.split("\\.").toSeq.flatMap(_.toIntOption)
        Option.when(parts.size == 3 && major.contains(parts.head))((parts.head, parts(1), parts(2)) -> version)
      })
    candidates.sortBy(_._1).reverse.map(_._2).distinct
  }

  private def compiledModules(modules: Seq[Gav]): Seq[Gav] = modules.filterNot(_.packageing == "pom")
    .map(gav =>
      gav.copy(version = None, packageing = if (gav.packageing.isEmpty) "jar" else gav.packageing,
        classifier = "", scope = "")).distinct

  private[release] def modulesAtTag(sgit: Sgit, tag: String, opts: Opts, repo: RepoZ): Seq[Gav] = {
    val directory = Files.createTempDirectory("release-semver-poms-")
    try {
      val paths = sgit.gitNative(Seq("ls-tree", "-r", "--name-only", tag), showErrorsOnStdErr = false).get
        .filter(path => path == "pom.xml" || path.endsWith("/pom.xml"))
      paths.foreach(path => {
        val target = directory.resolve(path).normalize()
        require(target.startsWith(directory), "POM path outside temporary directory")
        Files.createDirectories(target.getParent)
        Files.writeString(target, sgit.gitNative(Seq("show", s"${tag}:${path}"), showErrorsOnStdErr = false).get.mkString("\n"))
      })
      PomMod.withRepo(directory.toFile, opts, repo, failureCollector = None, revisionFallback = Some(tag.stripPrefix("v")))
        .listSelf.map(_.gav())
    } finally FileUtils.deleteRecursive(directory.toFile)
  }

  private[release] def calculate(currentVersion: String, tags: Seq[String], modules: Seq[Gav], repo: RepoZ,
      releaseModules: Option[String => Seq[Gav]] = None): Result = {
    try {
      if (!currentVersion.endsWith("-SNAPSHOT")) return Unavailable("the current project version is not a SNAPSHOT.")
      val versions = baselines(currentVersion, tags)
      val latestVersion = versions.headOption match {
        case Some(version) => version
        case None => return Unavailable("no stable x.y.z release tag found in the current major series.")
      }
      val artifacts = compiledModules(modules)
      if (artifacts.exists(gav => gav.packageing != "jar" && gav.packageing != "war")) {
        return Unavailable("only JAR and WAR modules are supported; comparison would be incomplete.")
      }

      val notes = scala.collection.mutable.ArrayBuffer.empty[String]
      val cache = scala.collection.mutable.Map.empty[(Gav, String), Try[JApiCmpArchive]]
      def resolve(gav: Gav, version: String): Try[JApiCmpArchive] = cache.getOrElseUpdate(
        (gav, version), {
          if (Thread.currentThread().isInterrupted) throw new InterruptedException("SemVer analysis cancelled")
          val request = s"${gav.groupId}:${gav.artifactId}:${gav.packageing}:${version}"
          repo.tryResolveReqWorkNexus(request) match {
            case Success((file, resolvedVersion)) => Success(new JApiCmpArchive(file, resolvedVersion))
            case Failure(error) =>
              val details = Iterator.iterate(error)(_.getCause).take(8).takeWhile(_ != null)
                .flatMap(cause => Option(cause.getMessage)).map(_.replaceAll("\\s+", " ").trim)
                .filter(_.nonEmpty).toSeq.distinct.mkString("; ")
                .replaceAll("(?i)(https?://)[^\\s/]*@", "$1***@").take(1000)
              val reason = if (details.isEmpty) error.getClass.getSimpleName else details
              logger.warn(s"cannot download ${request}", error)
              Failure(new IllegalStateException(s"cannot download ${request}: ${reason} (details in ~/.release.log)", error))
          }
        }
      )
      def missing(error: Throwable): Boolean = Iterator.iterate(error)(_.getCause).take(8).takeWhile(_ != null)
        .exists(_.isInstanceOf[ArtifactNotFoundException])
      val newResults = artifacts.map(gav => gav -> resolve(gav, currentVersion)).toMap
      val candidates = versions.take(5).iterator
      var baseVersion = latestVersion
      var oldModules = Seq.empty[Gav]
      var oldResults = Map.empty[Gav, Try[JApiCmpArchive]]
      var searching = true
      while (searching && candidates.hasNext) {
        baseVersion = candidates.next()
        oldModules = compiledModules(releaseModules.map(_(baseVersion)).getOrElse(modules))
        if (oldModules.exists(gav => gav.packageing != "jar" && gav.packageing != "war")) {
          return Unavailable("only JAR and WAR modules are supported; comparison would be incomplete.")
        }
        oldResults = oldModules.map(gav => gav -> resolve(gav, baseVersion)).toMap
        val errors = oldResults.values.collect { case Failure(error) => error }.toSeq
        searching = errors.nonEmpty && errors.forall(missing) && candidates.hasNext
      }
      if (baseVersion != latestVersion) notes += s"comparison uses ${baseVersion}; newest Git release is ${latestVersion}"
      val added = artifacts.diff(oldModules)
      val removed = oldModules.diff(artifacts)
      if (added.nonEmpty) notes += s"${added.size} new module(s) in the Git POMs"
      if (removed.nonEmpty) notes += s"${removed.size} removed module(s) in the Git POMs"
      val failed = (oldModules ++ artifacts).distinct.filter(gav =>
        oldResults.get(gav).exists(_.isFailure) || newResults.get(gav).exists(_.isFailure))
      failed.foreach(gav => {
        (oldResults.get(gav).toSeq ++ newResults.get(gav).toSeq).collect { case Failure(error) => error }
          .foreach(error => notes += error.getMessage)
      })
      val usable = (oldModules ++ artifacts).distinct.diff(failed)
      val oldArchives = usable.flatMap(gav => oldResults.get(gav).flatMap(_.toOption))
      val newArchives = usable.flatMap(gav => newResults.get(gav).flatMap(_.toOption))
      if (oldArchives.isEmpty && newArchives.isEmpty && added.isEmpty && removed.isEmpty) {
        return Unavailable((notes.toSeq :+ "no compiled Maven modules could be compared").mkString("; "))
      }
      val options = Options.newDefault()
      options.setIgnoreMissingClasses(false)
      options.setOutputOnlyModifications(true)
      options.setAccessModifier(AccessModifier.PROTECTED)
      val comparator = new JarArchiveComparator(JarArchiveComparatorOptions.of(options))
      var missingClasses = false
      val compared = Try(comparator.compare(oldArchives.asJava, newArchives.asJava)).recoverWith {
        case error: JApiCmpException if error.getReason == JApiCmpException.Reason.ClassLoading =>
          missingClasses = true
          logger.warn("SemVer comparison continues with missing classes", error)
          notes += "missing classes ignored: " + error.getMessage.take(500)
          options.setIgnoreMissingClasses(true)
          Try(new JarArchiveComparator(JarArchiveComparatorOptions.of(options))
              .compare(oldArchives.asJava, newArchives.asJava))
      }
      compared.failed.foreach(error =>
        notes += "API comparison incomplete: " + error.getClass.getSimpleName + " (details in ~/.release.log)")
      compared.failed.foreach(error => logger.warn("SemVer API comparison incomplete", error))
      if (compared.isFailure && added.isEmpty && removed.isEmpty) return Unavailable(notes.mkString("; "))
      val changes = compared.getOrElse(new java.util.ArrayList[japicmp.model.JApiClass]())
      if (changes.isEmpty && added.isEmpty && removed.isEmpty)
        return Unavailable("the downloaded archives contain no comparable API classes.")
      new OutputFilter(options).filter(changes)
      // Class status also includes private members; discard classes changed only by filtered members.
      changes.removeIf(c =>
        c.isChangeCausedByClassElement && c.getMethods.isEmpty &&
          c.getConstructors.isEmpty && c.getFields.isEmpty && c.getAnnotations.isEmpty &&
          c.getCompatibilityChanges.isEmpty && c.getClassType.getChangeStatus == JApiChangeStatus.UNCHANGED &&
          c.getSuperclass.getChangeStatus == JApiChangeStatus.UNCHANGED &&
          c.getInterfaces.asScala.forall(_.getChangeStatus == JApiChangeStatus.UNCHANGED) &&
          c.getModifiers.asScala.forall(_.getChangeStatus == JApiChangeStatus.UNCHANGED) &&
          c.getGenericTemplates.asScala.forall(_.getChangeStatus == JApiChangeStatus.UNCHANGED))
      val apiChanged = !changes.isEmpty || added.nonEmpty || removed.nonEmpty
      // japicmp classifies some compatible additions as PATCH. SemVer requires MINOR for added API.
      val addedApi = changes.asScala.exists(c =>
        c.getChangeStatus == JApiChangeStatus.NEW ||
          c.getMethods.asScala.exists(_.getChangeStatus == JApiChangeStatus.NEW) ||
          c.getConstructors.asScala.exists(_.getChangeStatus == JApiChangeStatus.NEW) ||
          c.getFields.asScala.exists(_.getChangeStatus == JApiChangeStatus.NEW))
      val semver = if (removed.nonEmpty) "1.0.0"
      else {
        val diff = new SemverOut(options, changes).generate()
        if (diff != "1.0.0" && added.nonEmpty) "0.1.0" else diff
      }
      val parts = latestVersion.split("\\.").map(_.toInt)
      val (increment, next) = semver match {
        case "1.0.0" => "major" -> s"${parts(0) + 1}.0.0"
        case "0.1.0" => "minor" -> s"${parts(0)}.${parts(1) + 1}.0"
        case "0.0.1" if addedApi => "minor" -> s"${parts(0)}.${parts(1) + 1}.0"
        case "0.0.1" | "0.0.0" => "patch" -> s"${parts(0)}.${parts(1)}.${parts(2) + 1}"
        case _ => return Unavailable("japicmp could not classify the API changes.")
      }
      val incomplete = failed.nonEmpty || compared.isFailure || missingClasses || baseVersion != latestVersion
      if (incomplete) notes += "incomplete comparison; additional breaking changes may require MAJOR"
      Recommendation(baseVersion, currentVersion, increment, next, usable.size, apiChanged, notes.toSeq, incomplete)
    } catch {
      case NonFatal(error) =>
        logger.warn("SemVer suggestion unavailable", error)
        Unavailable(Option(error.getMessage).getOrElse(error.getClass.getSimpleName))
    }
  }
}
