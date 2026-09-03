package release

import org.gradle.tooling.GradleConnector
import org.gradle.tooling.model.idea.{IdeaProject, IdeaSingleEntryLibraryDependency}
import release.PomMod.DepTree
import release.ProjectMod.{Dep, Gav3, SelfRef}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

case class GradleMod(file: File, repoZ: RepoZ, opts: Opts) extends ProjectMod {

  override lazy val repo: RepoZ = repoZ

  private val root = file.getParentFile
  private val properties = GradleMod.propertiesOf(GradleMod.gradleProperties(root))
  private val modelsByFile: Seq[(File, GradleMod.GradleModel)] = GradleMod.buildFiles(root)
    .map(f => (f, GradleMod.modelOfContents(GradleMod.read(f), properties)))
  private val rootModel = modelsByFile.find(_._1 == file).map(_._2).getOrElse(GradleMod.GradleModel(Nil, None, None, Nil))
  private val parsedDependencies = modelsByFile.flatMap(_._2.deps).distinct
  private lazy val toolingDependencies = GradleMod.configuredGradleHome
    .map(GradleMod.dependenciesFrom(root, _))
  private lazy val effectiveDependencies = toolingDependencies match {
    case Some(dependencies) => dependencies.recover({ case exception =>
        logger.warn(s"Gradle Tooling API model failed for $root; using the static parser", exception)
        parsedDependencies
      }).get
    case None => parsedDependencies
  }

  override lazy val depInFiles: Seq[(ProjectMod.Dep, File)] = toolingDependencies
    .flatMap(_.toOption)
    .map(_.map((_, file)))
    .getOrElse(modelsByFile.flatMap((f, model) => model.deps.map((_, f))))
  override val listDependencies: Seq[ProjectMod.Dep] = effectiveDependencies
  override val listDependenciesPlugin: Seq[ProjectMod.Dep] = Nil
  override val listRawDeps: Seq[ProjectMod.Dep] = listDependencies
  override val listPluginDependencies: Seq[ProjectMod.PluginDep] = Nil
  override val listProperties: Map[String, String] = properties
  override val skipPropertyReplacement: Boolean = true

  val selfVersion: String = rootModel.selfVersion.orElse(properties.get("version")).getOrElse("n/a")

  override def listRemoteRepoUrls(): Seq[String] = modelsByFile.flatMap(_._2.repositoryUrls).distinct

  override def getDepTreeFileContents: Map[File, DepTree] = Map.empty

  override def isShop: Boolean = listDependencies.exists(_.groupId.startsWith("com.novomind.ishop"))

  override val selfDepsMod: Seq[ProjectMod.Dep] = Nil

  override def suggestReleaseVersions(branchNames: Seq[String], tagNames: Seq[String], increment: Option[Increment] = None): Seq[String] =
    throw new UnsupportedOperationException("suggesting Gradle release versions is not supported yet")

  override def suggestNextRelease(releaseVersion: String): String =
    throw new UnsupportedOperationException("suggesting the next Gradle release is not supported yet")

  override def listSnapshotDependenciesDistinct: Seq[ProjectMod.Dep] =
    listDependencies.filter(_.version.exists(_.endsWith("-SNAPSHOT"))).distinct

  override def writeTo(targetFolder: File): Unit =
    throw new UnsupportedOperationException("writing Gradle builds is not supported yet")

  override def changeVersion(newVersion: String): Unit =
    throw new UnsupportedOperationException("changing Gradle project versions is not supported yet")

  override def changeDependecyVersion(patch: Seq[(Gav3, String)]): Unit =
    throw new UnsupportedOperationException("changing Gradle dependency versions is not supported yet")

  override def depTreeFilenameList(): Seq[String] = Nil
}

object GradleMod {

  private val GradleHomeProperty = "release.gradle.home"
  private val GradleHomeEnvironment = "RELEASE_GRADLE_HOME"

  case class GradleModel(deps: Seq[ProjectMod.Dep], selfVersion: Option[String], groupId: Option[String], repositoryUrls: Seq[String])

  private val assignment =
    """(?m)^\s*(?:(?:def|val|var)\s+|(?:ext\.)?)([A-Za-z_][A-Za-z0-9_.]*)\s*=\s*["']([^"']*)["']""".r
  private val methodAssignment = """(?m)^\s*(group|version)\s+["']([^"']+)["']""".r
  private val propertyAssignment = """(?m)^\s*([A-Za-z_][A-Za-z0-9_.]*)\s*=\s*([^#\s]+)\s*$""".r
  private val configuration = """^\s*([A-Za-z_][A-Za-z0-9_]*)\s*[\s(]""".r
  private val stringNotation = """["']([^"']+:[^"']+:[^"']+)["']""".r
  private val groupField = """(?:group|groupId)\s*[:=]\s*["']([^"']+)["']""".r
  private val nameField = """(?:name|module)\s*[:=]\s*["']([^"']+)["']""".r
  private val versionField = """version\s*[:=]\s*["']([^"']+)["']""".r
  private val repositoryUrl =
    """(?m)\burl\s*(?:=\s*)?(?:uri\s*\(\s*)?["'](https?://[^"']+)["']""".r

  private val dependencyConfigurations = Set(
    "api",
    "implementation",
    "compile",
    "compileonly",
    "runtime",
    "runtimeonly",
    "testcompile",
    "testimplementation",
    "testcompileonly",
    "testruntime",
    "testruntimeonly",
    "annotationprocessor",
    "kapt",
    "ksp",
    "classpath"
  )

  def buildGradle(root: File): Option[File] = {
    Seq(new File(root, "build.gradle.kts"), new File(root, "build.gradle")).find(_.canRead)
  }

  def withRepo(workfolder: File, opts: Opts, repo: RepoZ): GradleMod = {
    GradleMod(buildGradle(workfolder).getOrElse(throw new IllegalArgumentException(s"no Gradle build in $workfolder")), repo, opts)
  }

  /** Uses the Tooling API only with an existing local Gradle installation. In particular, this deliberately does not let GradleConnector
    * select the wrapper/build distribution because that may download it.
    */
  def dependenciesFrom(root: File, gradleHome: File): Try[Seq[Dep]] = Try {
    Using.resource(GradleConnector.newConnector()
        .forProjectDirectory(root)
        .useInstallation(gradleHome)
        .connect()) { connection =>
      val project = connection.getModel(classOf[IdeaProject])
      project.getModules.asScala
        .flatMap(_.getDependencies.asScala.collect({
          case dependency: IdeaSingleEntryLibraryDependency => fromToolingDependency(dependency)
        }).flatten)
        .toSeq
        .distinct
    }
  }

  private[release] def configuredGradleHome: Option[File] = {
    sys.props.get(GradleHomeProperty)
      .orElse(sys.env.get(GradleHomeEnvironment))
      .map(new File(_))
      .filter(isGradleInstallation)
  }

  private def isGradleInstallation(home: File): Boolean = {
    home.isDirectory && (new File(home, "bin/gradle").isFile || new File(home, "bin/gradle.bat").isFile)
  }

  private[release] def fromToolingDependency(dependency: IdeaSingleEntryLibraryDependency): Option[Dep] = {
    Option(dependency.getGradleModuleVersion).map(module =>
      Dep(SelfRef.undef, module.getGroup, module.getName, Option(module.getVersion), "",
        toolingScope(dependency.getScope.getScope), "", "", Nil))
  }

  private[release] def toolingScope(scope: String): String = scope.toLowerCase match {
    case "compile" => ""
    case "provided" => "provided"
    case "runtime" => "runtime"
    case "test" => "test"
    case other => other
  }

  def gradleProperties(root: File): File = new File(root, "gradle.properties")

  private[release] def read(file: File): String = {
    if (file.canRead) Files.readString(file.toPath, StandardCharsets.UTF_8) else ""
  }

  def propertiesOf(file: File): Map[String, String] = {
    propertyAssignment.findAllMatchIn(read(file)).map(m => m.group(1) -> m.group(2)).toMap
  }

  def buildFiles(root: File): Seq[File] = {
    if (!root.isDirectory) {
      Nil
    } else {
      Using.resource(Files.walk(root.toPath)) { paths =>
        paths.iterator().asScala
          .filter(Files.isRegularFile(_))
          .filter(path => path.getFileName.toString == "build.gradle" || path.getFileName.toString == "build.gradle.kts")
          .filterNot(isIgnoredBuildFile(root.toPath, _))
          .map(_.toFile)
          .toSeq
          .sortBy(_.getAbsolutePath)
      }
    }
  }

  private def isIgnoredBuildFile(root: Path, file: Path): Boolean = {
    root.relativize(file).iterator().asScala
      .map(_.toString)
      .exists(part => part == ".gradle" || part == ".git" || part == "build" || part == "target")
  }

  def modelOfContents(contents: String, externalProperties: Map[String, String] = Map.empty): GradleModel = {
    val buildProperties = assignment.findAllMatchIn(contents).map(m => m.group(1) -> m.group(2)).toMap ++
      methodAssignment.findAllMatchIn(contents).map(m => m.group(1) -> m.group(2)).toMap
    val variables = externalProperties ++ buildProperties
    val deps = contents.linesIterator.flatMap(line => dependencyOf(line, variables)).toSeq
    GradleModel(
      deps = deps.distinct,
      selfVersion = variables.get("version"),
      groupId = variables.get("group"),
      repositoryUrls = repositoryUrl.findAllMatchIn(contents).map(_.group(1)).toSeq.distinct
    )
  }

  private def dependencyOf(line: String, variables: Map[String, String]): Option[Dep] = {
    val uncommented = line.replaceFirst("\\s+//.*$", "").trim
    configuration.findFirstMatchIn(uncommented)
      .filter(m => isDependencyConfiguration(m.group(1)))
      .flatMap(m => {
        val scope = scopeOf(m.group(1))
        stringNotation.findFirstMatchIn(uncommented)
          .flatMap(n => depOfNotation(resolve(n.group(1), variables), scope))
          .orElse(depOfMapNotation(uncommented, variables, scope))
      })
  }

  private def isDependencyConfiguration(name: String): Boolean = {
    val lower = name.toLowerCase
    dependencyConfigurations.contains(lower) ||
    lower.endsWith("implementation") || lower.endsWith("compileonly") ||
    lower.endsWith("runtimeonly") || lower.endsWith("annotationprocessor")
  }

  private def scopeOf(configuration: String): String = {
    val lower = configuration.toLowerCase
    if (lower.startsWith("test")) "test"
    else if (lower.contains("runtime")) "runtime"
    else if (lower.contains("compileonly")) "provided"
    else ""
  }

  private def depOfNotation(notation: String, scope: String): Option[Dep] = {
    val parts = notation.split(":", -1)
    if (parts.length < 3 || parts.take(3).exists(_.isBlank)) {
      None
    } else {
      val versionAndExtension = parts(2).split("@", 2)
      Some(Dep(SelfRef.undef, parts(0), parts(1), Some(versionAndExtension(0)), "", scope,
          versionAndExtension.lift(1).getOrElse(""), parts.lift(3).getOrElse(""), Nil))
    }
  }

  private def depOfMapNotation(line: String, variables: Map[String, String], scope: String): Option[Dep] = {
    for {
      group <- groupField.findFirstMatchIn(line).map(m => resolve(m.group(1), variables))
      name <- nameField.findFirstMatchIn(line).map(m => resolve(m.group(1), variables))
      version <- versionField.findFirstMatchIn(line).map(m => resolve(m.group(1), variables))
    } yield Dep(SelfRef.undef, group, name, Some(version), "", scope, "", "", Nil)
  }

  private def resolve(value: String, variables: Map[String, String]): String = {
    variables.foldLeft(value)((result, entry) =>
      result.replace("${" + entry._1 + "}", entry._2).replace("$" + entry._1, entry._2))
  }
}
