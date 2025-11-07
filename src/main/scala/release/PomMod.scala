package release

import com.google.common.base.Strings
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.w3c.dom.{Document, Node}
import release.Conf.Tracer
import release.PomMod._
import release.ProjectMod._
import release.Starter.{PreconditionsException}
import release.Util.Ext.*

import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.LocalDate
import java.time.temporal.WeekFields
import java.util.Locale
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, TransformerFactory}
import scala.annotation.{nowarn, tailrec}
import scala.collection.parallel.CollectionConverters._
import scala.util.{Failure, Success, Try}

case class PomMod(file: File, repoZ: RepoZ, opts: Opts,
                  skipPropertyReplacement: Boolean = false, withSubPoms: Boolean,
                  failureCollector: Option[Exception => Unit]) extends ProjectMod with LazyLogging {
  logger.trace("init pomMod")
  override lazy val repo: RepoZ = repoZ
  private var depMap: Map[Dep, Node] = Map.empty

  private val rootPom = PomMod.rootPom(file)
  if (!rootPom.canRead) {
    throw new PreconditionsException(file.toString + " seems to be no maven project")
  }
  if (rootPom.canRead && FileUtils.read(rootPom).trim.isEmpty) {
    throw new PreconditionsException(file.toString + " seems to be no maven project // empty file")
  }
  private val allRawPomFiles = Tracer.msgAround("read all poms", logger,
    () => PomMod.allRawModulePomsFiles(Seq(file), withSubPoms))

  private[release] val raws: Seq[RawPomFile] = toRawPoms(allRawPomFiles)
  override lazy val depInFiles: Seq[(Dep, File)] = raws.flatMap(d => {
    val result = PomMod.selfDep(_ => {})(d.document)
    result.map(e => (e, d.pomFile))
  })
  private[release] val allPomsDocs: Seq[Document] = raws.map(_.document).toList

  private[release] val mvnExtension: Option[File] = {
    val dotMvn = new File(rootPom.getParentFile, ".mvn")
    val extension = new File(dotMvn, "extensions.xml")
    if (extension.canRead && extension.isFile) {
      Some(extension)
    } else {
      None
    }
  }

  private def depU(d: Map[Dep, Node]): Unit = {
    depMap = depMap ++ d
  }

  private[release] val listSelf: Seq[Dep] = {
    allPomsDocs.flatMap(PomMod.selfDep(depU))
  }

  val selfDepsMod: Seq[Dep] = {
    val selfDeps: Seq[Dep] = listSelf ++
      listSelf.map(_.copy(scope = "test")) ++
      listSelf.map(_.copy(scope = "test", classifier = "tests")) ++
      listSelf.map(_.copy(scope = "test", classifier = "tests", packaging = "")) ++
      listSelf.map(_.copy(scope = "test", packaging = "")) ++
      listSelf.map(_.copy(classifier = "tests")) ++
      listSelf.map(_.copy(classifier = "tests", packaging = "")) ++
      listSelf.map(_.copy(classifier = "sources")) ++ // for bo-client
      listSelf.map(_.copy(classifier = "", typeN = "war", scope = "runtime", packaging = "")) ++ // for bo-client
      listSelf.map(_.copy(packaging = ""))
    val pomMods = selfDeps.map(_.copy(typeN = "pom"))
    val pomModsImport = pomMods.map(_.copy(scope = "import"))
    (pomMods ++ pomModsImport ++ selfDeps)
      .map(_.copy(pomRef = SelfRef.undef, pomPath = Nil))
      .distinct
      .sortBy(_.toString)
  }

  private val rootPomGav: Seq[Gav] = selfDepsMod.map(_.gav().copy(packageing = "")).distinct

  private val currentVersion: Option[String] = {
    Xpath.onlyString(Xpath.documentOfFile(rootPom), PomMod.xPathToProjectVersion)
  }

  val listProperties: Map[String, String] = {
    PomMod.listProperties(opts, raws, failureCollector, allPomsDocs, listSelf)
  }

  val selfVersion: String = {
    val v = listSelf.map(_.version.get).distinct
    PomMod.checkSnapshots(v)
    Util.only(v, {
      val selection: String = listSelf.groupBy(_.version.get) match {
        case grouped: Map[String, Seq[Dep]] if grouped.keySet.size == 2 => {

          val sizeMap: Map[String, Int] = grouped.map(entry => (entry._1, entry._2.size))
          val key = sizeMap.toList.minBy(_._2)._1
          grouped(key).toList.mkString(", ")
        }
        case grouped => grouped.toString()
      }
      "More then one Version found in your pom.xmls: " + selection
    })
  }

  override lazy val selfVersionReplaced: String = {
    replaceWithLimit(listProperties, selfVersion)
  }

  val listRawDeps: Seq[Dep] = allPomsDocs.flatMap(deps)
  val listDependencies: Seq[Dep] = {
    val oo = replacedVersionProperties(listProperties, skipPropertyReplacement)(listRawDeps) // never distinct
    oo
  }

  val listRawDepsPlugin: Seq[Dep] = allPomsDocs.flatMap(pluginInnerDeps)
  val listDependenciesPlugin: Seq[Dep] = {
    val oo = replacedVersionProperties(listProperties, skipPropertyReplacement)(listRawDepsPlugin) // never distinct
    oo
  }

  if (opts.checkOverlapping) {
    PomChecker.checkDepScopes(listDependencies, listDependenciesPlugin)
    PomChecker.checkDepVersions(listDependencies, listRawDeps)
    PomChecker.checkOwnArtifactNames(depInFiles, file)
  }
  if (opts.checkProjectDeps) {
    PomChecker.checkExternalWithProjectScope(listRawDeps, selfDepsMod, listProperties)
  }

  val listPluginDependencies: Seq[PluginDep] = {

    val extensionsDeps = mvnExtension.toSeq.flatMap(PomMod.toPluginDep)
    val allP: Seq[PluginDep] = allPomsDocs.map(in => {
      val gav = PomMod.selfDep(depU)(in).get.gavWithDetailsFormatted
      val nodes = Xpath.toSeqTuples(in, "//plugins/plugin")
      (nodes, gav)
    }).flatMap(gavNode => {
      val oo: Seq[Seq[(String, String, Node)]] = gavNode._1
      val x = gavNode._2
      oo.map(pluginDepFrom(x))
    })
    allP.toList ++ extensionsDeps
  }

  private[release] val mavenDependencyPlugins: Seq[PluginDep] = {
    PomMod.dependecyPlugins(listPluginDependencies)
  }

  private[release] def rawSub(f: String): Seq[String] = {
    val ouu = raws.map(_.subfolder + "/" + f)
    ouu
  }

  private[release] var depTreeFileContents: Map[File, DepTree] = depTreeFiles(depTreeFilename(), rawSub)
    .map(f => (f, DepTree(FileUtils.read(f))))
    .filterNot(in => in._1.getParentFile.getName == ".")
    .sortBy(_._1)
    .foldLeft(Map.empty[File, DepTree])(_ + _)

  def getDepTreeFileContents: Map[File, DepTree] = depTreeFileContents

  logger.trace("pomMod val/var init")

  private def nodePath(node: Node): Seq[String] = {

    @tailrec
    def nodePathOther(node: Node, others: Seq[String] = Nil): Seq[String] = {
      val parent = node.getParentNode
      if ("#document" == parent.getNodeName) {
        others
      } else {
        nodePathOther(parent, others ++ Seq(parent.getNodeName))
      }
    }

    Seq(node.getNodeName) ++ nodePathOther(node)
  }

  private def pluginDepFrom(id: String)(depSeq: Seq[(String, String, Node)]): PluginDep = {
    val deps = Xpath.toMapOf(depSeq)
    val node = Util.only(depSeq.map(_._3).distinct, "only a single instance is possible")
    val groupId = deps.getOrElse("groupId", "")
    val artifactId = deps.getOrElse("artifactId", "")
    val version = deps.get("version")

    val execNodes = Xpath.nodeElements(node, "executions/execution")
    val execs = execNodes.map(in => {
      val id = Xpath.nodeElementValue(in, "id").getOrElse("")
      val phase = Xpath.nodeElementValue(in, "phase").getOrElse("")
      val goals = Xpath.nodeElements(in, "goals/goal").map(_.getFirstChild.getTextContent)

      val configNodes = Xpath.nodeElementMap(in, "configuration")
      PluginExec(id, goals, phase, configNodes)
    })

    replacedVersionProperty(PluginDep(SelfRef.parse(id), groupId, artifactId, version, execs, nodePath(node)))
  }

  private def mavenDependencyPluginConfigsByGoal(goalname: String): Seq[(String, String)] = {
    mavenDependencyPlugins.flatMap(_.execs).filter(_.goals == Seq(goalname)).flatMap(_.config)
  }

  def findNodesAndChangeVersion(groupId: String, artifactId: String, version: String, newVersion: String): Unit = {
    findNodes(groupId, artifactId, version).foreach(n => {
      Xpath.toSeqNodes(n.getChildNodes).find(_.getNodeName == "version").foreach(as => {
        as.setTextContent(newVersion)
      })
    })

    changeDepTreesVersion(groupId, artifactId, version, newVersion)
  }

  private[release] def changeDepTreesVersion(groupId: String, artifactId: String, version: String, newVersion: String): Unit = {
    val edited = depTreeFileContents.toList
      .map(entry => {
        (entry._1, entry._2.copy(PomMod.replacedDepTreesVersion(entry, groupId, artifactId, version, newVersion)))
      })
    val folded = edited.foldLeft(Map.empty[File, DepTree])(_ + _)
    depTreeFileContents = folded
  }

  private[release] def changeDepTreesGA(groupId: String, artifactId: String, version: String,
                                        groupIdFn: String => String, artifactIdFn: String => String): Unit = {
    depTreeFileContents = depTreeFileContents.toList.map(entry => {
      (entry._1, entry._2.copy(replacedDepTreesGA(entry._2.content, groupId, artifactId, version, groupIdFn, artifactIdFn)))
    }).foldLeft(Map.empty[File, DepTree])(_ + _)
  }

  private def replacedDepTreesGA(in: String, groupId: String, artifactId: String, version: String,
                                 groupIdFn: String => String, artifactIdFn: String => String) = {
    in.linesIterator
      .map(_.replaceFirst(groupId + ":" + artifactId + ":war:" + version, groupIdFn.apply(groupId).replace("." + artifactIdFn.apply(artifactId), "") + ":" + artifactIdFn.apply(artifactId) + ":war:" + version))
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):([^:]*):" + version, groupIdFn.apply(groupId) + ":" + artifactIdFn.apply(artifactId) + ":$1:$2:" + version))
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):" + version, groupIdFn.apply(groupId) + ":" + artifactIdFn.apply(artifactId) + ":$1:" + version))
      .mkString("\n") + "\n"
  }

  def getVersionFromDocs(): String = {
    val out = raws.map(_.document).flatMap(d => {
      val parentVersion = Xpath.toSeq(d, PomMod.xPathToProjectParentVersion).map(_.getTextContent)
      val projectVersion = Xpath.toSeq(d, PomMod.xPathToProjectVersion).map(_.getTextContent)

      parentVersion ++ projectVersion ++ listSelf.map(_.version.get)
    }).distinct
    Util.only(out, "only one Version is allowed")
  }

  def changeShopGroupArtifact(newValue: String): Unit = {
    val pattern = "[a-z0-9]+"
    if (!newValue.matches(pattern)) {
      throw new PreconditionsException("invalid groupidArtifactName \"" + newValue + "\"; must match '" + pattern +
        "' - We enforce this to build GAs like: 'org.example.{groupidArtifactName}.main:{groupidArtifactName}-service' -" +
        " for example dashes (-) are not part of the naming conventions for groupIds and dots (.) not for artifactIds -" +
        " see also: https://maven.apache.org/guides/mini/guide-naming-conventions.html")
    }

    val replaceInArtifactId: (String => String) = in => {
      newValue + in.replaceFirst("^[^-]+", "")
    }

    val replaceInName: (String => String) = {
      case any if any.startsWith("Shopsystem:") => "Shopsystem:" + newValue.substring(0, 1).toUpperCase + newValue.substring(1)
      case any => newValue + any.replaceFirst("^[^-]+", "")
    }

    raws.foreach(d => {
      if (d.pomFile.getParentFile == file) {
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectGroupId, "com.novomind.ishop.shops." + newValue)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectArtifactId, replaceInArtifactId)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectName, replaceInName)
        PomMod.applyToGroupAndArtifactId(d, listSelf, _ => "com.novomind.ishop.shops." + newValue, replaceInArtifactId)
      } else {
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectParentGroupId, "com.novomind.ishop.shops." + newValue)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectParentArtifactId, replaceInArtifactId)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectName, replaceInName)
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectGroupId, "com.novomind.ishop.shops")
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectArtifactId, replaceInArtifactId)
        PomMod.applyToGroupAndArtifactId(d, listSelf, _ => "com.novomind.ishop.shops." + newValue, replaceInArtifactId)
      }
    })
    selfDepsMod.foreach(entry => {
      changeDepTreesGA(entry.groupId, entry.artifactId, entry.version.get, _ => "com.novomind.ishop.shops." + newValue, replaceInArtifactId)
    })
  }

  def changeDependecyVersion(patch: Seq[(Gav3, String)]): Unit = {
    println("@Beta: update all dependencies to latest")
    val allTo = listGavsForCheck()
    val toUp = allTo.flatMap(d => {
      val ups = patch.filter(pe => pe._1 == d.gav().simpleGav())
      if (ups.isEmpty) {
        None
      } else {
        Some((d, Util.only(ups.map(_._2), "only one update expected")))
      }
    })

    raws.foreach(d => {
      toUp.foreach(pc => {
        PomMod.applyVersionTo(d, Seq(pc._1), pc._2)
      })
    })
    println("HINT: you have uncommited changes")
    println("HINT: create trees manually")
  }

  def changeVersion(newVersion: String): Unit = {
    val oldVersion: String = PomMod.selectFirstVersionFrom(raws).get
    raws.foreach(d => {
      if (d.pomFile.getParentFile == file) {
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectVersion, oldVersion, newVersion)
        PomMod.applyVersionTo(d, listSelf, oldVersion, newVersion)
      } else {
        if (d.parentDep.isDefined && rootPomGav.contains(d.parentDep.get.gav())) {
          PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectParentVersion, oldVersion, newVersion)
        }
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectVersion, oldVersion, newVersion)
        PomMod.applyVersionTo(d, listSelf, oldVersion, newVersion)
      }
    })
    selfDepsMod.foreach(entry => {
      changeDepTreesVersion(entry.groupId, entry.artifactId, entry.version.get, newVersion)
    })

  }

  def depTreeFiles(theDepTreeFilename: Option[String], allFn: String => Seq[String]): Seq[File] = {
    val result = theDepTreeFilename
      .map(f => allFn.apply(f))
      .map(in => in.map(f => new File(f).getAbsoluteFile).filter(_.exists()))
      .getOrElse(Nil)
      .map(c => c.toPath.normalize())
      .distinct
      .map(_.toFile)
    result
  }

  def depTreeFilenameList(): Seq[String] = {
    depTreeFilename().map(Seq(_)).getOrElse(Nil)
  }

  def depTreeFilename(): Option[String] = {
    val depPluginConfigs: Seq[(String, String)] = mavenDependencyPluginConfigsByGoal("tree")
    if (depPluginConfigs != Nil) {
      val treeOutputFileName: String = Util.only(depPluginConfigs.filter(_._1 == "outputFile"),
        "more than one or none tree file definition in your pom.xmls - invalid pom parent layout" +
          " - please add <outputFile>dep.tree</outputFile>")._2
      Some(treeOutputFileName)
    } else {
      None
    }
  }

  def findNodes(groupId: String, artifactId: String, version: String): Seq[Node] = {
    depMap
      .toList
      .filter(dep => {
        val str = replacedPropertyOf(listProperties, skipPropertyReplacement)(dep._1.artifactId)
        val x = Util.only(Xpath.mapToSeqMap(Seq(dep._2)), "no element")
        str == artifactId && x.nonEmpty
      }).map(_._2)
      .filter(_ != null)
  }

  def writeTo(targetFolder: File): Unit = {
    raws.par.foreach(sub => {
      val path = file.toPath.relativize(sub.pomFile.toPath)
      PomMod.writePom(targetFolder)(targetFolder.toPath.resolve(path).toFile, sub.document)
    })

    depTreeFileContents
      .foreach(fe => {
        val path = file.toPath.normalize().relativize(fe._1.toPath.normalize()).normalize()
        val tF = targetFolder.toPath.resolve(path).toAbsolutePath.normalize().toFile
        PomMod.writeContent(targetFolder)(tF, fe._2.content)
      })
  }

  def isShop: Boolean = {
    val filtered = listSelf.filterNot(_.packaging == "pom")
    filtered.map(_.groupId).contains("com.novomind.ishop.shops")
  }

  private def replacedVersionProperty(dep: PluginDep): PluginDep = {
    dep.copy(version = dep.version.map(replacedPropertyOf(listProperties, skipPropertyReplacement)))
  }

  def listRemoteRepoUrls(): Seq[String] = {
    // TODO cat ~/.m2/settings.xml
    val localSettingsXml = new File("settings.xml")
    if (localSettingsXml.canRead) {
      PomMod.extractUrlsFromSettings(localSettingsXml)
    } else {
      Nil

    }
  }

  def listSnapshotDependenciesDistinct: Seq[Dep] = {
    Util.distinctOn[Dep, Dep](listSnapshotDependencies, _.copy(pomRef = SelfRef.undef))
  }

  def listSnapshotDependencies: Seq[Dep] = {
    val deps = listDependencies
    val filteredDeps = deps.filterNot(dep => {
      val mod = dep.copy(pomRef = SelfRef.undef, pomPath = Nil)
      selfDepsMod.contains(mod)
    })

    val replacedParams = replacedVersionProperties(listProperties, skipPropertyReplacement)(filteredDeps)
    val onlySnapshots = replacedParams.filter(_.version.getOrElse("").contains("SNAPSHOT"))
    onlySnapshots
  }

  private def deps(document: Document, xpathS: String, withParents: Boolean): Seq[Dep] = {
    val self: Option[Dep] = PomMod.selfDep(depU)(document)
    self.toSeq.flatMap(s => {
      val xmlNodes = Xpath.toSeqTuples(document, xpathS)

      val t = xmlNodes.filter(_.nonEmpty).map(PomMod.depFrom(s.pomRef.gav3, depU))
      if (withParents) {
        val maybeDep: Seq[Dep] = PomMod.parentDep(depU)(s, document).toList
        maybeDep ++ t
      } else {
        t
      }
    })
  }

  private def deps(document: Document): Seq[Dep] = {
    deps(document, xPathToDependecies, withParents = true)
  }

  private def pluginInnerDeps(document: Document): Seq[Dep] = {
    deps(document, xPathToPluginInnerDependecies, withParents = false)
  }

  private def checkCurrentVersion(current: Option[String]): Unit = {
    if (current.isEmpty) {
      throw new IllegalStateException(file.getName + " as no version, please define")
    }
  }

  def suggestReleaseVersion(branchNames: Seq[String] = Nil, tagNames: Seq[String] = Nil, increment: Option[Increment] = None): Seq[String] = {
    checkCurrentVersion(currentVersion)

    PomMod.suggestReleaseBy(LocalDate.now(), currentVersion.get, isShop, branchNames, tagNames, nextVersionFileContent(), increment)
  }

  def suggestNextRelease(releaseVersion: String): String = {
    checkCurrentVersion(currentVersion)
    PomMod.suggestNextReleaseBy(currentVersion.get, releaseVersion)
  }

  private def nextVersionFileContent(): () => String = {
    () => {
      try {
        val conf = ConfigFactory.parseFile(new File(rootPom.getParentFile, "nextVersion.conf"))
        val ll = conf.getString("nextVersion")
        ll
      } catch {
        case _: Exception => ""
      }
    }
  }

  private def toRawPom(pomFile: File): RawPomFile = {
    // TODO check if pom is sub sub module - parse
    RawPomFile(pomFile, PomMod.stripDependencyDefaults(Xpath.documentOfFile(pomFile)), file)
  }

  private def toRawPoms(pomFiles: Seq[File]): Seq[RawPomFile] = {
    pomFiles.map(toRawPom)
  }

}

case class RawPomFile(pomFile: File, document: Document, file: File) {
  val subfolder: String = if (pomFile.getParentFile == file) {
    file.toPath.toAbsolutePath.normalize().toFile.getAbsolutePath
  } else {
    pomFile.toPath.toAbsolutePath
      .getParent.normalize().toFile.getAbsolutePath
  }

  lazy val selfDep: Option[Dep] = PomMod.selfDep(_ => ())(document)

  lazy val parentDep: Option[Dep] = selfDep.flatMap(s => PomMod.parentDep(_ => ())(s, document))

}

object PomMod {

  def listProperties(opts: Opts, raws: Seq[RawPomFile], failureCollector: Option[Exception => Unit],
                     allPomsDocs: Seq[Document], listSelf: Seq[Dep]): Map[String, String] = {
    if (failureCollector.isDefined) {
      try {
        PomChecker.checkRootFirstChildPropertiesVar(opts, raws)
      } catch {
        case e: Exception => failureCollector.get.apply(e)
      }
    } else {
      PomChecker.checkRootFirstChildPropertiesVar(opts, raws)
    }

    val allPropsFromDocs = allPomsDocs.flatMap(PomMod.createPropertyMap)
    val result = allPropsFromDocs.foldLeft(Map.empty[String, String])(_ + _)
    val selfVersion = Util.only(listSelf.map(_.version).distinct, "version")

    replacedProperties(result, selfVersion)
  }

  @tailrec
  private def replaceWithLimit(result: Map[String, String], key: String, limit: Int = 4): String = {
    if (key.contains("$") && limit > 0) {
      replaceWithLimit(result, replaceProperty(result)(key), limit - 1)
    } else {
      key
    }

  }

  def replacedProperties(result: Map[String, String], selfVersion: Option[String]): Map[String, String] = {
    def skip(key: String)(p: (String, String)) = p.copy(_2 = p._2.replace(key, "skip"))

    @nowarn("msg=possible missing interpolator")
    val projectBasedirt = "${project.basedir}"
    @nowarn("msg=possible missing interpolator")
    val projectBuildFinalName = "${project.build.finalName}"
    @nowarn("msg=possible missing interpolator")
    val projectBuildDirectory = "${project.build.directory}"
    @nowarn("msg=possible missing interpolator")
    val mavenBuildTimestamp = "${maven.build.timestamp}"

    val mavenInternalVersion = Map("project.version" -> replaceWithLimit(result, selfVersion.get))
    val allProps = (result ++ mavenInternalVersion)
      .map(skip(projectBasedirt))
      .map(skip(projectBuildFinalName))
      .map(skip(projectBuildDirectory))
      .map(skip(mavenBuildTimestamp))
    val invalid = allProps.filter(_._2.contains("$"))
    if (invalid.nonEmpty) {
      throw new IllegalStateException("do not use properties in properties: " + invalid)
    }
    allProps
  }

  def checkSnapshots(v: Seq[String]) = {
    val typos = v.filter(s => s.toUpperCase().contains("SNAPSHOT")).filterNot(_.contains("SNAPSHOT"))
    if (typos.nonEmpty) {
      throw new PreconditionsException(s"invalid SNAPSHOT definition in »${typos.mkString(", ")}«. SNAPSHOT has to be in upper case.")
    }
  }

  def extractUrlsFromSettings(settingsXml: File): Seq[String] = {
    extractUrlsFromSettings(FileUtils.read(settingsXml))
  }

  def extractUrlsFromSettings(settingsXmlContent: String): Seq[String] = {
    try {
      val doc = Xpath.newDocument(settingsXmlContent)

      val mirrors = Xpath.toSeq(doc, "/settings/mirrors/mirror")
      val mirrorUrls = mirrors.flatMap(node => Xpath.nodeElementValue(node, "url"))

      val profiles = Xpath.toSeq(doc, "/settings/profiles/profile")
      val profileUrls = profiles.flatMap(node => {
        val repositories = Xpath.nodeElements(node, "repositories/repository")

        val plugins = Xpath.nodeElements(node, "pluginRepositories/pluginRepository")

        repositories.flatMap(node => Xpath.nodeElementValue(node, "url")) ++
          plugins.flatMap(node => Xpath.nodeElementValue(node, "url"))
      })

      (mirrorUrls ++ profileUrls).filterNot(_ == "http://0.0.0.0/").distinct
    } catch {
      case e: Exception => {
        Nil
      }
    }

  }

  private[release] def replacedPropertyOf(listProperties: Map[String, String], skipPropertyReplacement: Boolean)(string: String) = {
    PomMod.replaceProperty(listProperties, skipPropertyReplacement)(string)
  }

  private[release] def replacedVersionProperties(listProperties: Map[String, String], skipPropertyReplacement: Boolean)(deps: Seq[Dep]): Seq[Dep] = {
    def r(k: String) = replacedPropertyOf(listProperties, skipPropertyReplacement)(k)

    deps.map(dep => dep.copy(
      pomRef = dep.pomRef.copy(id = r(dep.pomRef.id),
        gav3 = dep.pomRef.gav3.copy(version = dep.pomRef.gav3.version.map(r))),
      version = dep.version.map(r),
      packaging = r(dep.packaging),
      typeN = r(dep.typeN),
      scope = r(dep.scope))
    ).map(in => {
      if (in.toString.contains("$") && !skipPropertyReplacement) {
        throw new IllegalStateException("missing var in " + in)
      }
      in
    })
  }

  def toPluginDep(file: File): Seq[PluginDep] = {
    try {
      val document = Xpath.documentOfFile(file)
      val nodes = Xpath.toSeq(document, "//extensions/extension")
      nodes.map(node => {
        val groupId = Xpath.nodeElementValue(node, "groupId")
        val artifactId = Xpath.nodeElementValue(node, "artifactId")
        val version = Xpath.nodeElementValue(node, "version")
        PluginDep(SelfRef.extensions,
          groupId.get, artifactId.get, Some(version.get), Nil, Nil)
      })
    } catch {
      case e: Exception => {
        e.printStackTrace()
        Nil
      }
    }

  }

  def selectFirstVersionFrom(raws: Seq[RawPomFile]): Option[String] = {
    val document = raws.head.document
    Xpath.onlyString(document, PomMod.xPathToProjectVersion)
  }

  def isVariable(in: String): Boolean = {
    in.matches("^\\$\\{.+\\}$")
  }

  def of(file: File, opts: Opts, failureCollector: Option[Exception => Unit]): PomMod = {
    lazy val repo = opts.newRepo
    withRepo(file, opts, repo, failureCollector = failureCollector)
  }

  def withRepoTry(file: File, opts: Opts, repo: RepoZ, skipPropertyReplacement: Boolean = false,
                  withSubPoms: Boolean = true, failureCollector: Option[Exception => Unit]): Try[PomMod] = {
    try {
      Success(withRepo(file, opts, repo, skipPropertyReplacement, withSubPoms, failureCollector))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  def withRepo(file: File, opts: Opts, repo: RepoZ, skipPropertyReplacement: Boolean = false,
               withSubPoms: Boolean = true, failureCollector: Option[Exception => Unit]): PomMod = {
    PomMod(file, repo, opts, skipPropertyReplacement, withSubPoms, failureCollector)
  }

  object DepTree {
    def parseGavsOnly(tree: DepTree): Seq[Gav3] = {
      DepTreeParsers.parseGavsOnly(tree.content)
    }
  }

  case class DepTree(content: String)

  private val xPathToProjectGroupId = "//project/groupId"
  private val xPathToProjectArtifactId = "//project/artifactId"
  private val xPathToProjectName = "//project/name"
  private val xPathToProjectVersion = "//project/version"
  private val xPathToProjectPackaging = "//project/packaging"

  private val xPathToProjectParentGroupId = "//project/parent/groupId"
  private val xPathToProjectParentArtifactId = "//project/parent/artifactId"
  private val xPathToProjectParentVersion = "//project/parent/version"
  private val xPathToProjectParentPackaging = "//project/parent/packaging"

  private val xPathToDependecies = "//dependencies/dependency"
  private val xPathToPluginInnerDependecies = "//plugin/dependencies/dependency"
  private val xPathToDependeciesScopeCompile = "//dependencies/dependency/scope[text() = 'compile']"
  private val xPathToDependeciesTypeJar = "//dependencies/dependency/type[text() = 'jar']"

  private[release] def replacedDepTreesVersion(entry: (File, PomMod.DepTree), groupId: String, artifactId: String, version: String, newVersion: String) = {
    entry._2.content.linesIterator
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):([^:]*):" + version, groupId + ":" + artifactId + ":$1:$2:" + newVersion))
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):" + version, groupId + ":" + artifactId + ":$1:" + newVersion))
      .map(_.replaceFirst("-SNAPSHOT-SNAPSHOT", "-SNAPSHOT"))
      .mkString("\n") + "\n"
  }

  def abbreviate(max: Int)(in: Seq[String]): Seq[String] = {
    if (in.length > max) {
      Seq(in.head, "..") ++ in.tail.drop(in.length - max)
    } else {
      in
    }
  }

  def unmanaged(emptyVersions: Seq[Gav], relevantGavs: Seq[Gav]): Seq[Gav] = {
    val value = emptyVersions.filterNot(_.version.isEmpty)
    if (value != Nil) {
      throw new PreconditionsException("invalid empty versions:\n" + value.map(_.formatted).mkString("\n"))
    } else {
      emptyVersions
        .filterNot(_ == Gav.empty) // e.g. poms with no parent
        // TODO scope erasure might be problematic
        .filterNot(gav => relevantGavs.exists(rGav => rGav.copy(version = None, scope = "") == gav.copy(scope = "")))
    }
  }

  def rootPom(file: File) = new File(file, "pom.xml")

  private def pathOf(value: Seq[Node]): Seq[String] = {
    val distinct = value.filter(_ != null).distinct
    val single: Node = Util.only(distinct, "more then one or none nodes in this group is unexpected")
    Xpath.nodePath(single)
  }

  private def depFrom(id: Gav3, dfn: Map[Dep, Node] => Unit)(depSeq: Seq[(String, String, Node)]): Dep = {
    val deps = Xpath.toMapOf(depSeq)
    val dep = Dep(pomRef = SelfRef.ofGav3(id),
      groupId = deps.getOrElse("groupId", ""),
      artifactId = deps.getOrElse("artifactId", ""),
      version = deps.get("version"),
      typeN = deps.getOrElse("type", ""),
      scope = deps.getOrElse("scope", ""),
      packaging = deps.getOrElse("packaging", ""),
      classifier = deps.getOrElse("classifier", ""),
      pomPath = pathOf(depSeq.map(_._3)),
    )
    val ma = depSeq.map(_._3).distinct

    dfn.apply(Map(dep -> Util.only(ma, "invalid dep creation")))
    dep
  }

  private[release] def allRawModulePomsFiles(rootFolders: Seq[File], withSubPoms: Boolean): Seq[File] = {

    def subModuleNames(document: Document): Seq[String] = {
      Xpath.toSeq(document, "//project/modules/module").map(_.getTextContent)
    }

    def isPomPacked(document: Document): Boolean = {
      Seq("pom") == Xpath.toSeq(document, PomMod.xPathToProjectPackaging).map(_.getTextContent)
    }

    rootFolders
      .flatMap(rootFolder => {
        val pomFile = if (rootFolder.isFile) {
          rootFolder.getParentFile
        } else {
          new File(rootFolder, "pom.xml")
        }
        val document: Document = Xpath.documentOfFile(pomFile.toPath.normalize().toFile)
        if (withSubPoms && isPomPacked(document)) {
          val subModules = subModuleNames(document)
          val s = subModules.par.map(subModuleName => {
            new File(pomFile.getParentFile, subModuleName).getAbsoluteFile
          }).seq
          pomFile +: allRawModulePomsFiles(s, withSubPoms)
        } else {
          Seq(pomFile)
        }
      }).map(resultFile => {
        val files = resultFile.isFile
        if (!files) {
          throw new IllegalStateException("is not a file: " + resultFile)
        } else {
          resultFile
        }
      })

  }

  private[release] def selfDep(dfn: Map[Dep, Node] => Unit)(document: Document): Option[Dep] = {
    val parentGroupid = Xpath.onlyString(document, PomMod.xPathToProjectParentGroupId)
    val groupId = Xpath.onlyString(document, PomMod.xPathToProjectGroupId)
    val artifactIdNode = Xpath.onlyStringNode(document, PomMod.xPathToProjectArtifactId)
    val packaging = Xpath.onlyString(document, PomMod.xPathToProjectPackaging)
    val parentVersion = Xpath.onlyString(document, PomMod.xPathToProjectParentVersion)
    val version = Xpath.onlyString(document, PomMod.xPathToProjectVersion)
    if (artifactIdNode.isDefined) {
      Some(depFrom(Gav3.opt(groupId.orElse(parentGroupid), artifactIdNode.map(_._1), version.orElse(parentVersion)), dfn)(Map(
        "groupId" -> groupId.orElse(parentGroupid).getOrElse(""),
        "artifactId" -> artifactIdNode.map(_._1).getOrElse(""),
        "packaging" -> packaging.getOrElse(""),
        "version" -> version.orElse(parentVersion).getOrElse("")).toSeq.map(t => (t._1, t._2, artifactIdNode.get._2))))
    } else {
      None
    }
  }

  private[release] def parentDep(dfn: Map[Dep, Node] => Unit)(dep: Dep, document: Document): Option[Dep] = {
    val groupid = Xpath.onlyString(document, PomMod.xPathToProjectParentGroupId)
    val artifactIdNode = Xpath.onlyStringNode(document, PomMod.xPathToProjectParentArtifactId)
    val packaging = Xpath.onlyString(document, PomMod.xPathToProjectParentPackaging)
    val parentVersion = Xpath.onlyString(document, PomMod.xPathToProjectParentVersion)
    if (artifactIdNode.isDefined) {
      val vT = if (parentVersion.isDefined) {
        Map("version" -> parentVersion.get)
      } else {
        Map.empty
      }
      val seq = (Map(
        "groupId" -> groupid.getOrElse(""),
        "artifactId" -> artifactIdNode.map(_._1).getOrElse(""),
        "packaging" -> packaging.getOrElse(""),
      ) ++ vT).toSeq
      val pDep = depFrom(Gav3(dep.groupId, dep.artifactId, dep.version), dfn)(seq
        .map(t => (t._1, t._2, artifactIdNode.map(_._2).get)))
      Some(pDep)
    } else {
      None
    }
  }

  def replaceProperty(props: Map[String, String], sloppy: Boolean = false)(input: String): String = {
    if (!sloppy && props.isEmpty) {
      throw new IllegalStateException("property map is empty")
    }

    def tryReplace(in: String, p: (String, String)): String = {
      try {
        Strings.nullToEmpty(in).replace("${" + p._1 + "}", p._2)
      } catch {
        case e: IllegalArgumentException => throw new IllegalStateException(e.getMessage + " in " + in)
      }
    }

    if (input == null) {
      null
    } else {
      val distinct = props.toList.map(p => tryReplace(input, p)).distinct
      val allReplacemdents = distinct
        .filterNot(_.contains("$"))
      if (sloppy) {
        val bracesCount = input.count(_ == '{') + input.count(_ == '}')
        if (Math.floorMod(bracesCount, 2) != 0) {
          throw new IllegalArgumentException("unbalanced curly braces: " + input)
        } else if (input.contains("${}")) {
          throw new IllegalArgumentException("empty curly braces: " + input)
        } else {
          if (distinct.isEmpty) {
            input
          } else if (allReplacemdents.size == 1) {
            Util.only(allReplacemdents, "should never happen")
          } else {
            Util.only(distinct.sortBy(_.count(_ == '$')).headOption.toList, "should not happen")
          }
        }

      } else {
        Util.only(allReplacemdents, "No property replacement found in pom.xmls for: \"" + input + "\" " +
          "- define properties where they are required and not in parent pom.xml")
      }
    }

  }

  private[release] def findPluginsByName(plugins: Seq[PluginDep], name: String) = {
    plugins
      .filterNot(_.pomPath.contains("pluginManagement"))
      .filter(_.artifactId == name)
  }

  private[release] def dependecyPlugins(plugins: Seq[PluginDep]) = findPluginsByName(plugins, "maven-dependency-plugin")

  def weekOfYear(localDate: LocalDate): Int = {
    val weekFields = WeekFields.of(Locale.getDefault())
    localDate.get(weekFields.weekOfWeekBasedYear())
  }

  def suggestReleaseBy(localDate: LocalDate, currentVersion: String, hasShopPom: Boolean,
                       branchNames: Seq[String], tagNames: Seq[String] = Nil,
                       nextVersion: () => String = () => "", increment: Option[Increment] = None): Seq[String] = {
    if (hasShopPom) {
      val releaseBranchNames = branchNames.filter(_.startsWith("release/")).map(_.replaceFirst("^release/", ""))
      val knownVersions: Seq[Version] = releaseBranchNames.map {
        case Version.shopPattern(pre, year, week, minor, low) => Version.fromString(pre, year, week, minor, low, "")
        case _ => Version.undef
      }

      if (currentVersion.startsWith("master") || currentVersion.startsWith("main")) {
        def dateBased(localDate: LocalDate, known: Seq[Version]): String = {

          Version("RC-", localDate.getYear, weekOfYear(localDate), 0, "", "")
            .nextIfKnown(knownVersions)
            .formatShopAsSnapshot()
        }

        Seq(dateBased(localDate, knownVersions),
          dateBased(localDate.plusWeeks(1), knownVersions),
          dateBased(localDate.plusWeeks(2), knownVersions))
      } else if (currentVersion.matches("^[0-9]+x-SNAPSHOT$")) {
        val withoutSnapshot = currentVersion.replaceFirst("x-SNAPSHOT$", "") + ".0.0"

        val versionTuples = branchNames.map(_.replace("release/", "")).flatMap(Version.toVersion)

        @tailrec
        def nextNumberedReleaseIfExisting(known: Seq[Version], names: Version*): Seq[Version] = {

          if (known.exists(in => names.contains(in))) {
            val latest = known.max
            val nextMinor = latest.copy(minor = latest.minor + 1, patch = 0)
            val nextPatch = latest.copy(patch = latest.patch + 1)
            nextNumberedReleaseIfExisting(known, nextPatch, nextMinor)
          } else {
            names.toList
          }
        }

        nextNumberedReleaseIfExisting(versionTuples, Version.toVersion(withoutSnapshot).get).map(_.formatAsSnapshot())
      } else {
        val unSnapshoted = Version.removeTrailingSnapshots(currentVersion)
        unSnapshoted match {
          case Version.shopPatternSloppy(pre, year, week, minor, low) => {
            val version = Version.fromString(pre, year, week, minor, low, "")
            if (knownVersions.contains(version)) {
              Seq(version.nextIfKnown(knownVersions).formatShopAsSnapshot())
            } else {
              Seq(version.formatShopAsSnapshot())
            }
          }
          case _ => Seq(currentVersion) // release a feature branch
        }
      }
    } else {
      val withoutSnapshot = Version.removeTrailingSnapshots(currentVersion)
      val tagNamesI = Sgit.stripVersionPrefix(tagNames).flatMap(Version.toVersion)
      if (tagNamesI.nonEmpty && Version.toVersion(withoutSnapshot).isEmpty) {
        val versions = tagNamesI
        val byMajor = versions.groupBy(_.major)
        val firstP = "[1-9][0-9]*".r
        val fM = firstP.findFirstIn(withoutSnapshot).flatMap(_.toIntOption)
        val series = fM.flatMap(byMajor.get)
        if (series.isDefined) {
          val matchingSeries = series.get
          val m = matchingSeries.max
          val patchV = m.nextVersionResetZero((0, 0, 1)).format()
          val minorV = m.nextVersionResetZero((0, 1, 0)).format()
          val majorV = m.nextVersionResetZero((1, 0, 0)).format()
          val suggested = increment match {
            case Increment.major => Seq(majorV)
            case Increment.minor => Seq(minorV)
            case Increment.patch => Seq(patchV)
            case _ => Seq(majorV, minorV, patchV)
          }
          val maybeNext = suggested.filter(_ == nextVersion.apply())
          if (maybeNext.nonEmpty) {
            maybeNext
          } else {
            suggested
          }
        } else {
          Seq(withoutSnapshot)
        }
      } else {
        val sug = Version.parseSloppy(withoutSnapshot)
        val maybeVersion = tagNamesI.find(v => sug == v)
        if (maybeVersion.isDefined) {
          Seq(maybeVersion.get.nextIfKnown(tagNamesI).format())
        } else {
          Seq(withoutSnapshot)
        }

      }
    }
  }

  private val undef = "-UNDEF"

  def isUnknownVersionPattern(in: String): Boolean = {
    suggestNextReleaseBy(in, in, sloppyShop = false).endsWith(undef)
  }

  def trySuggestKnownPattern(in: String): Try[String] = {
    try {
      val str = suggestKnownPattern(in)
      if (str.isBlank) {
        Failure(new IllegalStateException("suggestion is blank"))
      } else {
        Success(str)
      }

    } catch {
      case e: Exception => Failure(e)
    }
  }

  def suggestKnownPattern(in: String): String = {

    def formatAlt(alts: Seq[String]): String = {
      "Maybe one of the following versions is what you want: " + checkedAlts(alts)
        .map(l => s"'${l}'")
        .mkString(", ") + "\n"
    }

    def checkedAlts(alts: Seq[String]): Seq[String] = {
      alts.map(t => {
        if (isUnknownVersionPattern(t)) {
          throw new IllegalStateException("invalid inner creation " + t)
        } else {
          t
        }

      })
    }

    if (isUnknownVersionPattern(in)) {
      if (in.toLowerCase().startsWith("rc")) {
        val num = in.split("[^0-9]").flatMap(_.toIntOption)
        val alternatives = if (num.length >= 3) {
          if (num(2) == 0) {
            Seq(
              s"RC-${num(0)}.${"%02d".format(num(1))}"
            )
          } else {
            Seq(
              s"RC-${num(0)}.${"%02d".format(num(1))}.${num(2)}"
            )
          }

        } else {
          throw new IllegalArgumentException("unexpected num")
        }

        formatAlt(alternatives)
      } else {
        val num = in.split("[^0-9]").flatMap(_.toIntOption)
        if (num.size == 4) {
          val alternatives = Seq(
            s"${num(0)}.${num(1)}.${num(2)}_${num(3)}",
            s"${num(0)}.${num(1)}.${num(2)}-${num(3)}")
          formatAlt(alternatives)
        } else {
          ""
        }
      }
    } else {
      ""
    }
  }

  def suggestNextReleaseBy(currentVersionIn: String, releaseVersion: String, sloppyShop: Boolean = true): String = {
    val currentVersion = Strings.nullToEmpty(currentVersionIn)
    if (currentVersion == "master-SNAPSHOT" || currentVersion == "master") {
      "master"
    } else if (currentVersion == "main-SNAPSHOT" || currentVersion == "main") {
      "main"
    } else if (currentVersion.matches("^[0-9]+x-SNAPSHOT$")) {
      Version.removeTrailingSnapshots(currentVersion)
    } else if (currentVersion.matches("^[0-9]+x$")) {
      currentVersion
    } else {
      val withoutSnapshot = Version.removeTrailingSnapshots(releaseVersion)
      if (withoutSnapshot.count(_ == '.') > 2) {
        releaseVersion + undef
      } else {
        withoutSnapshot match {
          case Version.stableShop(pre) => pre + "-stable-RELEASE-DEMO-DELETE-ME"
          case Version.semverPatternRCEnd(ma, mi, b, _) => ma + "." + mi + "." + b
          case Version.semverPatternLetterEnd(ma, mi, b, pre) => ma + "." + mi + "." + (b.toInt + 1) + "-" + pre
          case Version.semverPatternLowdash(ma, mi, b, low) => ma + "." + mi + "." + b + "_" + (low.toInt + 1)
          case Version.semverPatternLowdashString(ma, mi, b, low) => ma + "." + mi + "." + (b.toInt + 1) + "_" + low
          case Version.semverPattern(ma, mi, b) => ma + "." + mi + "." + (b.toInt + 1)
          case Version.semverPatternNoBugfix(ma, mi) => ma + "." + (mi.toInt + 1) + ".0"
          case Version.semverPatternNoMinor(ma) => s"${ma.toInt + 1}.0.0"
          case Version.shopPatternSloppy(pre, year, week, minor, low) if sloppyShop => {
            Version.removeTrailingSnapshots(currentVersion) match {
              case Version.shopPatternSloppy(_, _, _, _, _) => {
                val verso = Version.fromString(pre, year, week, minor, low, "").plusWeek()
                verso.copy(patch = 0, low = "").formatShop()
              }
              case any => {
                Version.removeTrailingSnapshots(any)
              }
            }
          }
          case Version.shopPattern(pre, year, week, minor, low) if !sloppyShop => {
            Version.removeTrailingSnapshots(currentVersion) match {
              case Version.shopPattern(_, _, _, _, _) => {
                val verso = Version.fromString(pre, year, week, minor, low, "").plusWeek()
                verso.copy(patch = 0, low = "").formatShop()
              }
              case any => {
                Version.removeTrailingSnapshots(any)
              }
            }
          }
          case any => any + undef
        }

      }
    }
  }

  private def gavFind(gavElements: Seq[Node], key: String): Option[Node] = {
    gavElements.find(_.getNodeName == key)
  }

  private def filterBy(doc: Document, sGroupId: String, sArtifactId: String, sVersionId: Option[String]): Seq[Node] = {
    (Xpath.toSeq(doc, "//plugins/plugin") ++ Xpath.toSeq(doc, xPathToDependecies)).filter(node => {

      val gavElements = Xpath.toSeqNodes(node.getChildNodes)

      def gavMatchX(gavElements: Seq[Node])(key: String, value: String): Boolean = {
        val found = gavFind(gavElements, key)
        found.exists(_.getTextContent == value)
      }

      def gavMatch(key: String, value: String) = gavMatchX(gavElements)(key, value)

      if (sVersionId.isDefined) {
        gavMatch("groupId", sGroupId) && gavMatch("artifactId", sArtifactId) && gavMatch("version", sVersionId.get)
      } else {
        gavMatch("groupId", sGroupId) && gavMatch("artifactId", sArtifactId)
      }
    })
  }

  def formatDependency(in: String, selectedGroupId: String, selectedArtifcatId: String, selectedVersion: String, newVersion: String): String = {
    val doc = Xpath.newDocument(in)
    val filtered = filterBy(doc, selectedGroupId, selectedArtifcatId, Some(selectedVersion))
    filtered.headOption.foreach(node => {
      val gavElements = Xpath.toSeqNodes(node.getChildNodes)
      val versionNode: Node = gavFind(gavElements, "version").head
      versionNode.setTextContent(newVersion)
    })

    toString(doc)
  }

  private[release] def writePom(targetFolder: File)(file: File, document: Document): Unit = {
    PomMod.writeContent(targetFolder)(file, PomMod.toString(document) + "\n")
  }

  private[release] def toString(doc: Document): String = {
    val transformerFactory = TransformerFactory.newInstance()
    val transformer = transformerFactory.newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    val baos = new ByteArrayOutputStream()
    transformer.transform(new DOMSource(doc), new StreamResult(baos))
    baos.toString(StandardCharsets.UTF_8.displayName()).replace("\r", "")
      .replaceAll("/>", " />").replaceFirst("[ ]+xsi:schemaLocation", "\n  xsi:schemaLocation")
  }

  private[release] def applyVersionTo(raw: RawPomFile, selfs: Seq[Dep], oldValue: String, newValue: String): Unit = {
    applyToKey(raw, selfs, "version", Some(oldValue), _ => newValue)
  }

  private[release] def applyVersionTo(raw: RawPomFile, selfs: Seq[Dep], newValue: String): Unit = {
    applyToKey(raw, selfs, "version", None, _ => newValue)
  }

  private def applyToGroupAndArtifactId(raw: RawPomFile, selfs: Seq[Dep], groupidFn: String => String, artifactIdFn: String => String): Unit = {

    val ga = selfs.map(x => (x.groupId, x.artifactId))

    def childNodesByExternalKey(depNode: Node): Seq[Node] = {
      Xpath.toSeqNodes(depNode.getChildNodes)
        .filter(u => (u.getNodeName == "groupId" || u.getNodeName == "artifactId") && !u.getTextContent.isEmpty)
    }

    val tt = ga.flatMap(in => filterBy(raw.document, in._1, in._2, None))
    val result = tt
      .map(childNodesByExternalKey)
      .filter(_.nonEmpty)
      .flatten

    if (result.nonEmpty) {
      result.foreach(in => {
        if (in.getNodeName == "groupId") {
          in.setTextContent(groupidFn.apply(in.getTextContent))
        } else if (in.getNodeName == "artifactId") {
          in.setTextContent(artifactIdFn.apply(in.getTextContent))
        } else {
          throw new IllegalStateException("bad node name " + in.getNodeName)
        }
      })
    }

  }

  private def applyToKey(raw: RawPomFile, selfs: Seq[Dep], key: String, oldKey: Option[String], newValueFn: String => String): Unit = {

    val ga = selfs.map(x => (x.groupId, x.artifactId))

    def childNodesByExternalKey(depNode: Node): Seq[Node] = {
      Xpath.toSeqNodes(depNode.getChildNodes)
        .filter(u => u.getNodeName == key && u.getTextContent.nonEmpty)
    }

    val tt = ga.flatMap(in => filterBy(raw.document, in._1, in._2, None))
    val result = tt
      .map(childNodesByExternalKey)
      .filter(_.nonEmpty)
      .flatten

    if (result.nonEmpty) {
      result.foreach(in => {
        if (oldKey.isDefined) {
          if (oldKey.get == in.getTextContent || isVariable(in.getTextContent)) {
            in.setTextContent(newValueFn.apply(in.getTextContent))
          }
        } else {
          in.setTextContent(newValueFn.apply(in.getTextContent))
        }

      })
    }

  }

  def modifyValueOfXpathTo(raw: RawPomFile, xpath: String, newValueFn: String => String): Unit = {
    val xPathResult = Xpath.toSeq(raw.document, xpath)
    val node = xPathResult.headOption
    if (node.isDefined) {
      node.get.setTextContent(newValueFn.apply(node.get.getTextContent))
    }
  }

  def applyValueOfXpathTo(raw: RawPomFile, xpath: String, oldValue: String, newValue: String): Unit = {
    val xPathResult = Xpath.toSeq(raw.document, xpath)
    val node = xPathResult.headOption
    if (node.isDefined && (node.get.getTextContent == oldValue || isVariable(node.get.getTextContent))) {
      node.get.setTextContent(newValue)
    }
  }

  def applyValueOfXpathTo(raw: RawPomFile, xpath: String, newValue: String): Unit = {
    val xPathResult = Xpath.toSeq(raw.document, xpath)
    val node = xPathResult.headOption
    if (node.isDefined) {
      node.get.setTextContent(newValue)
    }
  }

  private[release] def stripDependencyDefaults(doc: Document): Document = {

    val toRemove = Xpath.toSeq(doc, xPathToDependeciesScopeCompile) ++ Xpath.toSeq(doc, xPathToDependeciesTypeJar)
    toRemove.foreach(in => {
      val comments = Xpath.nodeComments(in.getParentNode, "comment()")
        .map(n => n.getTextContent.trim)
      if (!comments.contains("@release:keep-compile-scope")) {
        // XXX compile scope allows transitiv dependencies, and we use this to create test dependency _bundles_
        if (in.getNextSibling.getTextContent.blank()) {
          in.getParentNode.removeChild(in.getNextSibling)
        }
        in.getParentNode.removeChild(in)
      }
    })

    doc
  }

  private[release] def createPropertyMap(pomDoc: Document): Map[String, String] = {
    val tuples = Xpath.toSeqTuples(pomDoc, "//project/properties")
    val pro: Seq[(String, String)] = tuples.flatten.map(t => (t._1, t._2))
    val result = pro.foldLeft(Map.empty[String, String])(_ + _)
    val diff = Util.symmetricDiff(result.toSeq, pro)
    if (diff.nonEmpty) {
      throw new IllegalStateException("invalid property definition / multiple: " + diff)
    }
    result
  }

  def checkNoSlashesNotEmptyNoZeros(s: String): String = {
    if (s.contains("/") || s.contains("\\")) {
      throw new IllegalArgumentException("no slashes are allowed in \"" + s + "\"")
    } else if (s.trim.isEmpty) {
      throw new IllegalArgumentException("empty is not allowed")
    } else if (s.trim.startsWith("00") || s.trim == "0" || s.trim.replaceAll("[,.;:-]+", "").replaceAll("[0]+", "0") == "0") {
      throw new IllegalArgumentException("no leading zeros are allowed in \"" + s + "\"")
    } else if (s.trim.matches(".*[,.;:-]0[0]+$")) {
      throw new IllegalArgumentException("no trailing zeros are allowed in \"" + s + "\"")
    } else {
      s
    }
  }

  private[release] def writeContent(parent: File)(file: File, text: String): Unit = {
    if (!file.toPath.toAbsolutePath.normalize().startsWith(parent.toPath.toAbsolutePath().normalize())) {
      throw new IllegalStateException(s"${file.getAbsolutePath} must start with ${parent.getAbsolutePath}")
    }
    FileUtils.handleWindowsFilesystem { _ =>
      if (Files.exists(file.toPath)) {
        Files.delete(file.toPath)
      }
      if (!Files.isDirectory(file.toPath.getParent)) {
        Files.createDirectories(file.toPath.getParent)
      }
      Files.write(file.toPath, text.getBytes(StandardCharsets.UTF_8))
    }
  }

  def format(in: String, xPathStr: String, value: String): String = {
    val raw = RawPomFile(new File("."), Xpath.newDocument(in), new File("."))
    applyValueOfXpathTo(raw, xPathStr, value)
    toString(raw.document)
  }

}
