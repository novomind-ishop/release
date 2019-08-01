package release

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.LocalDate
import java.time.temporal.WeekFields
import java.util.Locale

import com.typesafe.scalalogging.LazyLogging
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, TransformerFactory}
import org.w3c.dom.{Document, Node}
import release.Conf.Tracer
import release.PomChecker.ValidationException
import release.PomMod._
import release.ProjectMod._
import release.Starter.{Opts, PreconditionsException}
import release.Util.pluralize

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

case class PomMod(file: File, aether: Aether, opts: Opts) extends ProjectMod with LazyLogging {
  logger.trace("init pomMod")
  private var depMap: Map[Dep, Node] = Map.empty

  private val rootPom = PomMod.rootPom(file)
  if (!rootPom.canRead) {
    throw new PreconditionsException(file.toString + " seems to be no maven project")
  }
  private val allRawPomFiles = Tracer.msgAround("read all poms", logger, () => allRawModulePomsFiles(Seq(file)))

  private val raws: Seq[RawPomFile] = toRawPoms(allRawPomFiles)
  private[release] val allPomsDocs: Seq[Document] = raws.map(_.document).toList

  case class DepTree(content: String)

  private def depU(d: Map[Dep, Node]): Unit = {
    depMap = depMap ++ d
  }

  private[release] val listSelf: Seq[Dep] = {
    allPomsDocs.map(PomMod.selfDep(depU))
  }

  private val rootPomGav: Seq[Gav] = selfDepsMod.map(_.gav().copy(packageing = "")).distinct

  val selfVersion: String = {
    val v = listSelf.map(_.version).distinct

    Util.only(v, {
      val selection: String = listSelf.groupBy(_.version) match {
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

  private val currentVersion: Option[String] = {
    Xpath.onlyString(Xpath.pomDoc(rootPom), PomMod.xPathToProjectVersion)
  }

  private def checkRootFirstChildProperties(): Unit = {
    PomMod.checkRootFirstChildPropertiesVar(opts, raws)
  }

  val listProperties: Map[String, String] = {
    checkRootFirstChildProperties()
    val allPropsFromDocs = allPomsDocs.flatMap(PomMod.createPropertyMap)
    val result = allPropsFromDocs.foldLeft(Map.empty[String, String])(_ + _)
    val selfVersion = Util.only(listSelf.map(_.version).distinct, "version")

    def skip(key: String)(p: (String, String)) = p.copy(_2 = p._2.replace(key, "skip"))

    val allProps = (result ++ Map("project.version" -> selfVersion))
      .map(skip("${project.basedir}"))
      .map(skip("${project.build.finalName}"))
      .map(skip("${project.build.directory}"))
      .map(skip("${maven.build.timestamp}"))
    val invalid = allProps.filter(_._2.contains("$"))
    if (invalid.nonEmpty) {
      throw new IllegalStateException("do not use properties in properties: " + invalid)
    }
    allProps
  }

  val listDependecies: Seq[Dep] = {
    replacedVersionProperties(allPomsDocs.flatMap(deps)).distinct
  }

  val listPluginDependencies: Seq[PluginDep] = {
    val allP: Seq[PluginDep] = allPomsDocs.map(in => {
      val gav = PomMod.selfDep(depU)(in).gavWithDetailsFormatted
      val nodes = Xpath.toSeqTuples(in, "//plugins/plugin")
      (nodes, gav)
    }).flatMap(gavNode => {
      val oo: Seq[Seq[(String, String, Node)]] = gavNode._1
      val x = gavNode._2
      oo.map(pluginDepFrom(x))
    })
    allP.toList
  }

  private[release] val mavenDependencyPlugins: Seq[PluginDep] = {
    PomMod.dependecyPlugins(listPluginDependencies)
  }

  private[release] var depTreeFileContents: Map[File, DepTree] = depTreeFiles()
    .map(f => (f, DepTree(Util.read(f))))
    .filterNot(in => in._1.getParentFile.getName == ".")
    .foldLeft(Map.empty[File, DepTree])(_ + _)

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
    val version = deps.getOrElse("version", "")

    val execNodes = Xpath.nodeElements(node, "executions/execution")
    val execs = execNodes.map(in => {
      val id = Xpath.nodeElementValue(in, "id").getOrElse("")
      val phase = Xpath.nodeElementValue(in, "phase").getOrElse("")
      val goals = Xpath.nodeElements(in, "goals/goal").map(_.getFirstChild.getTextContent)

      val configNodes = Xpath.nodeElementMap(in, "configuration")
      PluginExec(id, goals, phase, configNodes)
    })

    replacedVersionProperty(PluginDep(PomRef(id), groupId, artifactId, version, execs, nodePath(node)))
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
    depTreeFileContents = depTreeFileContents.toList.map(entry => {
      (entry._1, entry._2.copy(PomMod.replacedDepTreesVersion(entry._2.content, groupId, artifactId, version, newVersion)))
    }).foldLeft(Map.empty[File, DepTree])(_ + _)
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

      parentVersion ++ projectVersion ++ listSelf.map(_.version)
    }).distinct
    Util.only(out, "only one Version is allowed")
  }

  def changeShopGroupArtifact(newValue: String): Unit = {
    val pattern = "[a-z0-9]+"
    if (!newValue.matches(pattern)) {
      throw new IllegalArgumentException("invalid groupidArtifactName \"" + newValue + "\"; must match " + pattern)
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
      changeDepTreesGA(entry.groupId, entry.artifactId, entry.version, _ => "com.novomind.ishop.shops." + newValue, replaceInArtifactId)
    })
  }

  def changeVersion(newVersion: String): Unit = {
    raws.foreach(d => {
      if (d.pomFile.getParentFile == file) {
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectVersion, newVersion)
        PomMod.applyVersionTo(d, listSelf, newVersion)
      } else {
        if (rootPomGav.contains(d.parentDep.gav())) {
          PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectParentVersion, newVersion)
        }
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectVersion, newVersion)
        PomMod.applyVersionTo(d, listSelf, newVersion)
      }
    })
    selfDepsMod.foreach(entry => {
      changeDepTreesVersion(entry.groupId, entry.artifactId, entry.version, newVersion)
    })

  }

  def depTreeFilenames(): Seq[String] = {
    depTreeFiles().map(f => {
      if (f.getParentFile.getName != file.getName) {
        val prefix = f.getParentFile.getName + "/" match {
          case "./" => ""
          case other => other
        }
        prefix + f.getName
      } else {
        f.getName
      }
    }).distinct
  }

  def depTreeFiles(): Seq[File] = {
    depTreeFilename().map(f => Seq(f) ++ raws.map(_.subfolder + "/" + f))
      .map(in => in.map(f => new File(file, f).getAbsoluteFile).filter(_.exists()))
      .getOrElse(Nil)
  }

  def depTreeFilenameList(): Seq[String] = {
    depTreeFilename().map(Seq(_)).getOrElse(Nil)
  }

  def depTreeFilename(): Option[String] = {
    val depPluginConfigs: Seq[(String, String)] = mavenDependencyPluginConfigsByGoal("tree")
    if (depPluginConfigs != Nil) {
      val treeOutputFileName: String = Util.only(depPluginConfigs.filter(_._1 == "outputFile"), "not only tree file")._2
      Some(treeOutputFileName)
    } else {
      None
    }
  }

  def findNodes(groupId: String, artifactId: String, version: String): Seq[Node] = {
    depMap.toList.filter(dep => replacedPropertyOf(dep._1.artifactId) == artifactId).map(_._2).filter(_ != null)
  }

  def writeTo(targetFolder: File): Unit = {
    raws.par.foreach(sub => {
      val path = file.toPath.relativize(sub.pomFile.toPath)
      PomMod.writePom(targetFolder.toPath.resolve(path).toFile, sub.document)
    })

    depTreeFileContents
      .foreach(fe => {
        val content = fe._2.content
        if (fe._1.getParentFile.getName == file.getName) {
          PomMod.writeContent(new File(targetFolder, fe._1.getName), content)
        } else {
          PomMod.writeContent(new File(new File(targetFolder, fe._1.getParentFile.getName), fe._1.getName), content)
        }
      })
  }

  def selfDepsMod: Seq[Dep] = {
    val selfDeps: Seq[Dep] = listSelf ++
      listSelf.map(_.copy(scope = "test")) ++
      listSelf.map(_.copy(scope = "test", classifier = "tests")) ++
      listSelf.map(_.copy(scope = "test", classifier = "tests", packaging = "")) ++
      listSelf.map(_.copy(scope = "test", packaging = "")) ++
      listSelf.map(_.copy(classifier = "tests")) ++
      listSelf.map(_.copy(classifier = "sources")) ++ // for bo-client
      listSelf.map(_.copy(classifier = "", typeN = "war", scope = "runtime", packaging = "")) ++ // for bo-client
      listSelf.map(_.copy(packaging = ""))
    val pomMods = selfDeps.map(_.copy(typeN = "pom"))
    val pomModsImport = pomMods.map(_.copy(scope = "import"))
    (pomMods ++ pomModsImport ++ selfDeps)
      .map(_.copy(pomRef = PomRef.undef))
      .distinct
      .sortBy(_.toString)
  }

  def isShop: Boolean = {
    val filtered = listSelf.filterNot(_.packaging == "pom")
    filtered.map(_.groupId).contains("com.novomind.ishop.shops")
  }


  private def replacedVersionProperty(dep: PluginDep) = dep.copy(version = replacedPropertyOf(dep.version))

  def listSnapshotsDistinct: Seq[Dep] = {
    Util.distinctOn[Dep, Dep](listSnapshots, _.copy(pomRef = PomRef.undef))
  }

  def listSnapshots: Seq[Dep] = {
    val deps = listDependecies
    val selfMods = selfDepsMod
    val filteredDeps = deps.filterNot(dep => {
      val mod = dep.copy(pomRef = PomRef.undef)
      //      if (mod.toString.contains("runtime")) {
      //        println("chech dep: " + mod)
      //        println("inn " + selfMods.filter(_.toString.contains("runtime")))
      //      }
      selfMods.contains(mod)
    }

    )

    val replacedParams = replacedVersionProperties(filteredDeps)
    val onlySnapshots = replacedParams.filter(_.version.contains("SNAPSHOT"))
    onlySnapshots
  }

  private def deps(document: Document): Seq[Dep] = {
    val self: Dep = PomMod.selfDep(depU)(document)
    val xmlNodes = Xpath.toSeqTuples(document, xPathToDependecies)
    PomMod.parentDep(depU)(self, document) +: xmlNodes.filter(_.nonEmpty).map(PomMod.depFrom(self.pomRef.id, depU))
  }

  private def checkCurrentVersion(current: Option[String]): Unit = {
    if (current.isEmpty) {
      throw new IllegalStateException(file.getName + " as no version, please define")
    }
  }

  def suggestReleaseVersion(branchNames: Seq[String] = Nil): Seq[String] = {
    checkCurrentVersion(currentVersion)
    PomMod.suggestReleaseBy(LocalDate.now(), currentVersion.get, isShop, branchNames)
  }

  def suggestNextRelease(releaseVersion: String): String = {
    checkCurrentVersion(currentVersion)
    PomMod.suggestNextReleaseBy(currentVersion.get, releaseVersion)
  }

  private def toRawPom(pomFile: File): RawPomFile = {
    // TODO check if pom is sub sub module - parse
    RawPomFile(pomFile, PomMod.stripDependencyDefaults(Xpath.pomDoc(pomFile)), file)
  }

  private def toRawPoms(pomFiles: Seq[File]): Seq[RawPomFile] = {
    pomFiles.map(toRawPom)
  }

}

case class RawPomFile(pomFile: File, document: Document, file: File) {
  val subfolder: String = if (pomFile.getParentFile == file) {
    "."
  } else {
    pomFile.getAbsoluteFile.getParentFile.getName
  }

  lazy val selfDep: Dep = PomMod.selfDep(_ => ())(document)

  lazy val parentDep: Dep = PomMod.parentDep(_ => ())(selfDep, document)

}

object PomMod {

  def of(file: File, unnused: PrintStream, opts: Opts): PomMod = {
    lazy val aether = new Aether(opts)
    ofAether(file, opts, aether)
  }

  def ofAetherForTests(file: File, aether: Aether): PomMod = {
    PomMod(file, aether, Opts())
  }

  def ofAether(file: File, opts: Opts, aether: Aether): PomMod = {
    PomMod(file, aether, opts)
  }

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
  private val xPathToDependeciesScopeCompile = "//dependencies/dependency/scope[text() = 'compile']"
  private val xPathToDependeciesTypeJar = "//dependencies/dependency/type[text() = 'jar']"

  private[release] def replacedDepTreesVersion(in: String, groupId: String, artifactId: String, version: String, newVersion: String) = {
    in.linesIterator
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

  def unmanged(emptyVersions: Seq[Gav], relevantGavs: Seq[Gav]): Seq[Gav] = {
    if (emptyVersions.filterNot(_.version.isEmpty) != Nil) {
      throw new IllegalArgumentException("invalid empty versions")
    } else {
      emptyVersions
        .filterNot(_ == Gav.empty) // e.g. poms with no parent
        // TODO scope erasure might be problematic
        .filterNot(gav => relevantGavs.exists(rGav => rGav.copy(version = "", scope = "") == gav.copy(scope = "")))
    }
  }

  def rootPom(file: File) = new File(file, "pom.xml")

  private def depFrom(id: String, dfn: Map[Dep, Node] => Unit)(depSeq: Seq[(String, String, Node)]): Dep = {
    val deps = Xpath.toMapOf(depSeq)
    val dep = Dep(pomRef = PomRef(id),
      groupId = deps.getOrElse("groupId", ""),
      artifactId = deps.getOrElse("artifactId", ""),
      version = deps.getOrElse("version", ""),
      typeN = deps.getOrElse("type", ""),
      scope = deps.getOrElse("scope", ""),
      packaging = deps.getOrElse("packaging", ""),
      classifier = deps.getOrElse("classifier", ""))
    val ma = depSeq.map(_._3).distinct

    dfn.apply(Map(dep -> Util.only(ma, "invalid dep creation")))
    dep
  }

  private[release] def allRawModulePomsFiles(rootFolders: Seq[File]): Seq[File] = {

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
        val document: Document = Xpath.pomDoc(pomFile)
        if (isPomPacked(document)) {
          val subModules = subModuleNames(document)
          val s = subModules.par.map(subModuleName => {
            new File(pomFile.getParentFile, subModuleName).getAbsoluteFile
          }).seq
          pomFile +: allRawModulePomsFiles(s)
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

  private[release] def selfDep(dfn: Map[Dep, Node] => Unit)(document: Document): Dep = {
    val parentGroupid = Xpath.onlyString(document, PomMod.xPathToProjectParentGroupId)
    val groupid = Xpath.onlyString(document, PomMod.xPathToProjectGroupId)
    val artifactId = Xpath.onlyString(document, PomMod.xPathToProjectArtifactId)
    val packaging = Xpath.onlyString(document, PomMod.xPathToProjectPackaging)
    val parentVersion = Xpath.onlyString(document, PomMod.xPathToProjectParentVersion)
    val version = Xpath.onlyString(document, PomMod.xPathToProjectVersion)
    val id = Seq(groupid, artifactId, version).flatten.mkString(":")
    depFrom(id, dfn)(Map(
      "groupId" -> groupid.orElse(parentGroupid).getOrElse(""),
      "artifactId" -> artifactId.getOrElse(""),
      "packaging" -> packaging.getOrElse(""),
      "version" -> version.orElse(parentVersion).getOrElse("")).toSeq.map(t => (t._1, t._2, null)))
  }

  private[release] def parentDep(dfn: Map[Dep, Node] => Unit)(dep: Dep, document: Document): Dep = {
    val groupid = Xpath.onlyString(document, PomMod.xPathToProjectParentGroupId)
    val artifactId = Xpath.onlyString(document, PomMod.xPathToProjectParentArtifactId)
    val packaging = Xpath.onlyString(document, PomMod.xPathToProjectParentPackaging)
    val parentVersion = Xpath.onlyString(document, PomMod.xPathToProjectParentVersion)
    val id = Seq(dep.groupId, dep.artifactId, dep.version).mkString(":")
    depFrom(id, dfn)(Map(
      "groupId" -> groupid.getOrElse(""),
      "artifactId" -> artifactId.getOrElse(""),
      "packaging" -> packaging.getOrElse(""),
      "version" -> parentVersion.getOrElse("")).toSeq.map(t => (t._1, t._2, null)))
  }

  private[release] def replaceProperty(props: Map[String, String])(input: String) = {
    if (props.isEmpty) {
      throw new IllegalStateException("property map is empty")
    }

    def tryReplace(in: String, p: (String, String)): String = {
      try {
        in.replace("${" + p._1 + "}", p._2)
      } catch {
        case e: IllegalArgumentException => throw new IllegalStateException(e.getMessage + " in " + in)
      }
    }

    val allReplacemdents = props.toList.map(p => tryReplace(input, p)).toSeq
      .distinct
      .filterNot(_.contains("$"))
    Util.only(allReplacemdents, "No property replacement found in pom.xmls for: \"" + input + "\" " +
      "- define properties where they are required and not in parent pom.xml")
  }

  private[release] def findPluginsByName(plugins: Seq[PluginDep], name: String) = {
    plugins
      .filterNot(_.pomPath.contains("pluginManagement"))
      .filter(_.artifactId == name)
  }

  private[release] def dependecyPlugins(plugins: Seq[PluginDep]) = findPluginsByName(plugins, "maven-dependency-plugin")

  def suggestReleaseBy(localDate: LocalDate, currentVersion: String, hasShopPom: Boolean,
                       branchNames: Seq[String]): Seq[String] = {
    if (hasShopPom) {
      val releaseBranchNames = branchNames.filter(_.startsWith("release/")).map(_.replaceFirst("^release/", ""))
      val knownVersions: Seq[Version] = releaseBranchNames.map {
        case Version.shopPattern(pre, year, week, minor, low) => Version.fromString(pre, year, week, minor, low)
        case _ => Version.undef
      }

      if (currentVersion.startsWith("master")) {
        def dateBased(localDate: LocalDate, known: Seq[Version]): String = {
          val weekFields = WeekFields.of(Locale.getDefault())
          Version("RC-", localDate.getYear, localDate.get(weekFields.weekOfWeekBasedYear()), 0, "")
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
        val unSnapshoted = currentVersion.replaceFirst("-SNAPSHOT", "")
        unSnapshoted match {
          case Version.shopPattern(pre, year, week, minor, low) => {
            val version = Version.fromString(pre, year, week, minor, low)
            if (knownVersions.contains(version)) {
              Seq(version.nextIfKnown(knownVersions).formatShopAsSnapshot())
            } else {
              Seq(currentVersion)
            }
          }
          case any => Seq(currentVersion) // release a feature branch
        }
      }
    } else {
      Seq(currentVersion.replaceFirst("-SNAPSHOT$", ""))
    }
  }

  def isUnknownReleasePattern(in: String): Boolean = {
    suggestNextReleaseBy(in, in).endsWith("-UNDEF")
  }

  def suggestNextReleaseBy(currentVersion: String, releaseVersion: String): String = {
    if (currentVersion == "master-SNAPSHOT" || currentVersion == "master") {
      "master"
    } else if (currentVersion.matches("^[0-9]+x-SNAPSHOT")) {
      currentVersion.replaceFirst("-SNAPSHOT$", "")
    } else {
      val withoutSnapshot = releaseVersion.replaceFirst("-SNAPSHOT", "")
      withoutSnapshot match {
        case Version.stableShop(pre) => pre + "-stable-RELEASE-DEMO-DELETE-ME"
        case Version.semverPatternLowdash(ma, mi, b, low) => ma + "." + mi + "." + b + "_" + (low.toInt + 1)
        case Version.semverPattern(ma, mi, b) => ma + "." + mi + "." + (b.toInt + 1)
        case Version.semverPatternNoBugfix(ma, mi) => ma + "." + (mi.toInt + 1) + ".0"
        case Version.semverPatternNoMinor(ma) => s"${ma.toInt + 1}.0.0"
        case Version.shopPattern(pre, year, week, minor, low) => {
          currentVersion.replaceFirst("-SNAPSHOT", "") match {
            case Version.shopPattern(_, _, _, _, _) => {
              val verso = Version.fromString(pre, year, week, minor, low).plusWeek()
              verso.copy(patch = 0, low = "").formatShop()
            }
            case any => {
              any.replaceFirst("-SNAPSHOT$", "")
            }
          }
        }
        case any => any + "-UNDEF"
      }
    }
  }

  private def gavFind(gavElements: Seq[Node], key: String): Option[Node] = {
    gavElements.find(_.getNodeName == key)
  }

  private def filterBy(doc: Document, sGroupId: String, sArtifactId: String, sVersionId: Option[String]): Seq[Node] = {
    Xpath.toSeq(doc, "//dependencies/dependency").filter(node => {

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

  def formatDependecy(in: String, selectedGroupId: String, selectedArtifcatId: String, selectedVersion: String, newVersion: String): String = {
    val doc = Xpath.newDocument(in)
    val filtered = filterBy(doc, selectedGroupId, selectedArtifcatId, Some(selectedVersion))
    filtered.headOption.foreach(node => {
      val gavElements = Xpath.toSeqNodes(node.getChildNodes)
      val versionNode: Node = gavFind(gavElements, "version").head
      versionNode.setTextContent(newVersion)
    })

    toString(doc)
  }

  private[release] def writePom(file: File, document: Document): Unit = {
    PomMod.writeContent(file, PomMod.toString(document) + "\n")
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

  private def applyVersionTo(raw: RawPomFile, selfs: Seq[Dep], newValue: String): Unit = {
    applyToKey(raw, selfs, "version", _ => newValue)
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

  private def applyToKey(raw: RawPomFile, selfs: Seq[Dep], key: String, newValueFn: String => String): Unit = {

    val ga = selfs.map(x => (x.groupId, x.artifactId))

    def childNodesByExternalKey(depNode: Node): Seq[Node] = {
      Xpath.toSeqNodes(depNode.getChildNodes)
        .filter(u => u.getNodeName == key && !u.getTextContent.isEmpty)
    }

    val tt = ga.flatMap(in => filterBy(raw.document, in._1, in._2, None))
    val result = tt
      .map(childNodesByExternalKey)
      .filter(_.nonEmpty)
      .flatten

    if (result.nonEmpty) {
      result.foreach(in => in.setTextContent(newValueFn.apply(in.getTextContent)))
    }

  }

  def modifyValueOfXpathTo(raw: RawPomFile, xpath: String, newValueFn: String => String): Unit = {
    val xPathResult = Xpath.toSeq(raw.document, xpath)
    val node = xPathResult.headOption
    if (node.isDefined) {
      node.get.setTextContent(newValueFn.apply(node.get.getTextContent))
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

  private[release] def checkRootFirstChildPropertiesVar(opts: Opts, childPomFiles: Seq[RawPomFile]): Unit = {
    case class DepProps(dep: Dep, parentDep: Dep, properties: Map[String, String])

    val allP: Seq[DepProps] = childPomFiles.map(in => {
      DepProps(in.selfDep, in.parentDep, createPropertyMap(in.document).view.filterKeys(key => !opts.skipProperties.contains(key)).toMap)
    })

    def ga(gav: Gav): String = Gav.format(Seq(gav.groupId, gav.artifactId, gav.version))

    val depProps = Util.groupedFiltered(allP)
    val depPropsMod = depProps.toList.map(in => {
      val inner = in._2.filter(o => {
        val a = ga(o.parentDep.gav())
        val b = ga(in._1.dep.gav())
        a == b
      })
      (in._1, inner)
    }).filter(_._2 != Nil)
      .map(in => {
        val allPropsFromDocs: Seq[(String, String)] = in._2.flatMap(_.properties) ++ in._1.properties
        val withRootProps: Seq[(String, String)] = Util.symmetricDiff(allPropsFromDocs, allPropsFromDocs.distinct)
        val diff = withRootProps intersect allPropsFromDocs
        if (diff.nonEmpty) {
          (diff, (Seq(in._1) ++ in._2).map(_.dep.gav()))
        } else {
          (Nil, Nil)
        }

      }).filter(_._2 != Nil)

    if (depPropsMod != Nil) {
      throw new ValidationException("unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
        depPropsMod.map(in => "  " + in._1.map(t => "(" + t._1 + " -> " + t._2 + ")").mkString(", ") +
          "\n      -> " + in._2.map(ga).mkString("\n      -> ")).mkString("\n"))
    }

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

  private[release] def writeContent(file: File, text: String): Unit = {
    Util.handleWindowsFilesystem { _ =>
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
