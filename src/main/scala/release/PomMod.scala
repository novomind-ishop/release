package release

import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.LocalDate
import java.time.temporal.WeekFields
import java.util.Locale

import com.google.common.annotations.VisibleForTesting
import com.typesafe.scalalogging.LazyLogging
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, TransformerFactory}
import org.w3c.dom.{Document, Node}
import release.Conf.Tracer
import release.PomChecker.ValidationException
import release.PomMod._
import release.Starter.{Opts, PreconditionsException, TermOs}

import scala.annotation.tailrec

case class PomMod(file: File, aether: Aether) extends LazyLogging {
  logger.trace("init pomMod")
  private var depMap: Map[Dep, Node] = Map.empty

  private val xPathToDependecies = "//dependencies/dependency"

  private val rootPom = new File(file, "pom.xml")
  if (!rootPom.canRead) {
    throw new PreconditionsException(file.toString + " seems to be no maven project")
  }
  private val allRawPomFiles = Tracer.msgAround("read all poms", logger, () ⇒ allRawModulePomsFiles(Seq(file)))

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
    Util.only(v, "More then one Version found in your pom.xmls")
  }

  private val currentVersion: Option[String] = {
    Xpath.onlyString(Xpath.pomDoc(rootPom), PomMod.xPathToProjectVersion)
  }

  private def checkRootFirstChildProperties(): Unit = {
    PomMod.checkRootFirstChildPropertiesVar(raws)
  }

  private[release] val listProperties: Map[String, String] = {
    checkRootFirstChildProperties()
    val allPropsFromDocs = allPomsDocs.flatMap(PomMod.createPropertyMap)
    val result = allPropsFromDocs.foldLeft(Map.empty[String, String])(_ + _)
    val selfVersion = Util.only(listSelf.map(_.version).distinct, "version")

    def skip(key: String)(p: (String, String)) = p.copy(_2 = p._2.replace(key, "skip"))

    val allProps = (result ++ Map("project.version" → selfVersion))
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

  private[release] val listPluginDependencies: Seq[PluginDep] = {
    val allP: Seq[PluginDep] = allPomsDocs.map(in ⇒ {
      val gav = PomMod.selfDep(depU)(in).gavWithDetailsFormatted
      val nodes = Xpath.toSeqTuples(in, "//plugins/plugin")
      (nodes, gav)
    }).flatMap(gavNode ⇒ {
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
    .map(f ⇒ (f, DepTree(Util.read(f))))
    .filterNot(in ⇒ in._1.getParentFile.getName == ".")
    .foldLeft(Map.empty[File, DepTree])(_ + _)

  logger.trace("pomMod val/var init")

  private def nodePath(node: Node): Seq[String] = {
    // TODO @tailrec
    def nodePathOther(node: Node): Seq[String] = {
      val parent = node.getParentNode
      if ("#document" == parent.getNodeName) {
        Nil
      } else {
        Seq(parent.getNodeName) ++ nodePathOther(parent)
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
    val execs = execNodes.map(in ⇒ {
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
    findNodes(groupId, artifactId, version).foreach(n ⇒ {
      Xpath.toSeqNodes(n.getChildNodes).find(_.getNodeName == "version").foreach(as ⇒ {
        as.setTextContent(newVersion)
      })
    })

    changeDepTreesVersion(groupId, artifactId, version, newVersion)
  }

  private[release] def changeDepTreesVersion(groupId: String, artifactId: String, version: String, newVersion: String): Unit = {
    depTreeFileContents = depTreeFileContents.map(entry ⇒ {
      (entry._1, entry._2.copy(replacedDepTreesVersion(entry._2.content, groupId, artifactId, version, newVersion)))
    }).foldLeft(Map.empty[File, DepTree])(_ + _)
  }

  private def replacedDepTreesVersion(in: String, groupId: String, artifactId: String, version: String, newVersion: String) = {
    in.lines
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):([^:]*):" + version, groupId + ":" + artifactId + ":$1:$2:" + newVersion))
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):" + version, groupId + ":" + artifactId + ":$1:" + newVersion))
      .mkString("\n") + "\n"
  }

  private[release] def changeDepTreesGA(groupId: String, artifactId: String, version: String,
                                        groupIdFn: String ⇒ String, artifactIdFn: String ⇒ String): Unit = {
    depTreeFileContents = depTreeFileContents.map(entry ⇒ {
      (entry._1, entry._2.copy(replacedDepTreesGA(entry._2.content, groupId, artifactId, version, groupIdFn, artifactIdFn)))
    }).foldLeft(Map.empty[File, DepTree])(_ + _)
  }

  private def replacedDepTreesGA(in: String, groupId: String, artifactId: String, version: String,
                                 groupIdFn: String ⇒ String, artifactIdFn: String ⇒ String) = {
    in.lines
      .map(_.replaceFirst(groupId + ":" + artifactId + ":war:" + version, groupIdFn.apply(groupId).replace("." + artifactIdFn.apply(artifactId), "") + ":" + artifactIdFn.apply(artifactId) + ":war:" + version))
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):([^:]*):" + version, groupIdFn.apply(groupId) + ":" + artifactIdFn.apply(artifactId) + ":$1:$2:" + version))
      .map(_.replaceFirst(groupId + ":" + artifactId + ":([^:]*):" + version, groupIdFn.apply(groupId) + ":" + artifactIdFn.apply(artifactId) + ":$1:" + version))
      .mkString("\n") + "\n"
  }

  def getVersionFromDocs(): String = {
    val out = raws.map(_.document).flatMap(d ⇒ {
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

    val replaceInArtifactId: (String ⇒ String) = in ⇒ {
      newValue + in.replaceFirst("^[^-]+", "")
    }

    val replaceInName: (String ⇒ String) = {
      case any if any.startsWith("Shopsystem:") ⇒ "Shopsystem:" + newValue.substring(0, 1).toUpperCase + newValue.substring(1)
      case any ⇒ newValue + any.replaceFirst("^[^-]+", "")
    }

    raws.foreach(d ⇒ {
      if (d.pomFile.getParentFile == file) {
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectGroupId, "com.novomind.ishop.shops." + newValue)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectArtifactId, replaceInArtifactId)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectName, replaceInName)
        PomMod.applyToGroupAndArtifactId(d, listSelf, _ ⇒ "com.novomind.ishop.shops." + newValue, replaceInArtifactId)
      } else {
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectParentGroupId, "com.novomind.ishop.shops." + newValue)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectParentArtifactId, replaceInArtifactId)
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectName, replaceInName)
        PomMod.applyValueOfXpathTo(d, PomMod.xPathToProjectGroupId, "com.novomind.ishop.shops")
        PomMod.modifyValueOfXpathTo(d, PomMod.xPathToProjectArtifactId, replaceInArtifactId)
        PomMod.applyToGroupAndArtifactId(d, listSelf, _ ⇒ "com.novomind.ishop.shops." + newValue, replaceInArtifactId)
      }
    })
    selfDepsMod.foreach(entry ⇒ {
      changeDepTreesGA(entry.groupId, entry.artifactId, entry.version, _ ⇒ "com.novomind.ishop.shops." + newValue, replaceInArtifactId)
    })
  }

  def changeVersion(newVersion: String): Unit = {
    raws.foreach(d ⇒ {
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
    selfDepsMod.foreach(entry ⇒ {
      changeDepTreesVersion(entry.groupId, entry.artifactId, entry.version, newVersion)
    })

  }

  def depTreeFilenames(): Seq[String] = {
    depTreeFiles().map(f ⇒ {
      if (f.getParentFile.getName != file.getName) {
        val prefix = f.getParentFile.getName + "/" match {
          case "./" ⇒ ""
          case other ⇒ other
        }
        prefix + f.getName
      } else {
        f.getName
      }
    }).distinct
  }

  def depTreeFiles(): Seq[File] = {
    depTreeFilename().map(f ⇒ Seq(f) ++ raws.map(_.subfolder + "/" + f))
      .map(in ⇒ in.map(f ⇒ new File(file, f).getAbsoluteFile).filter(_.exists()))
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
    depMap.toList.filter(dep ⇒ replacedPropertyOf(dep._1.artifactId) == artifactId).map(_._2).filter(_ != null)
  }

  def writeTo(targetFolder: File): Unit = {
    raws.par.foreach(sub ⇒ {
      val path = file.toPath.relativize(sub.pomFile.toPath)
      PomMod.writePom(targetFolder.toPath.resolve(path).toFile, sub.document)
    })

    depTreeFileContents
      .foreach(fe ⇒ {
        val content = fe._2.content
        if (fe._1.getParentFile.getName == file.getName) {
          PomMod.writeContent(new File(targetFolder, fe._1.getName), content)
        } else {
          PomMod.writeContent(new File(new File(targetFolder, fe._1.getParentFile.getName), fe._1.getName), content)
        }
      })
  }

  def showDependencyUpdates(shellWidth: Int, termOs: TermOs, out: PrintStream): Unit = {
    out.println("I: checking dependecies against nexus - please wait")
    val rootDeps = listDependeciesReplaces()

    def normalizeUnwanted(in: Seq[String]): Seq[String] = {
      in.filterNot(_.endsWith("-SNAPSHOT"))
        .filterNot(_.contains("patch"))
        .filterNot(_.matches(".*M[0-9]+$"))
        .filterNot(_.matches(".*pr[0-9]+$"))
        .filterNot(_.contains("pre"))
        .filterNot(_.contains("alpha"))
        .filterNot(_.contains("Alpha"))
        .filterNot(_.contains("ALPHA"))
        .filterNot(_.contains("BETA"))
        .filterNot(_.contains("Beta"))
        .filterNot(_.contains("beta"))
        .filterNot(_.contains("brew"))
        .filterNot(_.contains("EA"))
        .filterNot(_.matches(".*b[0-9]+.*"))
        .filterNot(_.endsWith(".*\\-beta$"))
        .filterNot(_.contains("RC"))
        .filterNot(_.contains("rc"))
        .filterNot(_.matches(".*SP[0-9]+$"))
        .filterNot(_.endsWith("-incubating"))
        .filterNot(_.endsWith("SONATYPE"))
        .filterNot(_.contains("jbossorg"))
        .filterNot(_.contains("-atlassian-"))
        .filterNot(_.contains("PFD"))
        .filterNot(_.contains("-cdh"))
        .filterNot(_.contains("darft"))
        .filterNot(_.startsWith("2003"))
        .filterNot(_.startsWith("2004"))

        .filterNot(_.endsWith("-PUBLISHED-BY-MISTAKE"))
    }

    val relevant = rootDeps
      .filterNot(_.version == "")
      .filterNot(_.groupId.startsWith("org.apache.tomcat")) // Remove later
      .distinct

    val aetherFetch = StatusLine(relevant.size, shellWidth)
    val updates = relevant.par
      .map(dep ⇒ (dep, {
        aetherFetch.start()
        val result = aether.newerVersionsOf(dep.groupId, dep.artifactId, dep.version)
        aetherFetch.end()
        result
      }))
      .seq
      .map(in ⇒ in.copy(_2 = normalizeUnwanted(in._2)))
      .filter(_._2.nonEmpty)
    aetherFetch.finish()
    updates.groupBy(_._1.pomRef).foreach(element ⇒ {
      val ref: PomRef = element._1
      val mods: Seq[(PomMod.Dep, Seq[String])] = element._2

      def ch(pretty: String, simple: String) = if (!termOs.isCygwin || termOs.isMinGw) {
        if (termOs.simpleChars) {
          simple
        } else {
          pretty
        }
      } else {
        simple
      }

      out.println(ch("║ ", "| ") + ref)
      mods.sortBy(_._1.toString).foreach((subElement: (PomMod.Dep, Seq[String])) ⇒ {
        out.println(ch("╠═╦═ ", "+-+- ") + subElement._1.gavWithDetailsFormatted)
        val o: Seq[String] = subElement._2

        def majorGrouping(in: String): String = {
          val major = in.replaceFirst("\\..*", "")
          if (in == major) {
            "_"
          } else {
            major
          }
        }

        val majorVersions: Seq[(String, Seq[String])] = o.groupBy(majorGrouping).toSeq
          .sortBy(_._1).reverse
        if (majorVersions.size == 1) {
          out.println(ch("║ ╚═══ ", "| +--- ") + majorVersions.head._2.mkString(", "))
        } else {
          majorVersions.tail.reverse.foreach(el ⇒ {
            out.println(ch("║ ╠═══ ", "| +--- ") + "(" + el._1 + ") " + el._2.mkString(", "))
          })
          out.println(ch("║ ╚═══ ", "| +--- ") + "(" + majorVersions.head._1 + ") " + majorVersions.head._2.mkString(", "))
        }

      })
      out.println(ch("║", "|"))
    })
    out.println("term: " + termOs)

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
    (pomMods ++ selfDeps)
      .map(_.copy(pomRef = PomRef.undef))
      .distinct
      .sortBy(_.toString)
  }

  def hasNoShopPom: Boolean = {
    !hasShopPom
  }

  def hasShopPom: Boolean = {
    val filtered = listSelf.filterNot(_.packaging == "pom")
    filtered.map(_.groupId).contains("com.novomind.ishop.shops")
  }

  private def replacedPropertyOf(string: String) = PomMod.replaceProperty(listProperties)(string)

  private def replacedVersionProperties(deps: Seq[Dep]) = deps.map(dep ⇒ dep.copy(
    version = replacedPropertyOf(dep.version),
    packaging = replacedPropertyOf(dep.packaging),
    typeN = replacedPropertyOf(dep.typeN),
    scope = replacedPropertyOf(dep.scope))
  ).map(in ⇒ {
    if (in.toString.contains("$")) {
      throw new IllegalStateException("missing var in " + in)
    }
    in
  })

  private def replacedVersionProperty(dep: PluginDep) = dep.copy(version = replacedPropertyOf(dep.version))

  def listSnapshotsDistinct: Seq[Dep] = {
    Util.distinctOn[Dep, Dep](listSnapshots, _.copy(pomRef = PomRef.undef))
  }

  def listSnapshots: Seq[Dep] = {
    val deps = listDependecies
    val selfMods = selfDepsMod
    val filteredDeps = deps.filterNot(dep ⇒ {
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

  private def listDependeciesReplaces(): Seq[Dep] = replacedVersionProperties(listDependecies)

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
    PomMod.suggestReleaseBy(LocalDate.now(), currentVersion.get, hasShopPom, branchNames)
  }

  def suggestNextRelease(releaseVersion: String): String = {
    checkCurrentVersion(currentVersion)
    PomMod.suggestNextReleaseBy(currentVersion.get, releaseVersion)
  }

  private def toRawPom(pomFile: File): RawPomFile = {
    // TODO check if pom is sub sub module - parse
    RawPomFile(pomFile, Xpath.pomDoc(pomFile), file)
  }

  private def toRawPoms(pomFiles: Seq[File]): Seq[RawPomFile] = {
    pomFiles.map(toRawPom)
  }

  private def subModuleNames(document: Document): Seq[String] = {
    Xpath.toSeq(document, "//project/modules/module").map(_.getTextContent)
  }

  // TODO @tailrec
  private def allRawModulePomsFiles(rootFolders: Seq[File]): Seq[File] = {
    def isPomPacked(document: Document): Boolean = {
      Seq("pom") == Xpath.toSeq(document, PomMod.xPathToProjectPackaging).map(_.getTextContent)
    }

    rootFolders
      .flatMap(rootFolder ⇒ {
        val p = if (rootFolder.isFile) {
          rootFolder.getParentFile
        } else {
          new File(rootFolder, "pom.xml")
        }
        val document: Document = Xpath.pomDoc(p)
        if (isPomPacked(document)) {
          val subModules = subModuleNames(document)
          val s = subModules.par.map(subModuleName ⇒ {
            new File(p.getParentFile, subModuleName).getAbsoluteFile
          }).seq
          val o: Seq[File] = p +: allRawModulePomsFiles(s)
          o
        } else {
          Seq(p)
        }
      }).map(r ⇒ {
      val files = r.isFile
      if (!files) {
        throw new IllegalStateException("asdf " + r)
      } else {
        r
      }
    })

  }
}

case class RawPomFile(pomFile: File, document: Document, file: File) {
  val subfolder: String = if (pomFile.getParentFile == file) {
    "."
  } else {
    pomFile.getAbsoluteFile.getParentFile.getName
  }

  lazy val selfDep: Dep = PomMod.selfDep(_ ⇒ Unit)(document)

  lazy val parentDep: Dep = PomMod.parentDep(_ ⇒ Unit)(selfDep, document)

}

object PomMod {

  def of(file: File, unnused: PrintStream, opts: Opts): PomMod = {
    lazy val aether = new Aether(opts)
    ofAether(file, unnused, aether)
  }

  def ofAether(file: File, unnused: PrintStream, aether: Aether): PomMod = {
    PomMod(file, aether)
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

  private val semverPattern = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)$".r
  private val semverPatternNoBugfix = "^([0-9]+)\\.([0-9]+)$".r
  private val semverPatternNoMinor = "^([0-9]+)$".r
  private val semverPatternLowdash = "^([0-9]+)\\.([0-9]+)\\.([0-9]+_)([0-9]+)$".r

  private def depFrom(id: String, dfn: Map[Dep, Node] ⇒ Unit)(depSeq: Seq[(String, String, Node)]): Dep = {
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

    dfn.apply(Map(dep → Util.only(ma, "invalid dep creation")))
    dep
  }

  private[release] def selfDep(dfn: Map[Dep, Node] ⇒ Unit)(document: Document): Dep = {
    val parentGroupid = Xpath.onlyString(document, PomMod.xPathToProjectParentGroupId)
    val groupid = Xpath.onlyString(document, PomMod.xPathToProjectGroupId)
    val artifactId = Xpath.onlyString(document, PomMod.xPathToProjectArtifactId)
    val packaging = Xpath.onlyString(document, PomMod.xPathToProjectPackaging)
    val parentVersion = Xpath.onlyString(document, PomMod.xPathToProjectParentVersion)
    val version = Xpath.onlyString(document, PomMod.xPathToProjectVersion)
    val id = Seq(groupid, artifactId, version).flatten.mkString(":")
    depFrom(id, dfn)(Map(
      "groupId" → groupid.orElse(parentGroupid).getOrElse(""),
      "artifactId" → artifactId.getOrElse(""),
      "packaging" → packaging.getOrElse(""),
      "version" → version.orElse(parentVersion).getOrElse("")).toSeq.map(t ⇒ (t._1, t._2, null)))
  }

  private[release] def parentDep(dfn: Map[Dep, Node] ⇒ Unit)(dep: Dep, document: Document): Dep = {
    val groupid = Xpath.onlyString(document, PomMod.xPathToProjectParentGroupId)
    val artifactId = Xpath.onlyString(document, PomMod.xPathToProjectParentArtifactId)
    val packaging = Xpath.onlyString(document, PomMod.xPathToProjectParentPackaging)
    val parentVersion = Xpath.onlyString(document, PomMod.xPathToProjectParentVersion)
    val id = Seq(dep.groupId, dep.artifactId, dep.version).mkString(":")
    PomMod.depFrom(id, dfn)(Map(
      "groupId" → groupid.getOrElse(""),
      "artifactId" → artifactId.getOrElse(""),
      "packaging" → packaging.getOrElse(""),
      "version" → parentVersion.getOrElse("")).toSeq.map(t ⇒ (t._1, t._2, null)))
  }

  private[release] def replaceProperty(props: Map[String, String])(input: String) = {
    if (props.isEmpty) {
      throw new IllegalStateException("property map is empty")
    }

    def tryReplace(in: String, p: (String, String)): String = {
      try {
        in.replace("${" + p._1 + "}", p._2)
      } catch {
        case e: IllegalArgumentException ⇒ throw new IllegalStateException(e.getMessage + " in " + in)
      }
    }

    val allReplacemdents = props.map(p ⇒ tryReplace(input, p)).toSeq
      .distinct
      .filterNot(_.contains("$"))
    Util.only(allReplacemdents, "No property replacement found in pom.xmls for: \"" + input + "\"")
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

      @tailrec
      def nextReleaseIfExisting(known: Seq[String], name: String, suffix: Int): String = {
        val finalName = if (suffix == 0) {
          name
        } else {
          name + "." + suffix
        }
        if (known.contains(finalName)) {
          nextReleaseIfExisting(known, name, suffix + 1)
        } else {
          finalName
        }
      }

      if (currentVersion.startsWith("master")) {

        def dateBased(localDate: LocalDate, known: Seq[String]): String = {
          val weekFields = WeekFields.of(Locale.getDefault())
          val releaseVersion = "RC-" + localDate.getYear + "." + "%02d".format(localDate.get(weekFields.weekOfWeekBasedYear()))
          nextReleaseIfExisting(known, releaseVersion, 0) + "-SNAPSHOT"
        }

        Seq(dateBased(localDate, releaseBranchNames),
          dateBased(localDate.plusWeeks(1), releaseBranchNames),
          dateBased(localDate.plusWeeks(2), releaseBranchNames))
      } else if (currentVersion.matches("^[0-9]+x-SNAPSHOT$")) {
        val withoutSnapshot = currentVersion.replaceFirst("x-SNAPSHOT$", "") + ".0.0"

        def toInts(m: String): (Int, Int, Int, Int) = m match {
          case semverPattern(ma, mi, b) ⇒ (ma.toInt, mi.toInt, b.toInt, 0)
          case semverPatternLowdash(ma, mi, b, low) ⇒ (ma.toInt, mi.toInt, b.toInt, low.toInt)
          case _ ⇒ (-1, -1, -1, -1)
        }

        val versionTupels = branchNames.map(_.replace("release/", "")).map(toInts).filterNot(_._1 <= -1)

        // TODO @tailrec
        def nextNumberedReleaseIfExisting(known: Seq[(Int, Int, Int, Int)], name: (Int, Int, Int, Int)): Seq[String] = {

          if (known.contains(name)) {
            val latest = known.max
            nextNumberedReleaseIfExisting(known, latest.copy(_3 = latest._3 + 1)) ++
              nextNumberedReleaseIfExisting(known, latest.copy(_2 = latest._2 + 1, _3 = 0))
          } else {
            Seq(Seq(name._1, name._2, name._3).mkString("."))
          }
        }

        val n = toInts(withoutSnapshot)
        nextNumberedReleaseIfExisting(versionTupels, n).map(_ + "-SNAPSHOT")
      } else {
        Seq(nextReleaseIfExisting(releaseBranchNames, currentVersion.replaceFirst("-SNAPSHOT$", ""), 0) + "-SNAPSHOT")
      }
    } else {
      Seq(currentVersion.replaceFirst("-SNAPSHOT$", ""))
    }
  }

  def suggestNextReleaseBy(currentVersion: String): String = {
    suggestNextReleaseBy(currentVersion, currentVersion)
  }

  def isUnknownReleasePattern(in: String): Boolean = {
    suggestNextReleaseBy(in, in).endsWith("-UNDEF")
  }

  def suggestNextReleaseBy(currentVersion: String, releaseVersion: String): String = {
    if (currentVersion == "master-SNAPSHOT") {
      "master"
    } else if (currentVersion == "master") {
      "master"
    } else if (currentVersion.matches("^[0-9]+x-SNAPSHOT")) {
      currentVersion.replaceFirst("-SNAPSHOT$", "")
    } else {
      val shopPattern = "^(RC-)([0-9]{4})\\.([0-9]+)(?:\\.[0-9]+[0-9\\.]*)?$".r
      val stableShop = "^([0-9]+x)-stable.*$".r

      val snapped = releaseVersion.replaceFirst("-SNAPSHOT", "")
      snapped match {
        case stableShop(pre) ⇒ pre + "-stable-RELEASE-DEMO-DELETE-ME"
        case semverPatternLowdash(ma, mi, b, low) ⇒ ma + "." + mi + "." + b + (low.toInt + 1)
        case semverPattern(ma, mi, b) ⇒ ma + "." + mi + "." + (b.toInt + 1)
        case semverPatternNoBugfix(ma, mi) ⇒ ma + "." + (mi.toInt + 1) + ".0"
        case semverPatternNoMinor(ma) ⇒ (ma.toInt + 1) + ".0.0"
        case shopPattern(pre, year, week) ⇒ {
          val addWeek = week.toInt + 1
          val nextWeek = if (addWeek > 52) {
            1
          } else {
            addWeek
          }
          val nextYear = if (addWeek > 52) {
            year.toInt + 1
          } else {
            year
          }
          pre + nextYear + "." + "%02d".format(nextWeek)
        }
        case any ⇒ any + "-UNDEF"
      }
    }
  }

  private def gavFind(gavElements: Seq[Node], key: String): Option[Node] = {
    gavElements.find(_.getNodeName == key)
  }

  private def filterBy(doc: Document, sGroupId: String, sArtifactId: String, sVersionId: Option[String]): Seq[Node] = {
    Xpath.toSeq(doc, "//dependencies/dependency").filter(node ⇒ {

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

  def formatDependcy(in: String, selectedGroupId: String, selectedArtifcatId: String, selectedVersion: String, newVersion: String): String = {
    val doc = Xpath.newDocument(in)
    val filtered = filterBy(doc, selectedGroupId, selectedArtifcatId, Some(selectedVersion))
    filtered.headOption.foreach(node ⇒ {
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
    applyToKey(raw, selfs, "version", _ ⇒ newValue)
  }

  private def applyToGroupAndArtifactId(raw: RawPomFile, selfs: Seq[Dep], groupidFn: (String ⇒ String), artifactIdFn: (String ⇒ String)): Unit = {

    val ga = selfs.map(x ⇒ (x.groupId, x.artifactId))

    def childNodesByExternalKey(depNode: Node): Seq[Node] = {
      Xpath.toSeqNodes(depNode.getChildNodes)
        .filter(u ⇒ (u.getNodeName == "groupId" || u.getNodeName == "artifactId") && !u.getTextContent.isEmpty)
    }

    val tt = ga.flatMap(in ⇒ filterBy(raw.document, in._1, in._2, None))
    val result = tt
      .map(childNodesByExternalKey)
      .filter(_.nonEmpty)
      .flatten

    if (result.nonEmpty) {
      result.foreach(in ⇒ {
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

  private def applyToKey(raw: RawPomFile, selfs: Seq[Dep], key: String, newValueFn: (String ⇒ String)): Unit = {

    val ga = selfs.map(x ⇒ (x.groupId, x.artifactId))

    def childNodesByExternalKey(depNode: Node): Seq[Node] = {
      Xpath.toSeqNodes(depNode.getChildNodes)
        .filter(u ⇒ u.getNodeName == key && !u.getTextContent.isEmpty)
    }

    val tt = ga.flatMap(in ⇒ filterBy(raw.document, in._1, in._2, None))
    val result = tt
      .map(childNodesByExternalKey)
      .filter(_.nonEmpty)
      .flatten

    if (result.nonEmpty) {
      result.foreach(in ⇒ in.setTextContent(newValueFn.apply(in.getTextContent)))
    }

  }

  def modifyValueOfXpathTo(raw: RawPomFile, xpath: String, newValueFn: (String ⇒ String)): Unit = {
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

  private[release] def checkRootFirstChildPropertiesVar(childs: Seq[RawPomFile], excludedNames: Seq[String] = Nil): Unit = {
    case class DepProps(dep: Dep, pdep: Dep, properties: Map[String, String])

    val allP: Seq[DepProps] = childs.map(in ⇒ {
      DepProps(in.selfDep, in.parentDep, createPropertyMap(in.document))
    })

    def ga(gav: Gav): String = Gav.format(Seq(gav.groupId, gav.artifactId, gav.version))

    val depProps = Util.groupedFiltered(allP)
    val depPropsMod = depProps.map(in ⇒ {
      val inner = in._2.filter(o ⇒ {
        val a = ga(o.pdep.gav()) // TODO ob dieser ga wert gut ist?
        val b = ga(in._1.dep.gav())
        a == b
      })
      (in._1, inner)
    }).filter(_._2 != Nil)
      .map(in ⇒ {
        val allPropsFromDocs: Seq[(String, String)] = in._2.flatMap(_.properties) ++ in._1.properties
        val withRootProps: Seq[(String, String)] = Util.symmetricDiff(allPropsFromDocs, allPropsFromDocs.distinct)
        val diff = withRootProps intersect allPropsFromDocs
        if (diff.nonEmpty) {
          (diff, (Seq(in._1) ++ in._2).map(_.dep.gav()))
        } else {
          (Nil, Nil)
        }

      }).filter(_._2 != Nil)

    if (depPropsMod != Map.empty) {
      throw new ValidationException("unnecessary/multiple property definition (move property to parent pom or remove from sub poms):\n" +
        depPropsMod.map(in ⇒ "  " + in._1.map(t ⇒ "(" + t._1 + " -> " + t._2 + ")").mkString(", ") +
          "\n      -> " + in._2.map(ga).mkString("\n      -> ")).mkString("\n"))
    }

  }

  private[release] def createPropertyMap(pomDoc: Document): Map[String, String] = {
    val tuples = Xpath.toSeqTuples(pomDoc, "//project/properties")
    val pro: Seq[(String, String)] = tuples.flatten.map(t ⇒ (t._1, t._2))
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
    Util.handleWindowsFilesystem { _ ⇒
      if (Files.exists(file.toPath)) {
        Files.delete(file.toPath)
      }
      if (!Files.isDirectory(file.toPath.getParent)) {
        Files.createDirectories(file.toPath.getParent)
      }
      Files.write(file.toPath, text.getBytes(StandardCharsets.UTF_8))
    }
  }

  @VisibleForTesting
  def format(in: String, xPathStr: String, value: String): String = {
    val raw = RawPomFile(new File("."), Xpath.newDocument(in), new File("."))
    applyValueOfXpathTo(raw, xPathStr, value)
    toString(raw.document)
  }

  case class Dep(pomRef: PomRef, groupId: String, artifactId: String, version: String, typeN: String,
                 scope: String, packaging: String, classifier: String) {
    val gavWithDetailsFormatted: String = Gav.format(Seq(groupId, artifactId, version, typeN, scope, packaging, classifier))

    def gav() = Gav(groupId, artifactId, version, packaging, classifier, scope)
  }

  case class PluginExec(id: String, goals: Seq[String], phase: String, config: Map[String, String])

  case class PluginDep(pomRef: PomRef, groupId: String, artifactId: String, version: String, execs: Seq[PluginExec], pomPath: Seq[String]) {

    def gav() = Gav(groupId, artifactId, version)
  }

  case class Gav(groupId: String, artifactId: String, version: String, packageing: String = "", classifier: String = "", scope: String = "") {
    def formatted = Gav.format(Seq(groupId, artifactId, version, packageing, classifier, scope))
  }

  object Gav {
    def format(parts: Seq[String]): String = parts.mkString(":").replaceAll("[:]{2,}", ":").replaceFirst(":$", "")
  }

  case class PomRef(id: String)

  object PomRef {
    val undef = PomRef("X")
  }

}
