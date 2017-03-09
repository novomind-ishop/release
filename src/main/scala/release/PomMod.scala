package release

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.LocalDate
import java.time.temporal.WeekFields
import java.util.Locale
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{OutputKeys, TransformerFactory}

import org.w3c.dom.{Document, Node}
import org.xml.sax.SAXParseException
import release.PomMod.{Dep, PomRef}
import release.Starter.TermOs

case class PomMod(file: File) {

  private var depMap: Map[Dep, Node] = Map.empty
  private val xPathToProjectVersion = "//project/version"
  private val xPathToProjectParentVersion = "//project/parent/version"
  private val xPathToDependecies = "//dependencies/dependency"

  private val rootPom = new File(file, "pom.xml")
  if (!rootPom.canRead) {
    throw new IllegalStateException(file.toString + " seems to be no maven project")
  }
  private val rootPomDoc = pomDoc(rootPom)
  private val subPomFiles = subModulePomsFiles(file, rootPomDoc)
  private val subs: Seq[SubPomFile] = toSubPoms(subPomFiles)
  private val allPoms: Seq[Document] = rootPomDoc +: subs.map(_.document)

  val listSelf: Seq[Dep] = {
    allPoms.map(selfDep)
  }

  val selfVersion: String = {
    Util.only(listSelf.map(_.version).distinct, "version")
  }

  private lazy val currentVersion: String = {
    val opt = Xpath.onlyString(rootPomDoc, "//project/version")
    if (opt.isEmpty) {
      throw new IllegalStateException(file.getName + " as no version, please define")
    }
    opt.get
  }


  lazy val listDependecies: Seq[Dep] = {
    // has side effect
    replacedVersionProperties(allPoms.flatMap(f ⇒ deps(f))).distinct
  }

  def changeVersion(version: String): Unit = {
    Seq(rootPomDoc).foreach(d ⇒ {
      PomMod.applyValueOfXpathTo(d, xPathToProjectVersion, version)
    })
    subs.map(_.document).foreach(d ⇒ {
      PomMod.applyValueOfXpathTo(d, xPathToProjectParentVersion, version)
      PomMod.applyValueOfXpathTo(d, xPathToProjectVersion, version)
      PomMod.applyVersionTo(d, listSelf, version)
    })

  }

  def findNodes(groupId: String, artifactId: String, version: String): Seq[Node] = {
    listDependecies // side effect
    depMap.toSeq.filter(dep ⇒ dep._1.artifactId == artifactId).map(_._2).filter(_ != null)
  }

  def writeTo(targetFolder: File): Unit = {
    writePom(new File(targetFolder, "pom.xml"), rootPomDoc)
    subs.par.foreach(sub ⇒ {
      writePom(new File(new File(targetFolder, sub.subfolder), "pom.xml"), sub.document)
    })
  }

  def findNodesAndSetVersion(groupId: String, artifactId: String, version: String, newVersion: String): Unit = {
    findNodes(groupId, artifactId, version).foreach(n ⇒ {
      Xpath.toSeqNodes(n.getChildNodes).find(_.getNodeName == "version").foreach(as ⇒ {
        as.setTextContent(newVersion)
      })
    })
  }

  def showDependecyUpdates(shellWidth: Int, termOs: TermOs): Unit = {
    println("checking dependecies agains nexus - please wait")
    val rootDeps = listDependeciesReplaces()

    def normalizeUnwanted(in: Seq[String]): Seq[String] = {
      in.filterNot(_.endsWith("-SNAPSHOT"))
        .filterNot(_.contains("patch"))
        .filterNot(_.matches(".*M[0-9]+$"))
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
        val result = Aether.newerVersionsOf(dep.groupId, dep.artifactId, dep.version)
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

      def ch(pretty: String, simple: String) = if (termOs.isCygwin || !termOs.isMinGw) {
        pretty
      } else {
        simple
      }

      println(ch("║ ", "| ") + ref)
      mods.sortBy(_._1.toString).foreach((subElement: (PomMod.Dep, Seq[String])) ⇒ {
        println(ch("╠═╦═ ", "+-+- ") + subElement._1.gav)
        val o: Seq[String] = subElement._2
        val majorVers: Seq[(String, Seq[String])] = o.groupBy(ver ⇒ ver.replaceFirst("\\..*", "")).toSeq
          .sortBy(_._1).reverse
        if (majorVers.size == 1) {
          println(ch("║ ╚═══ ", "| +--- ") + majorVers.head._2.mkString(", "))
        } else {
          majorVers.tail.foreach(el ⇒ {
            println(ch("║ ╠═══ ", "| +--- ") + "(" + el._1 + ") " + el._2.mkString(", "))
          })
          println(ch("║ ╚═══ ", "| +--- ") + "(" + majorVers.head._1 + ") " + majorVers.head._2.mkString(", "))
        }

      })
      println(ch("║", "|"))
    })
    println("term: " + termOs)

  }

  def listProperties(): Map[String, String] = {

    // TODO man darf eigentlich nicht alle variablen überall ersetzen - später mal reparieren

    def createPropertyMap(pomDoc: Document): Map[String, String] = {
      val tuples = Xpath.toSeqTuples(pomDoc, "//project/properties")
      val pro: Seq[(String, String)] = tuples.flatten.map(t ⇒ (t._1, t._2))
      val result = pro.foldLeft(Map.empty[String, String])(_ + _)
      val diff = Util.symmetricDiff(result.toSeq, pro)
      if (diff.nonEmpty) {
        throw new IllegalStateException("invalid property definition / multiple: " + diff)
      }
      result
    }

    val allPropsFromDocs: Seq[(String, String)] = allPoms.flatMap(createPropertyMap)

    def checkRootFirstChildProperties(): Unit = {
      val rootProperties: Seq[(String, String)] = createPropertyMap(rootPomDoc).toSeq
      val withRootProps: Seq[(String, String)] = Util.symmetricDiff(allPropsFromDocs, rootProperties)
      val diff = withRootProps intersect rootProperties
      if (diff.nonEmpty) {
        throw new IllegalStateException("unnecessary definition in sub poms: " + diff)
      }

    }

    checkRootFirstChildProperties()

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

  def selfDepsMod: Seq[Dep] = {
    val selfDeps: Seq[Dep] = listSelf ++ listSelf.map(_.copy(scope = "test")) ++
      listSelf.map(_.copy(scope = "test", packaging = "")) ++ listSelf.map(_.copy(packaging = ""))
    val pomMods = selfDeps.map(_.copy(typeN = "pom"))
    (pomMods ++ selfDeps).map(_.copy(pomRef = PomRef.undef)).distinct.sortBy(_.toString)
  }

  def hasNoShopPom: Boolean = {
    !hasShopPom
  }

  def hasShopPom: Boolean = {
    val filtered = listSelf.filterNot(_.packaging == "pom")
    filtered.map(_.groupId).contains("com.novomind.ishop.shops")
  }

  private def replaceProperty(props: Map[String, String])(string: String) = {
    def tryReplace(in: String, p: (String, String)): String = {
      try {
        in.replace("${" + p._1 + "}", p._2)
      } catch {
        case e: IllegalArgumentException ⇒ throw new IllegalStateException(e.getMessage + " in " + in)
      }
    }

    val allReplacemdents = props.map(p ⇒ tryReplace(string, p)).toSeq
      .distinct
      .filterNot(_.startsWith("$"))
    Util.only(allReplacemdents, string)
  }

  private def replacedPropertyOf(string: String) = replaceProperty(listProperties())(string)

  private def replacedVersionProperties(deps: Seq[Dep]) = deps.map(dep ⇒ dep.copy(version = replacedPropertyOf(dep.version)))

  def listSnapshotsDistinct: Seq[Dep] = {
    Util.distinctOn[Dep, Dep](listSnapshots, _.copy(pomRef = PomRef.undef))
  }

  def listSnapshots: Seq[Dep] = {
    val deps = listDependecies
    val selfMods = selfDepsMod
    val filteredDeps = deps.filterNot(dep ⇒ selfMods.contains(dep.copy(pomRef = PomRef.undef)))

    val replacedParams = replacedVersionProperties(filteredDeps)
    val onlySnapshots = replacedParams.filter(_.version.contains("SNAPSHOT"))
    onlySnapshots
  }

  def listDependeciesReplaces(): Seq[Dep] = replacedVersionProperties(listDependecies)

  private def depFrom(id: String)(markers: Seq[(String, String, Node)]): Dep = {
    val markersMap: Map[String, String] = markers.map(t ⇒ (t._1, t._2)).foldLeft(Map.empty[String, String])(_ + _)
    val dep = Dep(pomRef = PomRef(id),
      groupId = markersMap.getOrElse("groupId", ""),
      artifactId = markersMap.getOrElse("artifactId", ""),
      version = markersMap.getOrElse("version", ""),
      typeN = markersMap.getOrElse("type", ""),
      scope = markersMap.getOrElse("scope", ""),
      packaging = markersMap.getOrElse("packaging", ""))
    val ma = markers.map(_._3).distinct
    depMap = depMap ++ Map(dep → Util.only(ma, "invalid dep creation"))
    dep
  }

  private def deps(document: Document): Seq[Dep] = {
    val self: Dep = selfDep(document)
    val xmlNodes = Xpath.toSeqTuples(document, xPathToDependecies)
    parentDep(self, document) +: xmlNodes.filter(_.nonEmpty).map(depFrom(self.pomRef.id))
  }

  private def parentDep(dep:Dep, document: Document): Dep = {
    val groupid = Xpath.onlyString(document, "//project/parent/groupId")
    val artifactId = Xpath.onlyString(document, "//project/parent/artifactId")
    val packaging = Xpath.onlyString(document, "//project/parent/packaging")
    val parentVersion = Xpath.onlyString(document, xPathToProjectParentVersion)
    val version = Xpath.onlyString(document, xPathToProjectVersion)
    val id = Seq(dep.groupId, dep.artifactId, dep.version).mkString(":")
    depFrom(id)(Map(
      "groupId" → groupid.getOrElse(""),
      "artifactId" → artifactId.getOrElse(""),
      "packaging" → packaging.getOrElse(""),
      "version" → parentVersion.getOrElse("")).toSeq.map(t ⇒ (t._1, t._2, null)))
  }

  private def selfDep(document: Document): Dep = {
    val parentGroupid = Xpath.onlyString(document, "//project/parent/groupId")
    val groupid = Xpath.onlyString(document, "//project/groupId")
    val artifactId = Xpath.onlyString(document, "//project/artifactId")
    val packaging = Xpath.onlyString(document, "//project/packaging")
    val parentVersion = Xpath.onlyString(document, xPathToProjectParentVersion)
    val version = Xpath.onlyString(document, xPathToProjectVersion)
    val id = Seq(groupid, artifactId, version).flatten.mkString(":")
    depFrom(id)(Map(
      "groupId" → groupid.orElse(parentGroupid).getOrElse(""),
      "artifactId" → artifactId.getOrElse(""),
      "packaging" → packaging.getOrElse(""),
      "version" → version.orElse(parentVersion).getOrElse("")).toSeq.map(t ⇒ (t._1, t._2, null)))
  }

  def suggestReleaseVersion(): Seq[String] = {
    PomMod.suggestReleaseBy(LocalDate.now(), currentVersion, hasShopPom)
  }

  def suggestNextRelease(releaseVersion: String): String = {
    PomMod.suggestNextReleaseBy(currentVersion, releaseVersion)
  }

  private def writePom(file: File, document: Document): Unit = {
    Files.delete(file.toPath)
    Files.write(file.toPath, (PomMod.toString(document) + "\n").getBytes(StandardCharsets.UTF_8))
  }

  private def toSubPoms(pomFiles: Seq[File]): Seq[SubPomFile] = {
    pomFiles.map(file ⇒ {
      val folderName = file.getAbsoluteFile.getParentFile.getName
      SubPomFile(folderName, pomDoc(file))
    })
  }

  private def pomDoc(file: File): Document = {
    try {
      val out = PomMod.newDocument(new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8))
      out
    } catch {
      case se: SAXParseException ⇒ throw new IllegalStateException(file.getAbsolutePath, se);
    }
  }

  private def subModuleNames(document: Document): Seq[String] = {
    Xpath.toSeq(rootPomDoc, "//project/modules/module").map(_.getTextContent)
  }

  private def subModulePomsFiles(rootFolder: File, document: Document): Seq[File] = {
    val subModules = subModuleNames(document)
    subModules.par.map(subModluleName ⇒ new File(new File(rootFolder, subModluleName).getAbsoluteFile, "pom.xml")).seq
  }

  case class SubPomFile(subfolder: String, document: Document)

}

object PomMod {

  def suggestReleaseBy(localDate: LocalDate, currentVersion: String, hasShopPom: Boolean): Seq[String] = {
    if (hasShopPom) {
      if (currentVersion.startsWith("master")) {
        def dateBased(localDate: LocalDate): String = {
          val weekFields = WeekFields.of(Locale.getDefault())
          "RC-" + localDate.getYear + "." + "%02d".format(localDate.get(weekFields.weekOfWeekBasedYear())) + "-SNAPSHOT"
        }

        Seq(dateBased(localDate), dateBased(localDate.plusWeeks(1)), dateBased(localDate.plusWeeks(2)))
      } else {
        Seq(currentVersion)
      }
    } else {
      Seq(currentVersion.replaceFirst("-SNAPSHOT$", ""))
    }
  }

  def suggestNextReleaseBy(currentVersion: String): String = {
    suggestNextReleaseBy(currentVersion, currentVersion)
  }

  def suggestNextReleaseBy(currentVersion: String, releaseVersion: String): String = {
    if (currentVersion == "master-SNAPSHOT") {
      "master"
    } else {
      val semverPattern = "^([0-9]+)\\.([0-9]+)\\.([0-9]+)$".r
      val semverPatternLowdash = "^([0-9]+)\\.([0-9]+)\\.([0-9]+_)([0-9]+)$".r
      val shopPattern = "^(RC-)([0-9]{4})\\.([0-9]+)(?:\\.[0-9]+)?$".r

      val snapped = releaseVersion.replaceFirst("-SNAPSHOT", "")
      snapped match {
        case semverPattern(ma, mi, b) ⇒ ma + "." + mi + "." + (b.toInt + 1)
        case semverPatternLowdash(ma, mi, b, low) ⇒ ma + "." + mi + "." + b + (low.toInt + 1)
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
    val doc = newDocument(in)
    val filtered = filterBy(doc, selectedGroupId, selectedArtifcatId, Some(selectedVersion))
    filtered.headOption.foreach(node ⇒ {
      val gavElements = Xpath.toSeqNodes(node.getChildNodes)
      val versionNode: Node = gavFind(gavElements, "version").head
      versionNode.setTextContent(newVersion)
    })

    toString(doc)
  }

  private def newDocument(in: String): Document = {
    val docFactory = DocumentBuilderFactory.newInstance()
    val docBuilder = docFactory.newDocumentBuilder()
    val stream = new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8))
    docBuilder.parse(stream)
  }

  private def toString(doc: Document): String = {
    val transformerFactory = TransformerFactory.newInstance()
    val transformer = transformerFactory.newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    val baos = new ByteArrayOutputStream()
    transformer.transform(new DOMSource(doc), new StreamResult(baos))
    baos.toString(StandardCharsets.UTF_8.displayName()).replace("\r", "")
      .replaceAll("/>", " />").replaceFirst("[ ]+xsi:schemaLocation", "\n  xsi:schemaLocation")
  }

  private def applyVersionTo(doc: Document, selfs: Seq[Dep], newValue: String): Unit = {

    val ga = selfs.map(x ⇒ (x.groupId, x.artifactId))

    def asf(depNode: Node) = {
      Xpath.toSeqNodes(depNode.getChildNodes)
        .filter(u ⇒ u.getNodeName == "version" && !u.getTextContent.isEmpty)
    }

    val result = ga.flatMap(in ⇒ filterBy(doc, in._1, in._2, None))
      .map(asf)
      .filter(_.nonEmpty)
      .flatten

    if (result.nonEmpty) {
      result.foreach(_.setTextContent(newValue))
    }

  }

  def applyValueOfXpathTo(doc: Document, xpath: String, newValue: String): Unit = {
    val xPathResult = Xpath.toSeq(doc, xpath)
    val node = xPathResult.headOption
    if (node.isDefined) {
      node.get.setTextContent(newValue)
    }
  }

  def format(in: String, xPathStr: String, value: String): String = {
    val doc = newDocument(in)
    applyValueOfXpathTo(doc, xPathStr, value)
    toString(doc)
  }

  case class Dep(pomRef: PomRef, groupId: String, artifactId: String, version: String, typeN: String,
                 scope: String, packaging: String) {
    val gav: String = Seq(groupId, artifactId, version, typeN, scope, packaging)
      .mkString(":").replaceAll("[:]{2,}", ":").replaceFirst(":$", "")
  }

  case class PomRef(id: String)

  object PomRef {
    val undef = PomRef("X")
  }

}
