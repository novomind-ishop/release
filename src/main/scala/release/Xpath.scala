package release

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.{XPathConstants, XPathFactory}
import org.w3c.dom.{Document, Node, NodeList}
import org.xml.sax.SAXParseException

object Xpath {

  def pomDoc(file: File): Document = {
    if (!file.exists()) {
      throw new IllegalArgumentException("file does not exist; you passed: " + file.getAbsolutePath)
    } else if (!file.isFile) {
      throw new IllegalArgumentException("only files are allowed; you passed: " + file.getAbsolutePath)
    } else {
      try {
        newDocument(new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8))
      } catch {
        case se: SAXParseException => throw InvalidPomXmlException(file, se);
      }
    }
  }

  def newDocument(in: String): Document = {
    val docFactory = DocumentBuilderFactory.newInstance()
    val docBuilder = docFactory.newDocumentBuilder()
    val stream = new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8))
    docBuilder.parse(stream)
  }

  def onlyString(document: Document, xpath: String): Option[String] = {
    only(document, xpath).map(_.getTextContent)
  }

  def only(document: Document, xpath: String): Option[Node] = {
    val elements = toSeq(document, xpath)
    Util.onlyOpt(elements)
  }

  private def nodeCommentFilter(node: Node): Option[Node] = node match {
    case element: Node if element.getNodeType == Node.ELEMENT_NODE => None
    case element: Node if element.getNodeType == Node.TEXT_NODE => None
    case element: Node if element.getNodeType == Node.COMMENT_NODE => Some(node)
    case elment => throw new IllegalStateException(elment.getClass.getCanonicalName)
  }

  private def nodeFilter(node: Node): Option[Node] = node match {
    case element: Node if element.getNodeType == Node.ELEMENT_NODE => Some(node)
    case element: Node if element.getNodeType == Node.TEXT_NODE => None
    case element: Node if element.getNodeType == Node.COMMENT_NODE => None
    case elment => throw new IllegalStateException(elment.getClass.getCanonicalName)
  }

  private def convertToTuples(node: Node, nodeList: NodeList): Seq[(String, String, Node)] = {
    val length = nodeList.getLength
    if (length == 0) {
      throw new IllegalStateException("no elements found")
    }
    val tupleOpts: Seq[Option[(String, String, Node)]] = for (i <- Range(0, length)) yield {
      val singleNote: Node = nodeList.item(i)
      val nodeTupleOpt: Option[(String, String, Node)] = nodeFilter(singleNote)
        .map(element => (element.getNodeName, element.getTextContent, node))
      nodeTupleOpt
    }
    tupleOpts.flatten
  }

  def mapToSeqMap(nodes: Seq[Node]): Seq[Map[String, String]] = {
    mapToSeqTuples(nodes).map(in => {
      in.foldLeft(Map.empty[String, String])((m, t) => m ++ Map(t._1 -> t._2))
    })
  }

  private def mapToSeqTuples(nodes: Seq[Node]): Seq[Seq[(String, String, Node)]] = {
    nodes.map(in => convertToTuples(in, in.getChildNodes))
  }

  def toSeqTuples(document: Document, xpath: String): Seq[Seq[(String, String, Node)]] = {
    mapToSeqTuples(toSeq(document, xpath))
  }

  private def toSeqNodesF(nodeList: NodeList, nodeF: Node => Option[Node]): Seq[Node] = {
    List.tabulate(nodeList.getLength)(i => i).map(key => nodeList.item(key)).flatMap(in => nodeF(in))
  }

  def toSeqNodes(nodeList: NodeList): Seq[Node] = {
    toSeqNodesF(nodeList, nodeFilter)
  }

  def toSeq(document: Document, xpath: String): Seq[Node] = {
    toSeqNodes(toNodeList(document, xpath))
  }

  def nodeElementValue(node: Node, xpath: String): Option[String] = {
    nodeElements(node, xpath)
      .headOption
      .map(_.getFirstChild)
      .filter(_ != null)
      .map(_.getTextContent)
  }

  def nodeElementMap(node: Node, xpath: String): Map[String, String] = {
    val elements = nodeElements(node, xpath)
    val tuplets = mapToSeqTuples(elements)
    val o = tuplets.map(toMapOf)
    o.foldLeft(Map.empty[String, String])((a, b) => a ++ b)
  }

  def toMapOf(nodeSeq: Seq[(String, String, Node)]): Map[String, String] = {
    val markersMap: Map[String, String] = nodeSeq
      .map(in => (in._1, in._2))
      .map(t => (t._1, t._2.trim))
      .foldLeft(Map.empty[String, String])(_ + _)
    markersMap
  }

  def nodeElements(node: Node, xpath: String): Seq[Node] = {
    if (xpath.startsWith("/")) {
      throw new IllegalStateException("only relative xpathes are allowed for nodes")
    }
    toSeqNodes(toNodeList(node, xpath))
  }

  def nodeComments(node: Node, xpath: String): Seq[Node] = {
    if (xpath.startsWith("/")) {
      throw new IllegalStateException("only relative xpathes are allowed for nodes")
    }
    val list = toNodeList(node, xpath)
    toSeqNodesF(list, nodeCommentFilter)
  }

  private def toNodeList(doc: AnyRef, xpath: String): NodeList = {
    val xpathInstance = XPathFactory.newInstance().newXPath()
    xpathInstance.compile(xpath).evaluate(doc, XPathConstants.NODESET).asInstanceOf[NodeList]
  }

  case class InvalidPomXmlException(file: File, parent: Exception) extends IllegalStateException(file.getAbsolutePath, parent)

}
