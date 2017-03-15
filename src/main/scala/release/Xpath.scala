package release

import javax.xml.xpath.{XPathConstants, XPathFactory}

import org.w3c.dom.{Document, Node, NodeList}

object Xpath {

  def onlyString(document: Document, xpath: String): Option[String] = {
    only(document, xpath).map(_.getTextContent)
  }

  def only(document: Document, xpath: String): Option[Node] = {
    val elements = toSeq(document, xpath)
    Util.onlyOpt(elements)
  }

  private def nodeFilter(node: Node): Option[Node] = node match {
    case element: Node if element.getNodeType == Node.ELEMENT_NODE ⇒ Some(node)
    case element: Node if element.getNodeType == Node.TEXT_NODE ⇒ None
    case element: Node if element.getNodeType == Node.COMMENT_NODE ⇒ None
    case elment ⇒ throw new IllegalStateException(elment.getClass.getCanonicalName)
  }

  private def convertToTuples(node: Node, nodeList: NodeList): Seq[(String, String, Node)] = {
    val length = nodeList.getLength
    if (length == 0) {
      throw new IllegalStateException("no elements found")
    }
    val tupleOpts: Seq[Option[(String, String, Node)]] = for (i ← Range(0, length)) yield {
      val singleNote: Node = nodeList.item(i)
      val nodeTupleOpt: Option[(String, String, Node)] = nodeFilter(singleNote)
        .map(element ⇒ (element.getNodeName, element.getTextContent, node))
      nodeTupleOpt
    }
    tupleOpts.flatten
  }

  private def mapToSeqTuples(nodes: Seq[Node]): Seq[Seq[(String, String, Node)]] = {
    nodes.map(in ⇒ convertToTuples(in, in.getChildNodes))
  }

  def toSeqTuples(document: Document, xpath: String): Seq[Seq[(String, String, Node)]] = {
    mapToSeqTuples(toSeq(document, xpath))
  }

  def toSeqNodes(nodeList: NodeList): Seq[Node] = {
    List.tabulate(nodeList.getLength)(i ⇒ i).map(key ⇒ nodeList.item(key)).flatMap(nodeFilter)
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
    o.foldLeft(Map.empty[String, String])((a, b) ⇒ a ++ b)
  }

  def toMapOf(nodeSeq: Seq[(String, String, Node)]): Map[String, String] = {
    val markersMap: Map[String, String] = nodeSeq
      .map(in ⇒ (in._1, in._2))
      .map(t ⇒ (t._1, t._2.trim))
      .foldLeft(Map.empty[String, String])(_ + _)
    markersMap
  }

  def nodeElements(node: Node, xpath: String): Seq[Node] = {
    if (xpath.startsWith("/")) {
      throw new IllegalStateException("only relative xpathes are allowed for nodes")
    }
    toSeqNodes(toNodeList(node, xpath: String))
  }

  private def toNodeList(doc: AnyRef, xpath: String): NodeList = {
    val xpathInstance = XPathFactory.newInstance().newXPath()
    xpathInstance.compile(xpath).evaluate(doc, XPathConstants.NODESET).asInstanceOf[NodeList]
  }

}
