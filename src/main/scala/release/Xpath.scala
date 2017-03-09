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

  def toSeqTuples(document: Document, xpath: String): Seq[Seq[(String, String, Node)]] = {
    toSeq(document, xpath).map(in ⇒ convertToTuples(in, in.getChildNodes))
  }

  def toSeqNodes(nodeList: NodeList): Seq[Node] = {
    List.tabulate(nodeList.getLength)(i ⇒ i).map(key ⇒ nodeList.item(key)).flatMap(nodeFilter)
  }

  def toSeq(document: Document, xpath: String): Seq[Node] = {
    toSeqNodes(toNodeList(document, xpath))
  }

  private def toNodeList(doc: Document, value: String): NodeList = {
    val xpath = XPathFactory.newInstance().newXPath()
    xpath.compile(value).evaluate(doc, XPathConstants.NODESET).asInstanceOf[NodeList]
  }

}
