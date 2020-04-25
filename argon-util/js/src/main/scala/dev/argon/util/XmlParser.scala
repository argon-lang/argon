package dev.argon.util

import scala.xml.{PrefixedAttribute, UnprefixedAttribute, Elem => SElem, MetaData => SMetaData, Node => SNode, Text => SText, Null => SAttrNull}

object XmlParser {
  def parseString(xml: String): SElem = {
    val dom = new DOMParser()
    val doc = dom.parseFromString(xml, "text/xml")
    convertElem(doc.documentElement)
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Null", "org.wartremover.warts.Equals"))
  private def convertChildNodes(node: Node): Seq[SNode] = {
    def convertSibs(node: Node): List[SNode] =
      if(node eq null) Nil
      else node.nodeType match {
          // ELEMENT_NODE
          case 1 => convertElem(node.asInstanceOf[Element]) :: convertSibs(node.nextSibling)

          // TEXT_NODE, CDATA_SECTION_NODE
          case 3 | 4 => SText(node.nodeValue) :: convertSibs(node.nextSibling)

          // COMMENT_NODE
          case 8 => convertSibs(node.nextSibling)

          case _ => throw new RuntimeException("Unexpected node type")
        }

    convertSibs(node.firstChild)
  }

  private def convertElem(elem: Element): SElem = {
    SElem(
      elem.prefix,
      elem.tagName,
      convertAttrMap(elem.attributes, 0),
      scala.xml.TopScope,
      false,
      convertChildNodes(elem): _*
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Null", "org.wartremover.warts.Equals"))
  def convertAttrMap(attributes: NamedNodeMap, index: Int): SMetaData =
    if(index < attributes.length) {
      val attr = attributes(index).asInstanceOf[Attr]

      val attrValue = Seq(SText(attr.nodeValue))

      if(attr.prefix eq null)
        new UnprefixedAttribute(attr.name, attrValue, convertAttrMap(attributes, index + 1))
      else
        new PrefixedAttribute(attr.prefix, attr.name, attrValue, convertAttrMap(attributes, index + 1))
    }
    else SAttrNull

}
