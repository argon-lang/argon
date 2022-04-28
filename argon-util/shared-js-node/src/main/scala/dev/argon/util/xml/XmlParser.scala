package dev.argon.util.xml

import dev.argon.util.*
import zio.*
import zio.stream.*

import scala.scalajs.js.JavaScriptException

object XmlParser {

  def parse(xml: String): IO[XMLException, Element] =
    IO.attempt {
      val dom = new DOMParser()
      val doc = dom.parseFromString(xml, "text/xml")
      convertElem(doc.documentElement)
    }.refineOrDie {
      case JavaScriptException(ex: DOMException) => XMLException(ex)
    }

  def parse[R, E >: XMLException <: Throwable](stream: ZStream[R, E, Char]): ZIO[R, E, Element] =
    stream.run(ZSink.mkString).flatMap(parse)

  private def convertChildNodes(node: DomNode): Seq[Node] = {
    def convertSibs(node: DomNode | Null): List[Node] =
      if node eq null then Nil
      else
        node.nodeType match {
          // ELEMENT_NODE
          case 1 => convertElem(node.asInstanceOf[DomElement]) :: convertSibs(node.nextSibling)

          // TEXT_NODE, CDATA_SECTION_NODE
          case 3 | 4 => Characters(node.nodeValue) :: convertSibs(node.nextSibling)

          // COMMENT_NODE
          case 8 => convertSibs(node.nextSibling)

          case _ => throw new RuntimeException("Unexpected node type")
        }

    convertSibs(node.firstChild)
  }

  private def convertElem(elem: DomElement): Element =
    val ns = Option(elem.namespaceURI).getOrElse("")
    val name = Name(Namespace(ns), elem.tagName)
    Element(
      name,
      convertAttrMap(elem.attributes),
      convertChildNodes(elem)
    )
  end convertElem


  private def convertAttr(attributes: NamedNodeMap[DomAttr])(index: Int): Attribute =
    val attr = attributes(index)

    val ns = Option(attr.namespaceURI).getOrElse("")
    val name = Name(Namespace(ns), attr.name)

    Attribute(name, attr.nodeValue)
  end convertAttr

  private def convertAttrMap(attributes: NamedNodeMap[DomAttr]): Seq[Attribute] =
    (0 until attributes.length).map(convertAttr(attributes))


}
