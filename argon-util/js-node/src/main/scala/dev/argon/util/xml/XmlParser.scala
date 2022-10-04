package dev.argon.util.xml

import dev.argon.util.{*, given}
import zio.*
import zio.stream.*
import typings.xmldomXmldom.mod.DOMParser
import typings.std.{NamedNodeMap, Element as DomElement, Node as DomNode}

import scala.scalajs.js
import scala.scalajs.js.JavaScriptException

object XmlParser {

  private def removeUndefined[A](value: A | Unit): Option[A] =
    value.toOption

  private def optional[A](value: A | Null): Option[A] =
    removeUndefined[A | Null](value)
      .flatMap { value => Nullable(value).toOption }

  def parse(xml: String): IO[XMLException, Element] =
    ZIO.attempt {
      val doc = DOMParser.newInstance0().parseFromString(xml, "text/xml")
      convertElem(doc.documentElement)
    }.catchAll {
      case jsErr @ JavaScriptException(ex) =>
        ex.asInstanceOf[Matchable] match {
          case ex: js.Error => ZIO.fail(XMLException(ex))
          case _ => ZIO.die(jsErr)
        }

      case ex => ZIO.die(ex)
    }

  def parse[R, E >: XMLException <: Throwable](stream: ZStream[R, E, Char]): ZIO[R, E, Element] =
    stream.run(ZSink.mkString).flatMap(parse)

  private def convertChildNodes(node: DomNode): Seq[Node] = {
    def convertSibs(node: Option[DomNode]): List[Node] =
      node match
        case None => Nil
        case Some(node) =>
          node.nodeType match {
            // ELEMENT_NODE
            case 1 => convertElem(node.asInstanceOf[DomElement]) :: convertSibs(optional(node.nextSibling))

            // TEXT_NODE, CDATA_SECTION_NODE
            case 3 | 4 => Characters(optional(node.nodeValue).get) :: convertSibs(optional(node.nextSibling))

            // COMMENT_NODE
            case 8 => convertSibs(optional(node.nextSibling))

            case _ => throw new RuntimeException("Unexpected node type")
          }
      end match

    convertSibs(optional(node.firstChild))
  }

  private def convertElem(elem: DomElement): Element =
    val ns = optional(elem.namespaceURI).getOrElse("")
    val name = Name(Namespace(ns), elem.tagName)
    Element(
      name,
      convertAttrMap(elem.attributes),
      convertChildNodes(elem)
    )
  end convertElem


  private def convertAttr(attributes: NamedNodeMap)(index: Int): Attribute =
    val attr = attributes(index).get

    val ns = optional(attr.namespaceURI).getOrElse("")
    val name = Name(Namespace(ns), attr.name)

    Attribute(name, optional(attr.nodeValue).get)
  end convertAttr

  private def convertAttrMap(attributes: NamedNodeMap): Seq[Attribute] =
    (0 until attributes.length.toInt).map(convertAttr(attributes))


}
