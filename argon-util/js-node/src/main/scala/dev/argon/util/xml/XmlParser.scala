package dev.argon.util.xml

import dev.argon.util.{*, given}
import zio.*
import zio.stream.*

import scala.scalajs.js.JavaScriptException

object XmlParser {

  def parse(xml: String): IO[XMLException, Element] =
    ZIO.attempt {
      val dom = new DOMParser()
      val doc = dom.parseFromString(xml, "text/xml")
      convertElem(doc.documentElement)
    }.catchAll {
      case jsErr @ JavaScriptException(ex) =>
        ex.asInstanceOf[Matchable] match {
          case ex: DOMException => ZIO.fail(XMLException(ex))
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
            case 1 => convertElem(node.asInstanceOf[DomElement]) :: convertSibs(node.nextSibling.toOption.flatMap(Option.apply).map(_.nn))

            // TEXT_NODE, CDATA_SECTION_NODE
            case 3 | 4 => Characters(node.nodeValue.toOption.flatMap(Option.apply).get.nn) :: convertSibs(node.nextSibling.toOption.flatMap(Option.apply).map(_.nn))

            // COMMENT_NODE
            case 8 => convertSibs(node.nextSibling.toOption.flatMap(Option.apply).map(_.nn))

            case _ => throw new RuntimeException("Unexpected node type")
          }
      end match

    convertSibs(node.firstChild.toOption.flatMap(Option.apply).map(_.nn))
  }

  private def convertElem(elem: DomElement): Element =
    val ns = elem.namespaceURI.toOption.flatMap(Option.apply).map(_.nn).getOrElse("")
    val name = Name(Namespace(ns), elem.tagName)
    Element(
      name,
      convertAttrMap(elem.attributes),
      convertChildNodes(elem)
    )
  end convertElem


  private def convertAttr(attributes: NamedNodeMap[DomAttr])(index: Int): Attribute =
    val attr = attributes(index)

    val ns = attr.namespaceURI.toOption.flatMap(Option.apply).map[String](_.nn).getOrElse("")
    val name = Name(Namespace(ns), attr.name)

    Attribute(name, attr.nodeValue.toOption.flatMap(Option.apply).get.nn)
  end convertAttr

  private def convertAttrMap(attributes: NamedNodeMap[DomAttr]): Seq[Attribute] =
    (0 until attributes.length).map(convertAttr(attributes))


}
