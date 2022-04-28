package dev.argon.util.xml

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSName}


@js.native
@JSImport("@xmldom/xmldom", name = "DOMParser")
private[util] class DOMParser() extends js.Object {
  def parseFromString(@unused xmlsource: String, @unused mimetype: String): DomDocument = js.native
}

@js.native
@JSImport("@xmldom/xmldom", name = "DOMException")
class DOMException extends js.Error

@js.native
private[util] trait DomNode extends js.Object {
  val namespaceURI: String | Null
  val nodeType: Int
  val firstChild: DomNode | Null
  val nextSibling: DomNode | Null
  val nodeValue: String | Null
}

@js.native
private[util] trait DomElement extends DomNode {
  val attributes: NamedNodeMap[DomAttr]
  val tagName: String
}

@js.native
private[util] trait DomAttr extends DomNode {
  val name: String
}

@js.native
private[util] trait DomDocument extends DomNode {
  val documentElement: DomElement
}

@js.native
private[util] trait NamedNodeMap[TNode <: DomNode] extends js.Object {
  val length: Int

  @JSName("item")
  def apply(index: Int): TNode

}
