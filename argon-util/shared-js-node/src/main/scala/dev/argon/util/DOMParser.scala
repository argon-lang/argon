package dev.argon.util

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSName}

@js.native
@JSImport("xmldom", name = "DOMParser")
private[util] class DOMParser() extends js.Object {
  def parseFromString(@unused xmlsource: String, @unused mimetype: String): Document = js.native
}

@js.native
private[util] trait Node extends js.Object {
  val prefix: String
  val nodeType: Int
  val attributes: NamedNodeMap
  val firstChild: Node
  val nextSibling: Node
  val nodeValue: String
}

@js.native
private[util] trait Element extends Node {
  val tagName: String
}

@js.native
private[util] trait Attr extends Node {
  val name: String
}

@js.native
private[util] trait Document extends Node {
  val documentElement: Element
}

@js.native
private[util] trait NamedNodeMap extends js.Object {
  val length: Int

  @JSName("item")
  def apply(index: Int): Node

}
