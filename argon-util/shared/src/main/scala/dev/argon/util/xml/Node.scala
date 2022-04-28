package dev.argon.util.xml

import scala.collection.mutable

sealed trait Node {

  private[xml] def appendTextContent(sb: mutable.StringBuilder): Unit

  def textContent: String =
    val sb = new StringBuilder()
    appendTextContent(sb)
    sb.toString()
  end textContent

}
final case class Element(name: Name, attributes: Seq[Attribute], children: Seq[Node]) extends Node:
  private[xml] override def appendTextContent(sb: StringBuilder): Unit =
    for(child <- children) do
      child.appendTextContent(sb)

  private def collectChildren(tag: Name): PartialFunction[Node, Element] = {
    case element: Element if element.name == tag => element
  }

  def childrenByTag(tag: Name): Seq[Element] =
    children.collect(collectChildren(tag))

  def child(tag: Name): Option[Element] =
    children.collectFirst(collectChildren(tag))

  def attribute(name: Name): Option[Attribute] =
    attributes.find(_.name == name)

end Element

final case class Characters(text: String) extends Node:
  private[xml] override def appendTextContent(sb: StringBuilder): Unit =
    sb.append(text)
end Characters


