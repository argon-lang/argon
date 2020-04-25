package dev.argon.util

import scala.xml._

object XmlParser {
  def parseString(xml: String): Elem = XML.loadString(xml)
}
