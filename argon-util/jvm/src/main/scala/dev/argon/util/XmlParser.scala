package dev.argon.util

import zio.{IO, Task}
import scala.xml.*

object XmlParser {

  @SuppressWarnings(Array("scalafix:Disable.effect"))
  def parseString(xml: String): Task[Elem] = IO.effect { XML.loadString(xml) }

}
