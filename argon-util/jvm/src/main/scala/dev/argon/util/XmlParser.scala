package dev.argon.util

import zio.{IO, Task}

import scala.xml._

object XmlParser {
  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def parseString(xml: String): Task[Elem] = IO.effect { XML.loadString(xml) }
}
