package dev.argon.util.xml

import scala.scalajs.js

final case class XMLException(cause: js.Error) extends Exception(cause.message)
