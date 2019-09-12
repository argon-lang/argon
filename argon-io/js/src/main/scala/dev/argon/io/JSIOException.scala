package dev.argon.io

import java.io.IOException

import scala.scalajs.js

final case class JSIOException(error: js.Error) extends IOException {
  override def getMessage: String = error.message
}
