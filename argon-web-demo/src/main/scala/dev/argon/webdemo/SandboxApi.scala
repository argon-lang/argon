package dev.argon.webdemo

import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

class SandboxApi(private val messageHandler: String => Unit) extends js.Object {
  def appendOutput(text: String): Unit =
    messageHandler(text)
}
