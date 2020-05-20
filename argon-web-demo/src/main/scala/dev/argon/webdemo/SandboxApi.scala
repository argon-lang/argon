package dev.argon.webdemo

import scala.scalajs.js

class SandboxApi(private val messageHandler: String => Unit) extends js.Object {
  def appendOutput(text: String): Unit =
    messageHandler(text)
}
