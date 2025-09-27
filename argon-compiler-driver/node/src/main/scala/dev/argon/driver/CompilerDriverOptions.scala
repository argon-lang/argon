package dev.argon.driver

import scala.scalajs.js

trait CompilerDriverJSOptions extends js.Object {
  def pluginDirectories: js.Array[String]
  def arguments: js.Array[String]
}
