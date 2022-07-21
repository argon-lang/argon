package dev.argon.plugins.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object Astring {
  @js.native
  @JSImport("astring", "generate")
  def generate(ast: js.Any): String = js.native
}
