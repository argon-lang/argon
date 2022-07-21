package dev.argon.plugins.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object Acorn {
  @js.native
  @JSImport("acorn", "parse")
  def parse(code: String, options: Options): js.Any = js.native


  trait Options extends js.Object {
    val ecmaVersion: Int
    val sourceType: js.UndefOr["module" | "script"] = js.undefined
    val locations: js.UndefOr[Boolean] = js.undefined
    val sourceFile: js.UndefOr[String] = js.undefined
  }
}
