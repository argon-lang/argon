package dev.argon.js_module_extractor

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("esprima", JSImport.Namespace)
object Esprima extends js.Object {

  def parseModule(code: String): js.Object = js.native

}
