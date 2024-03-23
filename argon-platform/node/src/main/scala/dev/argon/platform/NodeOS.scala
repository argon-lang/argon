package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:os", JSImport.Namespace)
object NodeOS extends js.Object {
  val EOL: String = js.native
  
  def tmpdir(): String = js.native
}
