package dev.argon.util

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("os", JSImport.Default)
@js.native
object NodeOS extends js.Object {
  val EOL: String = js.native
}
