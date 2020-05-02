package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("os", JSImport.Default)
@js.native
private[platform] object NodeOS extends js.Object {
  val EOL: String = js.native
}
