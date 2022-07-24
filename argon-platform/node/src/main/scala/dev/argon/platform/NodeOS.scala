package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:os", JSImport.Namespace)
private[platform] object NodeOS extends js.Object {
  val EOL: String = js.native
}
