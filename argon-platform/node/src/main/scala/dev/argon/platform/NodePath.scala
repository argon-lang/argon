package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:path", JSImport.Namespace)
object NodePath extends js.Object {
  def join(paths: String*): String = js.native
}
