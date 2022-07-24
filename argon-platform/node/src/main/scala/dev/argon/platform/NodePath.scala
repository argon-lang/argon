package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:path", JSImport.Namespace)
private[platform] object NodePath extends js.Object {
  def dirname(path: String): String = js.native
  def join(paths: String*): String = js.native
}
