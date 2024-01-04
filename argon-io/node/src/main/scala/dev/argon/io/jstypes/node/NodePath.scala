package dev.argon.io.jstypes.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object NodePath {
  @js.native
  @JSImport("node:path")
  def join(paths: String*): String = js.native

  @js.native
  @JSImport("node:path")
  def dirname(path: String): String = js.native
}
