package dev.argon.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("path", JSImport.Namespace)
private[io] object JSPath extends js.Any {

  def resolve(paths: String*): String = js.native

}
