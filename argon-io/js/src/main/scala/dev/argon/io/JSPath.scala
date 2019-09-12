package dev.argon.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("path", JSImport.Namespace)
private[io] object JSPath extends js.Any {

  def basename(path: String, ext: js.UndefOr[String] = js.undefined): String = js.native
  def dirname(path: String): String = js.native
  def join(paths: String*): String = js.native
  def resolve(paths: String*): String = js.native

}
