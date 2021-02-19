package dev.argon.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport


@js.native
@JSImport("path", JSImport.Default)
private[io] object NodePath extends js.Any {
  def basename(path: String, ext: js.UndefOr[String] = js.undefined): String = js.native
  def extname(path: String): String = js.native
  def parse(path: String): NodeParsedPath = js.native
  def join(paths: String*): String = js.native
}

@js.native
private[io] trait NodeParsedPath extends js.Any {
  val dir: String
  val root: String
  val base: String
  val name: String
  val ext: String
}
