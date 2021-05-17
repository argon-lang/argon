package dev.argon.io

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport


@js.native
@JSImport("path", JSImport.Default)
private[io] object NodePath extends js.Any {
  def basename(@unused path: String, @unused ext: js.UndefOr[String] = js.undefined): String = js.native
  def extname(@unused path: String): String = js.native
  def parse(@unused path: String): NodeParsedPath = js.native
  def join(@unused paths: String*): String = js.native
}

@js.native
private[io] trait NodeParsedPath extends js.Any {
  val dir: String
  val root: String
  val base: String
  val name: String
  val ext: String
}
