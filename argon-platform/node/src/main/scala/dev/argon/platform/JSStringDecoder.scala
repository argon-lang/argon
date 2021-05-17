package dev.argon.platform

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("string_decoder", "StringDecoder")
private[platform] class JSStringDecoder(@unused encoding: js.UndefOr[String] = js.undefined) extends js.Object {
  def write(@unused buffer: Uint8Array): String = js.native
  def end(@unused buffer: js.UndefOr[Uint8Array] = js.undefined): String = js.native
}
