package dev.argon.backend.sjs

import dev.argon.util.async.AsyncIterableTools.AsyncIterable

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

trait BinaryResource[E] extends js.Object {
  val fileName: js.UndefOr[String]
  def asBytes(): AsyncIterable[Uint8Array]
}
