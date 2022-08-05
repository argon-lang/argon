package dev.argon.plugins.js

import scala.scalajs.js.annotation.JSImport
import scalajs.js

@js.native
@JSImport("node:stream", "Writable")
abstract class NodeWritableStream() extends js.Object {
  protected def _write(chunk: js.Any, encoding: String, callback: js.Function1[js.UndefOr[js.Any], Unit]): Unit
  protected def _writev(chunks: js.Array[NodeWritableStream.Chunk], callback: js.Function1[js.UndefOr[js.Any], Unit]): Unit
}

object NodeWritableStream {
  trait Chunk extends js.Object {
    val chunk: js.Any
    val encoding: String
  }
}
