package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("stream", "Readable")
private[platform] class NodeReadable extends js.Object {

  def on(event: "end", callback: js.Function0[Unit]): Unit = js.native

  def pipe(writable: NodeWritable): Unit = js.native
}
