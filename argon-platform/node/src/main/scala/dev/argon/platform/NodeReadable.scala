package dev.argon.platform

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("stream", "Readable")
private[platform] class NodeReadable extends js.Object {

  def on(@unused event: "end", @unused callback: js.Function0[Unit]): Unit = js.native

  def pipe(@unused writable: NodeWritable): Unit = js.native
}
