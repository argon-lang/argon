package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("stream", "Writable")
private[platform] abstract class NodeWritable() extends js.Object {

  def write(buffer: NodeBuffer, encoding: String, callback: js.Function1[Any, Unit]): Unit = js.native
  def destroy(err: js.UndefOr[Any] = js.undefined): Unit = js.native

  protected def _write(buffer: NodeBuffer, encoding: String, callback: js.Function1[Any, Unit]): Unit
  protected def _destroy(err: Any, callback: js.Function1[Any, Unit]): Unit = js.native
  protected def _final(err: Any, callback: js.Function1[Any, Unit]): Unit = js.native

}

