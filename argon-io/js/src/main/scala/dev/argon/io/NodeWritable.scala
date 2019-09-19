package dev.argon.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("stream", "Writable")
abstract class NodeWritable extends js.Object {

  def write(buffer: NodeBuffer, encoding: String, callback: js.Function1[Any, Unit]): Unit = js.native
  def destroy(err: js.UndefOr[js.Error] = js.undefined): Unit = js.native

  protected def _write(buffer: NodeBuffer, encoding: String, callback: js.Function1[Any, Unit]): Unit
  protected def _destroy(err: js.Error, callback: js.Function1[Any, Unit]): Unit = js.native
  protected def _final(err: js.Error, callback: js.Function1[Any, Unit]): Unit = js.native

}
