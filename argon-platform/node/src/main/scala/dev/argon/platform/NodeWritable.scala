package dev.argon.platform

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("stream", "Writable")
private[platform] abstract class NodeWritable() extends js.Object {

  def write(@unused buffer: NodeBuffer, @unused encoding: String, @unused callback: js.Function1[Any, Unit]): Unit = js.native
  def destroy(@unused err: js.UndefOr[Any] = js.undefined): Unit = js.native

  protected def _write(@unused buffer: NodeBuffer, @unused encoding: String, @unused callback: js.Function1[Any, Unit]): Unit
  protected def _destroy(@unused err: Any, @unused callback: js.Function1[Any, Unit]): Unit = js.native
  protected def _final(@unused err: Any, @unused callback: js.Function1[Any, Unit]): Unit = js.native

}

