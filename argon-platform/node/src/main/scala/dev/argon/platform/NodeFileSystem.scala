package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.{|}

@js.native
@JSImport("fs", JSImport.Namespace)
private[platform] object NodeFileSystem extends js.Any {

  def readFile(path: String, encoding: String, callback: js.Function2[Any, String, Any]): Unit = js.native
  def stat(path: String, callback: js.Function2[Any, NodeStats, Unit]): Unit = js.native
  def readdir(path: String, callback: js.Function2[Any, js.Array[String], Unit]): Unit = js.native
  def mkdir(path: String, options: NodeFSMkdirOptions, callback: js.Function1[Any, Unit]): Unit = js.native


  def open(path: String, flags: String | Double, callback: js.Function2[Any, Integer, Any]): Unit = js.native
  def read(fd: Integer, buffer: Uint8Array, offset: Int, length: Int, position: Integer, callback: js.Function3[Any, Integer, Any, Any]): Unit = js.native
  def write(fd: Integer, buffer: Uint8Array, callback: js.Function3[Any, Integer, Any, Any]): Unit = js.native
  def close(fd: Integer, callback: js.Function1[Any, Any]): Unit = js.native

}

trait NodeFSMkdirOptions extends js.Object {
  val recursive: js.UndefOr[Boolean]
}

object NodeFSMkdirOptions {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def apply(recursive: js.UndefOr[Boolean] = js.undefined): NodeFSMkdirOptions =
    js.Dynamic.literal(
      recursive = recursive
    ).asInstanceOf[NodeFSMkdirOptions]
}
