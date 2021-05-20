package dev.argon.platform

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|

@js.native
@JSImport("fs", JSImport.Namespace)
private[platform] object NodeFileSystem extends js.Any {

  def readFile(@unused path: String, @unused encoding: String, @unused callback: js.Function2[Any, String, Any]): Unit = js.native
  def stat(@unused path: String, @unused callback: js.Function2[Any, NodeStats, Unit]): Unit = js.native
  def readdir(@unused path: String, @unused callback: js.Function2[Any, js.Array[String], Unit]): Unit = js.native
  def mkdir(@unused path: String, @unused options: NodeFSMkdirOptions, @unused callback: js.Function1[Any, Unit]): Unit = js.native


  def open(@unused path: String, @unused flags: String | Double, @unused callback: js.Function2[Any, Integer, Any]): Unit = js.native
  def read(@unused fd: Integer, @unused buffer: Uint8Array, @unused offset: Int, @unused length: Int, @unused position: Integer, @unused callback: js.Function3[Any, Integer, Any, Any]): Unit = js.native
  def write(@unused fd: Integer, @unused buffer: Uint8Array, @unused callback: js.Function3[Any, Integer, Any, Any]): Unit = js.native
  def close(@unused fd: Integer, @unused callback: js.Function1[Any, Any]): Unit = js.native

}

trait NodeFSMkdirOptions extends js.Object {
  val recursive: js.UndefOr[Boolean]
}
