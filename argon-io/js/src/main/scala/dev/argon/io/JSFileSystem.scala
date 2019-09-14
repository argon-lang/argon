package dev.argon.io

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|

@js.native
@JSImport("fs", JSImport.Namespace)
private[io] object JSFileSystem extends js.Any {

  def readFile(path: String, encoding: String, callback: js.Function2[js.Error, String, Any]): Unit = js.native
  def stat(path: String, callback: js.Function2[js.Error, JSStats, Unit]): Unit = js.native
  def readdir(path: String, callback: js.Function2[js.Error, js.Array[String], Unit]): Unit = js.native


  def open(path: String, flags: String | Double, callback: js.Function2[js.Error, Integer, Any]): Unit = js.native
  def read(fd: Integer, buffer: Uint8Array, offset: Int, length: Int, position: Integer, callback: js.Function3[js.Error, Integer, Any, Any]): Unit = js.native
  def write(fd: Integer, buffer: Uint8Array, callback: js.Function3[js.Error, Integer, Any, Any]): Unit = js.native
  def close(fd: Integer, callback: js.Function1[js.Error, Any]): Unit = js.native

}

