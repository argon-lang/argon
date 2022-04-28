package dev.argon.io

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|
import scala.scalajs.js.Promise

@js.native
@JSImport("fs/promises", JSImport.Namespace)
private[io] object NodeFileSystem extends js.Object {

  def stat(@unused path: String): Promise[NodeStats] = js.native
  def readdir(@unused path: String): Promise[js.Array[String]] = js.native

  def open(@unused path: String, @unused flags: String | Double): Promise[NodeFileHandle] = js.native

  def access(@unused path: String): Promise[Unit] = js.native

}

@js.native
private[io] trait NodeReadDirOptions extends js.Object {
  val withFileTypes: true
}

@js.native
private[io] trait NodeStats extends js.Object {

  def isDirectory(): Boolean = js.native

}

@js.native
private[io] trait NodeFileHandle extends js.Object {

  def read(@unused buffer: Uint8Array, @unused offset: Int, @unused length: Int)
    : Promise[NodeFileHandleReadResult]

  def write(@unused buffer: Uint8Array): Promise[NodeFileHandleWriteResult]
  def close(): Promise[Unit]
}

private[io] trait NodeFileHandleReadResult extends js.Object {
  val bytesRead: Int
}

private[io] trait NodeFileHandleWriteResult extends js.Object {
  val bytesWritten: Int
}

@js.native
@JSImport("path", JSImport.Namespace)
private[io] object NodePath extends js.Object {
  def join(args: String*): String = js.native
}

