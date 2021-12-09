package dev.argon.io

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|
import scala.scalajs.js.Promise

@js.native
@JSImport("fs/promises", JSImport.Namespace)
private[io] object NodeFileSystem extends js.Any {

  def stat(@unused path: String): Promise[NodeStats] = js.native
  def readdir(@unused path: String, @unused options: NodeReadDirOptions): Promise[NodeDirent] = js.native

  def open(@unused path: String, @unused flags: String | Double): Promise[NodeFileHandle] = js.native

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
private[io] trait NodeDirent extends js.Object {
  def isDirectory(): Boolean
  val name: String
}

@js.native
private[io] trait NodeFileHandle extends js.Object {

  def read(@unused buffer: Uint8Array, @unused offset: Int, @unused length: Int, @unused position: Integer)
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
