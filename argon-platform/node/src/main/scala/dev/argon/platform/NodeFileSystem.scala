package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.TypedArray

@js.native
@JSImport("node:fs/promises", JSImport.Namespace)
private[platform] object NodeFileSystem extends js.Object {
  def open(path: String, flags: "r" | "w"): js.Promise[NodeFileHandle] = js.native
  def readdir(path: String): js.Promise[js.Array[String]] = js.native
  def stat(path: String): js.Promise[NodeStats] = js.native
}

private[platform] trait NodeFileHandle extends js.Object {
  def close(): js.Promise[Unit]
  def read(buffer: TypedArray[?, ?]): js.Promise[NodeFileHandleReadResult]
  def write(buffer: TypedArray[?, ?]): js.Promise[NodeFileHandleWriteResult]
}

private[platform] trait NodeFileHandleReadResult extends js.Object {
  val bytesRead: Int
  val buffer: js.Any
}

private[platform] trait NodeFileHandleWriteResult extends js.Object {
  val bytesWritten: Int
  val buffer: js.Any
}

private[platform] trait NodeStats extends js.Object {
  def isDirectory(): Boolean
}

