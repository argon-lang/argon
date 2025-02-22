package dev.argon.io.jstypes.node

import dev.argon.util.async.AsyncIterableTools.AsyncIterable

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArray, DataView}

@js.native
@JSImport("node:fs/promises", JSImport.Namespace)
object NodeFileSystem extends js.Object {
  def open(path: String, flags: "r" | "w"): js.Promise[NodeFileHandle] = js.native
  def readdir(path: String): js.Promise[js.Array[String]] = js.native
  def stat(path: String): js.Promise[NodeStats] = js.native
  def mkdtemp(prefix: String): js.Promise[String] = js.native
  def rm(path: String, options: RMOptions): js.Promise[Unit] = js.native
  def writeFile(file: String, data: String | js.typedarray.TypedArray[?, ?]): js.Promise[Unit] = js.native
  def mkdir(path: String, options: MkDirOptions): js.Promise[Unit] = js.native

  trait RMOptions extends js.Object {
    val maxRetries: js.UndefOr[Int] = js.undefined
    val recursive: js.UndefOr[Boolean] = js.undefined
    val retryDelay: js.UndefOr[Int] = js.undefined
  }

  trait MkDirOptions extends js.Object {
    val recursive: js.UndefOr[Boolean] = js.undefined
  }
}

trait NodeFileHandle extends js.Object {
  def writeFile(data: TypedArray[?, ?] | DataView): js.Promise[Unit]
  def read(buffer: TypedArray[?, ?]): js.Promise[NodeFileHandleReadResult]
  def close(): js.Promise[Unit]
}

trait NodeFileHandleReadResult extends js.Object {
  val bytesRead: Int
  val buffer: js.Any
}

trait NodeFileHandleWriteResult extends js.Object {
  val bytesWritten: Int
  val buffer: js.Any
}

trait NodeStats extends js.Object {
  def isDirectory(): Boolean
}

