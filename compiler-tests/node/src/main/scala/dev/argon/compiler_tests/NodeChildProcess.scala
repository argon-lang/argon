package dev.argon.compiler_tests

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import dev.argon.util.async.AsyncIterableTools.AsyncIterable
import scala.scalajs.js.typedarray.Uint8Array


trait NodeChildProcess extends js.Object {
  def exitCode: Int | Null
}

object NodeChildProcess {
  @js.native
  @JSImport("node:child_process")
  def execFile(file: String, args: js.Array[String], options: ExecFileOptions, callback: js.Function3[js.Any | Null, String, String, Unit]): NodeChildProcess = js.native

  trait ExecFileOptions extends js.Object {
    val cwd: js.UndefOr[String] = js.undefined
  }
}
