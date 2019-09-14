package dev.argon.build.testrunner.js

import scala.scalajs.js.annotation.JSImport
import scalajs.js

@js.native
@JSImport("memory-streams", "WritableStream")
class MemoryWritableStream() extends NodeWritableStream

@js.native
trait NodeWritableStream extends js.Object {
  def write(chunk: js.Any): Boolean = js.native
}
