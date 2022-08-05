package dev.argon.plugins.js

import scala.scalajs.js.annotation.JSImport
import scalajs.js

@js.native
@JSImport("console", "Console")
class NodeConsole(stdout: NodeWritableStream) extends js.Object
