package dev.argon

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("process")
object NodeProcess extends js.Object {
  val argv: js.Array[String] = js.native
}
