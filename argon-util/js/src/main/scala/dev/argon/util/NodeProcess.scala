package dev.argon.util

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal}

@js.native
@JSGlobal("process")
object NodeProcess extends js.Object {
  val env: NodeProcessEnvironment = js.native
  val argv: js.Array[String] = js.native
}

@js.native
trait NodeProcessEnvironment extends js.Object {
  @JSBracketAccess
  def apply(name: String): js.UndefOr[String] = js.native
}
