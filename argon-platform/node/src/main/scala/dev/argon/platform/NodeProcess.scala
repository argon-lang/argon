package dev.argon.platform

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal}

@js.native
@JSGlobal("process")
private[platform] object NodeProcess extends js.Object {
  val env: NodeProcessEnvironment = js.native
  val argv: js.Array[String] = js.native
}

@js.native
private[platform] trait NodeProcessEnvironment extends js.Object {
  @JSBracketAccess
  def apply(@unused name: String): js.UndefOr[String] = js.native
}
