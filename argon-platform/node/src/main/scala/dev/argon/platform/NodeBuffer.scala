package dev.argon.platform

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal}

@js.native
@JSGlobal("Buffer")
private[platform] class NodeBuffer extends js.Object {
  @JSBracketAccess
  def apply(index: Int): Short = js.native

  def length: Int = js.native
}

@js.native
@JSGlobal("Buffer")
private[platform] object NodeBuffer extends js.Object {
  def from(array: js.Array[Int]): NodeBuffer = js.native
}
