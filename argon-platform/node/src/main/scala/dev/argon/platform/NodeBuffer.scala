package dev.argon.platform

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal}

@js.native
@JSGlobal("Buffer")
private[platform] class NodeBuffer extends js.Object {
  @JSBracketAccess
  def apply(@unused index: Int): Short = js.native

  def length: Int = js.native
}

@js.native
@JSGlobal("Buffer")
private[platform] object NodeBuffer extends js.Object {
  def from(@unused array: js.Array[Int]): NodeBuffer = js.native
}
