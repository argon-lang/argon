package dev.argon.io

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal, JSImport}

@js.native
@JSGlobal("Buffer")
class NodeBuffer extends js.Object {
  @JSBracketAccess
  def apply(index: Int): Short = js.native

  def length: Int = js.native
}

@js.native
@JSGlobal("Buffer")
object NodeBuffer extends js.Object {
  def from(array: js.Array[Int]): NodeBuffer = js.native
}
