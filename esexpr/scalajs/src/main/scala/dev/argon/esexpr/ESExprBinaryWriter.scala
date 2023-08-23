package dev.argon.esexpr

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import dev.argon.util.AsyncIterableTools.*

import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("@argon-lang/esexpr")
class ESExprBinaryWriter(symbolTable: js.Array[String]) extends js.Object {
  def write(expr: ESExpr): AsyncIterable[Uint8Array] = js.native
}
