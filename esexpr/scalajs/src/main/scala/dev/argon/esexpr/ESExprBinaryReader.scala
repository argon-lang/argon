package dev.argon.esexpr

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import dev.argon.util.AsyncIterableTools.*

import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("@argon-lang/esexpr")
class ESExprBinaryReader(symbolTable: js.Array[String], iter: AsyncIterator[Uint8Array]) extends js.Object {
  def read(): js.Promise[ESExpr | Unit] = js.native
  def readAll(): AsyncIterator[ESExpr] = js.native
}
