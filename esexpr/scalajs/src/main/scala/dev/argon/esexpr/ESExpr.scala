package dev.argon.esexpr

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

trait Constructed extends js.Object {
  val `type`: "constructed"
  val constructor: String
  val kwargs: js.Map[String, ESExpr]
  val args: js.Array[ESExpr]
}

trait Float32 extends js.Object {
  val `type`: "float32"
  val value: Float
}

type ESExpr = Constructed | Boolean | js.BigInt | String | Uint8Array | Float32 | Double | Null
