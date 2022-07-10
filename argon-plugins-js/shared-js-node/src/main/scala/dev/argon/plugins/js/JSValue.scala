package dev.argon.plugins.js

import scala.scalajs.js
import scala.scalajs.js.JSConverters.given

private[js] type JSValue = js.Any

object JSValue {
  def fromMap(m: Map[String, JSValue]): JSValue = m.toJSDictionary
  def fromSeq(s: Seq[JSValue]): JSValue = s.toJSArray
  def fromBigInt(i: BigInt): JSValue = js.BigInt(i.toString())

  def nullValue: JSValue = null
}


