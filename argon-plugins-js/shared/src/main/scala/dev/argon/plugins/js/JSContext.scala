package dev.argon.plugins.js

import zio.*

final case class DecodedJSObject(map: Map[String, JSValue])
final case class DecodedJSArray(seq: Seq[JSValue])

trait JSContext {

  def fromMap(m: Map[String, JSValue]): JSValue
  def fromSeq(s: Seq[JSValue]): JSValue
  def fromString(s: String): JSValue
  def fromBoolean(b: Boolean): JSValue
  def fromDouble(d: Double): JSValue
  def fromBigInt(i: BigInt): JSValue
  def fromRegExp(r: JSRegExp): JSValue
  def fromNull: JSValue

  def decode(value: JSValue): DecodedJSObject | DecodedJSArray | String | Boolean | Double | BigInt | JSRegExp


  final def generate[A: JSValueCodec](a: A): UIO[String] =
    generateImpl(summon[JSValueCodec[A]].toJSValue(this)(a))

  final def parse(fileName: String, text: String): IO[String, estree.Program] =
    parseImpl(fileName, text).map(summon[JSValueCodec[estree.Program]].fromJSValue(this)).absolve

  protected def generateImpl(value: JSValue): UIO[String]
  protected def parseImpl(fileName: String, text: String): UIO[JSValue]
}

object JSContext extends JSContextPlatformSpecific
