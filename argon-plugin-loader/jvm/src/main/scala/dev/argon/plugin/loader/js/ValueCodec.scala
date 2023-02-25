package dev.argon.plugin.loader.js

import org.graalvm.polyglot.{Value, Context as JSContext}
import dev.argon.util.{*, given}

import scala.util.NotGiven

trait ValueEncoder[T] {
  def encode(value: T): Value
}

object ValueEncoder {
  def apply[T](using codec: ValueEncoder[T]): ValueEncoder[T] = codec
}

trait ValueDecoder[T] {
  def decode(value: Value): T
}

object ValueDecoder {
  def apply[T](using codec: ValueDecoder[T]): ValueDecoder[T] = codec
}

given ValueEncoder[Value] with
  override def encode(value: Value): Value = value
end given

given ValueDecoder[Value] with
  override def decode(value: Value): Value = value
end given

given (using jsContext: JSContext): ValueEncoder[Unit] with
  override def encode(value: Unit): Value =
    jsContext.eval("js", "undefined").nn
end given

given ValueDecoder[Unit] with
  override def decode(value: Value): Unit = ()
end given

given [A: ValueEncoder](using jsContext: JSContext): ValueEncoder[A | Null] with
  override def encode(value: A | Null): Value =
    if value == null then
      jsContext.asValue(null).nn
    else
      ValueEncoder[A].encode(value)
end given

given valueDecoderNullable[A: ValueDecoder]: ValueDecoder[A | Null] with
  override def decode(value: Value): A | Null =
    if value.isNull then
      null
    else
      ValueDecoder[A].decode(value)
end valueDecoderNullable

given (using jsContext: JSContext): ValueEncoder[String] with
  override def encode(value: String): Value = jsContext.asValue(value).nn
end given

given ValueDecoder[String] with
  override def decode(value: Value): String = value.asString().nn
end given

given [S <: String & Singleton](using jsContext: JSContext): ValueEncoder[S] with
  override def encode(value: S): Value = jsContext.asValue(value).nn
end given

given [S <: String & Singleton]: ValueDecoder[S] with
  override def decode(value: Value): S = value.asString().asInstanceOf[S]
end given

given (using jsContext: JSContext): ValueEncoder[BigInt] with
  override def encode(value: BigInt): Value =
    jsContext.eval("js", "BigInt").nn.execute(value.toString()).nn
end given

given (using jsContext: JSContext): ValueDecoder[BigInt] with
  override def decode(value: Value): BigInt =
    BigInt(jsContext.eval("js", "n => n.toString()").nn.execute(value).nn.asString().nn)
end given

given (using jsContext: JSContext): ValueEncoder[Boolean] with
  override def encode(value: Boolean): Value =
    jsContext.asValue(value).nn
end given

given ValueDecoder[Boolean] with
  override def decode(value: Value): Boolean =
    value.asBoolean()
end given
