package dev.argon.plugin.loader.js

import org.graalvm.polyglot.{Context, Value}
import zio.Chunk

final class Uint8Array private (private val value: Value)
object Uint8Array {
  given ValueEncoder[Uint8Array] with
    override def encode(value: Uint8Array): Value = value.value
  end given
  given ValueDecoder[Uint8Array] with
    override def decode(value: Value): Uint8Array = Uint8Array(value)
  end given
}

object TypedArrayUtil {
  def fromByteArray(a: scala.Array[Byte])(using context: Context): Uint8Array = {
    val ta = context.eval("js", "n => new TypedArray(n)").nn.execute(a.length).nn
    for i <- a.indices do
      ta.setArrayElement(i, a(i))
    ValueDecoder[Uint8Array].decode(ta)
  }

  def fromByteChunk(chunk: Chunk[Byte])(using context: Context): Uint8Array = fromByteArray(chunk.toArray)

  def toByteArray(a: Uint8Array): scala.Array[Byte] = {
    val ta = ValueEncoder[Uint8Array].encode(a)
    val arr = new scala.Array[Byte](ta.getArraySize.toInt)
    for i <- arr.indices do
      arr(i) = ta.getArrayElement(i).nn.asInt().toByte
    arr
  }

  def toByteChunk(a: Uint8Array): Chunk[Byte] = Chunk.fromArray(toByteArray(a))
}
