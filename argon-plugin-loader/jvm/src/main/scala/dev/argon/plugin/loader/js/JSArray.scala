package dev.argon.plugin.loader.js

import org.graalvm.polyglot.{Context, Value}
import zio.*

import scala.reflect.ClassTag


final class JSArray[A] private (private val value: Value)

object JSArray {
  given[A]: ValueEncoder[JSArray[A]] with
    override def encode(value: JSArray[A]): Value = value.value
  end given

  given[A]: ValueDecoder[JSArray[A]] with
    override def decode(value: Value): JSArray[A] = JSArray(value)
  end given
  
  def fromScala[A: ValueEncoder](a: scala.Array[A])(using context: Context): JSArray[A] = {
    val ta = context.eval("js", "n => new Array(n)").nn.execute(a.length).nn
    for i <- a.indices do
      ta.setArrayElement(i, ValueEncoder[A].encode(a(i)))
    ValueDecoder[JSArray[A]].decode(ta)
  }

  def fromChunk[A: ValueEncoder](chunk: Chunk[A])(using context: Context): JSArray[A] = {
    val ta = context.eval("js", "n => new Array(n)").nn.execute(chunk.size).nn
    for i <- chunk.indices do
      ta.setArrayElement(i, ValueEncoder[A].encode(chunk(i)))
    ValueDecoder[JSArray[A]].decode(ta)
  }

  def toScala[A: ValueDecoder: ClassTag](a: JSArray[A]): scala.Array[A] = {
    val arrayValue = ValueEncoder[JSArray[A]].encode(a)
    val arr = new scala.Array[A](arrayValue.getArraySize.toInt)
    for i <- arr.indices do
      arr(i) = ValueDecoder[A].decode(arrayValue.getArrayElement(i).nn)
    arr
  }

  def toByteChunk[A: ValueDecoder](a: JSArray[A]): Chunk[A] = {
    val arrayValue = ValueEncoder[JSArray[A]].encode(a)
    val size = arrayValue.getArraySize.toInt
    val cb = ChunkBuilder.make[A](size)
    for i <- 0 until size do
      cb.addOne(ValueDecoder[A].decode(arrayValue.getArrayElement(i).nn))
    cb.result()
  }
}
