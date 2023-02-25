package dev.argon.plugin.loader.js

import org.graalvm.polyglot.{Context, Value}
import zio.*

import scala.collection.mutable


final class JSMap[K, V] private (private val value: Value)

object JSMap {
  given[K, V]: ValueEncoder[JSMap[K, V]] with
    override def encode(value: JSMap[K, V]): Value = value.value
  end given

  given[K, V]: ValueDecoder[JSMap[K, V]] with
    override def decode(value: Value): JSMap[K, V] = JSMap(value)
  end given


  def fromScala[K: ValueEncoder, V: ValueEncoder](a: Map[K, V])(using context: Context): JSMap[K, V] = {
    val m = context.eval("js", "() => new Map()").nn.execute().nn
    for (k, v) <- a do
      context.eval("js", "(m, k, v) => m.put(k, v)").nn.executeVoid(m, ValueEncoder[K].encode(k), ValueEncoder[V].encode(v))

    ValueDecoder[JSMap[K, V]].decode(m)
  }

  def toScala[K: ValueDecoder, V: ValueDecoder](a: JSMap[K, V])(using context: Context): Map[K, V] = {
    val m = mutable.Map[K, V]()

    def f(k: K, v: V): Unit =
      m(k) = v

    context.eval("js", "(m, f) => m.forEach(f)").nn.executeVoid(ValueEncoder[JSMap[K, V]].encode(a), ValueEncoder[(K, V) => Unit].encode(f))

    m.toMap
  }
}
