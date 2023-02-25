package dev.argon.plugin.loader.js
import org.graalvm.polyglot.{Value, Context as JSContext}

abstract class JSTupleEncoder[T <: Tuple](using jsContext: JSContext) extends ValueEncoder[T] {
  def tupleSize: Int
  def encodeWithOffset(value: T, arr: Value, offset: Int): Unit

  override def encode(value: T): Value =
    val arr = jsContext.eval("js", "n => new Array(n)").nn.execute(tupleSize).nn
    encodeWithOffset(value, arr, 0)
    arr
  end encode
}

given (using jsContext: JSContext): JSTupleEncoder[EmptyTuple] with
  override def tupleSize: Int = 0

  override def encodeWithOffset(value: EmptyTuple, arr: Value, offset: Int): Unit = ()
end given

given [H: ValueEncoder, T <: Tuple: JSTupleEncoder](using jsContext: JSContext): JSTupleEncoder[H *: T] with
  override def tupleSize: Int = summon[JSTupleEncoder[T]].tupleSize + 1

  override def encodeWithOffset(value: H *: T, arr: Value, offset: Int): Unit =
    val (h *: t) = value
    arr.setArrayElement(offset, summon[ValueEncoder[H]].encode(h))
    summon[ValueEncoder[T]].encodeWithOffset(t, arr, offset + 1)
  end encodeWithOffset
end given

