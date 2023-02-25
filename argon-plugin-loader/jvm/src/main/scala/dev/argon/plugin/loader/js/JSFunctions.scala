package dev.argon.plugin.loader.js

import scala.util.NotGiven
import org.graalvm.polyglot.{Value, Context as JSContext}

sealed trait JSFunctionBase

@FunctionalInterface
trait RawJSFunction0 extends JSFunctionBase {
  def apply(): Value
}

@FunctionalInterface
trait RawJSFunction1 extends JSFunctionBase {
  def apply(arg0: Value): Value
}

@FunctionalInterface
trait RawJSFunction2 extends JSFunctionBase {
  def apply(arg0: Value, arg1: Value): Value
}

given function0ValueEncoder[U: ValueEncoder](using jsContext: JSContext): ValueEncoder[() => U] with
  override def encode(value: () => U): Value =
    val f: RawJSFunction0 = () => ValueEncoder[U].encode(value())
    jsContext.asValue(f).nn
  end encode
end function0ValueEncoder

given function0ValueDecoder[U: ValueDecoder](using jsContext: JSContext): ValueDecoder[() => U] with
  override def decode(value: Value): () => U = () => {
    val f = value.as(classOf[RawJSFunction0]).nn
    ValueDecoder[U].decode(f())
  }
end function0ValueDecoder

given function1ValueEncoder[T0: ValueDecoder, U: ValueEncoder](using jsContext: JSContext): ValueEncoder[T0 => U] with
  override def encode(value: T0 => U): Value =
    val f: RawJSFunction1 = arg0 => ValueEncoder[U].encode(value(ValueDecoder[T0].decode(arg0)))
    jsContext.asValue(f).nn
  end encode
end function1ValueEncoder

given function1ValueDecoder[T0: ValueEncoder, U: ValueDecoder](using jsContext: JSContext): ValueDecoder[T0 => U] with
  override def decode(value: Value): T0 => U = arg0 => {
    val f = value.as(classOf[RawJSFunction1]).nn
    ValueDecoder[U].decode(f(ValueEncoder[T0].encode(arg0)))
  }
end function1ValueDecoder

given function2ValueEncoder[T0: ValueDecoder, T1: ValueDecoder, U: ValueEncoder](using jsContext: JSContext): ValueEncoder[(T0, T1) => U] with
  override def encode(value: (T0, T1) => U): Value =
    val f: RawJSFunction2 = (arg0, arg1) => ValueEncoder[U].encode(value(ValueDecoder[T0].decode(arg0), ValueDecoder[T1].decode(arg1)))
    jsContext.asValue(f).nn
  end encode
end function2ValueEncoder

given function2ValueDecoder[T0: ValueEncoder, T1: ValueEncoder, U: ValueDecoder](using jsContext: JSContext): ValueDecoder[(T0, T1) => U] with
  override def decode(value: Value): (T0, T1) => U = (arg0, arg1) => {
    val f = value.as(classOf[RawJSFunction2]).nn
    ValueDecoder[U].decode(f(ValueEncoder[T0].encode(arg0), ValueEncoder[T1].encode(arg1)))
  }
end function2ValueDecoder
