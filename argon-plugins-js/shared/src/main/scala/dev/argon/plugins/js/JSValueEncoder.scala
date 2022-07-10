package dev.argon.plugins.js

import magnolia1.*
import dev.argon.util.{*, given}
import scala.reflect.TypeTest

trait JSValueEncoder[A] {
  def toJSValue(a: A): JSValue
  def skipForField(a: A): Boolean = false
}

object JSValueEncoder extends Derivation[JSValueEncoder]:
  given [S <: String]: JSValueEncoder[S] with
    override def toJSValue(a: S): JSValue = a
  end given

  given JSValueEncoder[Double] with
    override def toJSValue(a: Double): JSValue = a
  end given

  given JSValueEncoder[Int] with
    override def toJSValue(a: Int): JSValue = a
  end given
  
  given JSValueEncoder[BigInt] with
    override def toJSValue(a: BigInt): JSValue = JSValue.fromBigInt(a)
  end given

  given [B <: Boolean]: JSValueEncoder[B] with
    override def toJSValue(a: B): JSValue = a
  end given
  
  given JSValueEncoder[Nothing] with
    override def toJSValue(a: Nothing): JSValue = a
  end given

  given [A: JSValueEncoder]: JSValueEncoder[Seq[A]] with
    override def toJSValue(a: Seq[A]): JSValue =
      JSValue.fromSeq(a.map(summon[JSValueEncoder[A]].toJSValue))

  given [A: JSValueEncoder]: JSValueEncoder[Nullable[A]] with
    override def toJSValue(a: Nullable[A]): JSValue =
      a.fold(JSValue.nullValue, summon[JSValueEncoder[A]].toJSValue)
  end given

  given [A: JSValueEncoder]: JSValueEncoder[Option[A]] with
    override def toJSValue(a: Option[A]): JSValue =
      a.map(summon[JSValueEncoder[A]].toJSValue).orNull

    override def skipForField(a: Option[A]): Boolean = a.isEmpty
  end given

  def union[A: JSValueEncoder, B: JSValueEncoder](using TypeTest[A | B, A], TypeTest[A | B, B]): JSValueEncoder[A | B] =
    new JSValueEncoder[A | B] {
      override def toJSValue(a: A | B): JSValue =
        a match {
          case a: A => summon[JSValueEncoder[A]].toJSValue(a)
          case b: B => summon[JSValueEncoder[B]].toJSValue(b)
        }
    }

  override def join[T](ctx: CaseClass[JSValueEncoder, T]): JSValueEncoder[T] = value =>
    JSValue.fromMap(
      ctx.params
        .iterator
        .filterNot { param => param.typeclass.skipForField(param.deref(value)) }
        .map { param =>
          param.label -> param.typeclass.toJSValue(param.deref(value))
        }
        .toMap
    )

  override def split[T](ctx: SealedTrait[JSValueEncoder, T]): JSValueEncoder[T] = value =>
    ctx.choose(value) { sub => sub.typeclass.toJSValue(sub.cast(value)) }

end JSValueEncoder


trait JSFieldEncoder[A] {
  def toJSValue(a: A): Option[JSValue]
}


