package dev.argon.plugins.js

import magnolia1.*
import dev.argon.util.{*, given}
import scala.reflect.TypeTest

trait JSValueCodec[A] {
  def toJSValue(a: A): JSValue
  def skipForField(a: A): Boolean = false
}

object JSValueCodec extends Derivation[JSValueCodec]:
  given [S <: String]: JSValueCodec[S] with
    override def toJSValue(a: S): JSValue = a
  end given

  given JSValueCodec[Double] with
    override def toJSValue(a: Double): JSValue = a
  end given

  given JSValueCodec[Int] with
    override def toJSValue(a: Int): JSValue = a
  end given

  given [B <: Boolean]: JSValueCodec[B] with
    override def toJSValue(a: B): JSValue = a
  end given

  given JSValueCodec[Nothing] with
    override def toJSValue(a: Nothing): JSValue = a
  end given

  given [A: JSValueCodec]: JSValueCodec[Seq[A]] with
    override def toJSValue(a: Seq[A]): JSValue =
      JSValue.fromSeq(a.map(summon[JSValueCodec[A]].toJSValue))

  given [A: JSValueCodec]: JSValueCodec[Nullable[A]] with
    override def toJSValue(a: Nullable[A]): JSValue =
      a.map(summon[JSValueCodec[A]].toJSValue).unwrap
  end given

  given [A: JSValueCodec]: JSValueCodec[Option[A]] with
    override def toJSValue(a: Option[A]): JSValue =
      a.map(summon[JSValueCodec[A]].toJSValue).orNull

    override def skipForField(a: Option[A]): Boolean = a.isEmpty
  end given

  def union[A: JSValueCodec, B: JSValueCodec](using TypeTest[A | B, A], TypeTest[A | B, B]): JSValueCodec[A | B] =
    new JSValueCodec[A | B] {
      override def toJSValue(a: A | B): JSValue =
        a match {
          case a: A => summon[JSValueCodec[A]].toJSValue(a)
          case b: B => summon[JSValueCodec[B]].toJSValue(b)
        }
    }

  override def join[T](ctx: CaseClass[JSValueCodec, T]): JSValueCodec[T] = value =>
    JSValue.fromMap(
      ctx.params
        .iterator
        .filterNot { param => param.typeclass.skipForField(param.deref(value)) }
        .map { param =>
          param.label -> param.typeclass.toJSValue(param.deref(value))
        }
        .toMap
    )

  override def split[T](ctx: SealedTrait[JSValueCodec, T]): JSValueCodec[T] = value =>
    ctx.choose(value) { sub => sub.typeclass.toJSValue(sub.cast(value)) }

end JSValueCodec



