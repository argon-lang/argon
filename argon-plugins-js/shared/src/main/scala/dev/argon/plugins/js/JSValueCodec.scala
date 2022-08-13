package dev.argon.plugins.js

import magnolia1.*
import dev.argon.util.{*, given}
import scala.reflect.TypeTest

trait JSValueCodec[A] {
  def toJSValue(context: JSContext)(a: A): JSValue
  def fromJSValue(context: JSContext)(value: JSValue): Either[String, A]
  def skipForField(a: A): Boolean = false
  def defaultValue: Option[A] = None
}

object JSValueCodec extends Derivation[JSValueCodec]:
  given [S <: String](using TypeTest[String, S]): JSValueCodec[S] with
    override def toJSValue(context: JSContext)(a: S): JSValue = context.fromString(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, S] =
      (context.decode(value) match {
        case s: String =>
          s match {
            case s: S => Some(s)
            case _ => None
          }

        case _ => None
      }).toRight { s"Could not convert value to string: $value" }
  end given

  given JSValueCodec[Double] with
    override def toJSValue(context: JSContext)(a: Double): JSValue = context.fromDouble(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, Double] =
      context.decode(value) match {
        case value: Double => Right(value)
        case _ => Left("Could not convert value to double")
      }
  end given

  given JSValueCodec[Int] with
    override def toJSValue(context: JSContext)(a: Int): JSValue = context.fromDouble(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, Int] =
      context.decode(value) match {
        case value: Double => Right(value.toInt)
        case _ => Left("Could not convert value to int")
      }
  end given

  given JSValueCodec[BigInt] with
    override def toJSValue(context: JSContext)(a: BigInt): JSValue = context.fromBigInt(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, BigInt] =
      context.decode(value) match {
        case value: BigInt => Right(value.toInt)
        case _ => Left("Could not convert value to bigint")
      }
  end given

  given [B <: Boolean](using TypeTest[Boolean, B]): JSValueCodec[B] with
    override def toJSValue(context: JSContext)(a: B): JSValue = context.fromBoolean(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, B] =
      (context.decode(value) match {
        case b: Boolean =>
          b match {
            case b: B => Some(b)
            case _ => None
          }

        case _ => None
      }).toRight { "Could not convert value to boolean" }
  end given

  given JSValueCodec[Nothing] with
    override def toJSValue(context: JSContext)(a: Nothing): JSValue = a
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, Nothing] = Left("Cannot create nothing value")
  end given

  given [A: JSValueCodec]: JSValueCodec[Seq[A]] with
    override def toJSValue(context: JSContext)(a: Seq[A]): JSValue =
      context.fromSeq(a.map(summon[JSValueCodec[A]].toJSValue(context)))

    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, Seq[A]] =
      context.decode(value) match {
        case DecodedJSArray(seq) =>
          Traverse[Seq].traverse(seq)(summon[JSValueCodec[A]].fromJSValue(context))

        case _ =>
          Left("Invalid sequence")
      }
  end given

  given [A: JSValueCodec]: JSValueCodec[Nullable[A]] with
    override def toJSValue(context: JSContext)(a: Nullable[A]): JSValue =
      a.fold(context.fromNull, summon[JSValueCodec[A]].toJSValue(context))

    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, Nullable[A]] =
//      given CanEqual[context.DecodedJSValueNotNull | Null, Null] = canEqualNullRight[context.DecodedJSValueNotNull, context.DecodedJSValueNotNull | Null, Null]
      if context.decode(value) == null then
        Right(Nullable(null))
      else
        summon[JSValueCodec[A]].fromJSValue(context)(value).map(Nullable.apply)

  end given

  given [A: JSValueCodec]: JSValueCodec[Option[A]] with
    override def toJSValue(context: JSContext)(a: Option[A]): JSValue =
      a.fold(context.fromNull)(summon[JSValueCodec[A]].toJSValue(context))

    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, Option[A]] =
      if context.decode(value) == null then
        Right(None)
      else
        summon[JSValueCodec[A]].fromJSValue(context)(value).map(Some.apply)

    override def skipForField(a: Option[A]): Boolean = a.isEmpty

    override def defaultValue: Option[Option[A]] = Some(None)
  end given

  def union[A: JSValueCodec, B: JSValueCodec](using TypeTest[A | B, A], TypeTest[A | B, B]): JSValueCodec[A | B] =
    new JSValueCodec[A | B] {
      override def toJSValue(context: JSContext)(a: A | B): JSValue =
        a match {
          case a: A => summon[JSValueCodec[A]].toJSValue(context)(a)
          case b: B => summon[JSValueCodec[B]].toJSValue(context)(b)
        }

      override def fromJSValue(context: JSContext)(value: JSValue): Either[String, A | B] =
        summon[JSValueCodec[A]].fromJSValue(context)(value) match {
          case Right(a) => Right(a)
          case Left(error) =>
            summon[JSValueCodec[B]].fromJSValue(context)(value) match {
              case Right(b) => Right(b)
              case Left(error2) =>
                Left(s"Alternatives failed:\nValue: $value\nAttempted ${summon[JSValueCodec[A]]}, got $error\nAttempted ${summon[JSValueCodec[B]]}, got $error2")
            }
        }
    }

  override def join[T](ctx: CaseClass[JSValueCodec, T]): JSValueCodec[T] =
    new JSValueCodec[T] {
      override def toJSValue(context: JSContext)(value: T): JSValue =
        context.fromMap(
          ctx.params
            .iterator
            .filterNot { param => param.typeclass.skipForField(param.deref(value)) }
            .map { param =>
              param.label -> param.typeclass.toJSValue(context)(param.deref(value))
            }
            .toMap
        )

      override def fromJSValue(context: JSContext)(value: JSValue): Either[String, T] =
        context.decode(value) match {
          case DecodedJSObject(map) =>
            ctx.constructEither { param =>
              map.get(param.label) match {
                case Some(memberValue) => param.typeclass.fromJSValue(context)(memberValue)
                case None =>
                  param.typeclass.defaultValue match
                    case Some(memberValue) => Right(memberValue)
                    case None => Left(s"Missing key ${param.label} in object of type ${ctx.typeInfo.short}: , map: ${map}")

              }
            }.left.map { _.mkString("\n") }

          case obj =>
            Left("Invalid object")
        }

      override def toString: String = s"JSValueCodec for ${ctx.typeInfo.full}"
    }

  private trait SplitValueCodec[T] extends JSValueCodec[T] {
    def supportsType(typeName: String): Boolean
  }

  override def split[T](ctx: SealedTrait[JSValueCodec, T]): JSValueCodec[T] =
    new SplitValueCodec[T] {
      override def toJSValue(context: JSContext)(value: T): JSValue =
        ctx.choose(value) { sub => sub.typeclass.toJSValue(context)(sub.cast(value)) }

      override def fromJSValue(context: JSContext)(value: JSValue): Either[String, T] =
        context.decode(value) match {
          case DecodedJSObject(map) =>
            for {
              typeName <- map.get("type").map(context.decode) match {
                case Some(typeName: String) => Right(typeName)
                case _ => Left(s"Could not get type specifier from $map")
              }
              subtype <- ctx.subtypes.find(matchingTypeInfo(typeName)).toRight { s"Could not find specified type: $typeName, known sub types: ${ctx.subtypes.toSeq}" }
              value <- subtype.typeclass.fromJSValue(context)(value)
            } yield value

          case obj =>
            Left("Invalid object")
        }

      override def supportsType(typeName: String): Boolean =
        ctx.subtypes.exists(matchingTypeInfo(typeName))

      private def matchingTypeInfo(typeName: String)(subtype: SealedTrait.Subtype[JSValueCodec, T, ?]): Boolean =
        typeName == subtype.typeInfo.short || (subtype.typeclass match {
          case typeclass: SplitValueCodec[?] => typeclass.supportsType(typeName)
          case _ => false
        })

      override def toString: String = s"JSValueCodec for ${ctx.typeInfo.full}"
    }

end JSValueCodec



