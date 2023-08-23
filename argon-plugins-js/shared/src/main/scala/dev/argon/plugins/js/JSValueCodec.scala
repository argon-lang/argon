package dev.argon.plugins.js

import dev.argon.util.{*, given}

import scala.reflect.{ClassTag, TypeTest}
import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, error, summonInline}
import scala.quoted.*

trait JSValueCodec[A] {
  def toJSValue(context: JSContext)(a: A): JSValue
  def fromJSValue(context: JSContext)(value: JSValue): Either[String, A]
  def skipForField(a: A): Boolean = false
  def defaultValue: Option[A] = None
}

object JSValueCodec:

  inline def derived[A](using m: Mirror.Of[A]): JSValueCodec[A] =
    inline m match {
      case m: Mirror.SumOf[A] => sumDerived[A](using m)
      case m: Mirror.ProductOf[A] => productDerived[A](using m)
    }

  private trait SplitValueCodec[T] extends JSValueCodec[T] {
    def supportsType(typeName: String): Boolean
  }

  inline def sumDerived[A](using m: Mirror.SumOf[A]): JSValueCodec[A] =
    lazy val elemCodecs = sumTypeElemCodecs[m.MirroredElemTypes]
    val elemNames = sumTypeElemLabels[m.MirroredElemLabels]
    new SplitValueCodec[A] {
      override def toJSValue(context: JSContext)(a: A): JSValue =
        elemCodecs(m.ordinal(a)) match {
          case codec: JSValueCodec[b] =>
            codec.toJSValue(context)(a.asInstanceOf[b])
        }

      override def fromJSValue(context: JSContext)(value: JSValue): Either[String, A] =
        context.decode(value) match {
          case DecodedJSObject(map) =>
            for {
              typeName <- map.get("type").map(context.decode) match {
                case Some(typeName: String) => Right(typeName)
                case _ => Left(s"Could not get type specifier from $map")
              }
              subtype <- elemCodecs.zip(elemNames).find(matchingTypeInfo(typeName)).toRight {
                s"Could not find specified type: $typeName, known sub types: ${elemNames}"
              }
              value <- subtype._1.fromJSValue(context)(value)
            } yield value.asInstanceOf[A]

          case obj =>
            Left("Invalid object")
        }

      override def supportsType(typeName: String): Boolean =
        elemCodecs.zip(elemNames).exists(matchingTypeInfo(typeName))

      private def matchingTypeInfo(typeName: String)(subCodec: JSValueCodec[?], subTypeName: String): Boolean =
        typeName == subTypeName || (subCodec match {
          case split: SplitValueCodec[?] => split.supportsType(typeName)
          case _ => false
        })

    }

  inline def sumTypeElemCodecs[Types <: Tuple]: List[JSValueCodec[?]] =
    inline erasedValue[Types] match
      case _: (htype *: ttypes) =>
        summonInline[JSValueCodec[htype]] :: sumTypeElemCodecs[ttypes]

      case _: EmptyTuple =>
        Nil
    end match

  inline def sumTypeElemLabels[Labels <: Tuple]: List[String] =
    inline erasedValue[Labels] match
      case _: (h *: t) =>
        constValue[h & String] :: sumTypeElemLabels[t]

      case _: EmptyTuple =>
        Nil
    end match

  trait JSValueObjectCodec[A] {
    def toJSObject(context: JSContext)(a: A): Map[String, JSValue]
    def fromJSObject(context: JSContext)(obj: Map[String, JSValue]): Either[String, A]
  }

  inline def productDerived[A](using m: Mirror.ProductOf[A]): JSValueCodec[A] =
    val derivedTuple = productDerivedTuple[m.MirroredLabel, m.MirroredElemLabels, m.MirroredElemTypes]
    new JSValueCodec[A] {
      override def toJSValue(context: JSContext)(value: A): JSValue =
        context.fromMap(derivedTuple.toJSObject(context)(
          Tuple.fromProductTyped[A & Product](
            summonInline[A =:= (A & Product)](value)
          )(using summonInline[Mirror.ProductOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =:= Mirror.ProductOf[A & Product] { type MirroredElemTypes = m.MirroredElemTypes }](m))
        ))

      override def fromJSValue(context: JSContext)(value: JSValue): Either[String, A] =
        context.decode(value) match {
          case DecodedJSObject(map) =>
            derivedTuple.fromJSObject(context)(map)
              .map(m.fromTuple)

          case obj =>
            Left("Invalid object")
        }

      override def toString: String = s"JSValueCodec for ${constValue[m.MirroredLabel]}"
    }

  inline def productDerivedTuple[TypeLabel <: String, Labels <: Tuple, Types <: Tuple]: JSValueObjectCodec[Types] =
    inline (erasedValue[Labels], erasedValue[Types]) match
      case _: ((hlabel *: tlabels), (htype *: ttypes)) =>
        lazy val hcodec = summonInline[JSValueCodec[htype]]
        val tcodec = productDerivedTuple[TypeLabel, tlabels, ttypes]
    
        summonInline[JSValueObjectCodec[htype *: ttypes] =:= JSValueObjectCodec[Types]](new JSValueObjectCodec[htype *: ttypes] {
          override def toJSObject(context: JSContext)(a: htype *: ttypes): Map[String, JSValue] = {
            val (h *: t) = a
            tcodec.toJSObject(context)(t) + (constValue[hlabel & String] -> hcodec.toJSValue(context)(h))
          }
    
          override def fromJSObject(context: JSContext)(obj: Map[String, JSValue]): Either[String, htype *: ttypes] =
            for
              h <- obj.get(constValue[hlabel & String]) match {
                case Some(memberValue) => hcodec.fromJSValue(context)(memberValue)
                case None =>
                  hcodec.defaultValue match
                    case Some(memberValue) => Right(memberValue)
                    case None => Left(s"Missing key ${constValue[hlabel & String]} in object of type ${constValue[TypeLabel]}, map: ${obj}")
    
              }
              t <- tcodec.fromJSObject(context)(obj)
            yield h *: t
        })

      case _: (EmptyTuple, EmptyTuple) =>
        summonInline[JSValueObjectCodec[EmptyTuple] =:= JSValueObjectCodec[Types]](new JSValueObjectCodec[EmptyTuple] {
          override def toJSObject(context: JSContext)(a: EmptyTuple): Map[String, JSValue] = Map.empty

          override def fromJSObject(context: JSContext)(obj: Map[String, JSValue]): Either[String, EmptyTuple] = Right(EmptyTuple)
        })
    end match

  given JSValueCodec[String] with
    override def toJSValue(context: JSContext)(a: String): JSValue = context.fromString(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, String] =
      (context.decode(value) match {
        case s: String =>
          Some(s)

        case _ => None
      }).toRight {
        s"Could not convert value to string: $value"
      }
  end given

  given [S <: String & Singleton](using strValue: ValueOf[S]): JSValueCodec[S] with
    override def toJSValue(context: JSContext)(a: S): JSValue = context.fromString(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, S] =
      (context.decode(value) match {
        case s: String if s == strValue.value =>
          Some(strValue.value)

        case _ => None
      }).toRight { s"Could not convert value to string: $value" }
  end given

  def stringLiterals[S <: String](using TypeTest[String, S]): JSValueCodec[S] =
    new JSValueCodec[S] {
      override def toJSValue(context: JSContext)(a: S): JSValue = context.fromString(a)

      override def fromJSValue(context: JSContext)(value: JSValue): Either[String, S] =
        (context.decode(value) match {
          case s: String =>
            s match {
              case s: S => Some(s)
              case _ => None
            }

          case _ => None
        }).toRight {
          s"Could not convert value to string: $value"
        }
    }

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

  given JSValueCodec[Boolean] with
    override def toJSValue(context: JSContext)(a: Boolean): JSValue = context.fromBoolean(a)
    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, Boolean] =
      (context.decode(value) match {
        case b: Boolean =>
          Some(b)

        case _ => None
      }).toRight { "Could not convert value to boolean" }
  end given

  given[B <: Boolean] (using boolValue: ValueOf[B]): JSValueCodec[B] with
    override def toJSValue(context: JSContext)(a: B): JSValue = context.fromBoolean(a)

    override def fromJSValue(context: JSContext)(value: JSValue): Either[String, B] =
      (context.decode(value) match {
        case b: Boolean if b == boolValue.value =>
          Some(boolValue.value)

        case _ => None
      }).toRight {
        "Could not convert value to boolean"
      }
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

end JSValueCodec



