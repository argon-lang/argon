package dev.argon.options

import magnolia1.*
import dev.argon.io.*
import dev.argon.util.{*, given}
import dev.argon.esexpr.{ESExpr, ESExprCodec, ESExprCodecDerivation, ESExprTag, WrappedCodec}

import scala.deriving.Mirror
import scala.util.NotGiven
import zio.*

import scala.compiletime.summonInline
import scala.quoted.*


trait OptionDecoder[R, E, A] {
  lazy val tags: Set[ESExprTag]
  def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, A]
}

object OptionDecoder {

  class Derivation[R, E] extends ESExprCodecDerivation[[A] =>> OptionDecoder[R, E, A]] {
    override def simpleEnumCodec[T](caseNames: Array[String], caseValues: Map[String, T])(using m: Mirror.SumOf[T]): OptionDecoder[R, E, T] =
      new OptionDecoder[R, E, T] {
        override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

        override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, T] =
          value match {
            case ESExpr.Str(s) =>
              caseValues.get(s).toRight { s"Invalid simple enum value: $s" }

            case _ =>
              Left("Expected a string for enum value")
          }

      }

    override def getCodecTags[T](codec: OptionDecoder[R, E, T]): Set[ESExprTag] =
      codec.tags

    override inline def derivedSumCreateCodec[T](codecMap: => Map[ESExprTag, WrappedCodec[[A] =>> OptionDecoder[R, E, A], ? <: T]])(using m: Mirror.SumOf[T]): OptionDecoder[R, E, T] =
      new OptionDecoder[R, E, T] {
        override lazy val tags: Set[ESExprTag] = codecMap.keySet

        override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, T] =
          val tag = ESExprTag.fromExpr(value)
          codecMap.get(tag) match {
            case Some(codec) => codec.codec.decode(resFactory)(value)
            case None => Left(s"Unexpected tag: ${tag}")
          }
        end decode
      }

    override type TCodecProduct[T] = DecoderProduct[T]
    override type TCodecField[T] = DecoderField[T]

    trait DecoderProduct[T] {
      def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, T]
    }

    trait DecoderField[T] {
      def defaultValue: Option[T]
      def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, T]
    }

    override inline def codecProductToCodec[T](using m: Mirror.ProductOf[T])(constructor: String, codecProduct: DecoderProduct[m.MirroredElemTypes]): OptionDecoder[R, E, T] =
      new OptionDecoder[R, E, T] {
        override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor(constructor))

        override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, T] =
          value match {
            case expr: ESExpr.Constructed =>
              for
                _ <- if expr.constructor == constructor then Right(()) else Left("Unexpected constructor name")
                res <- codecProduct.decode(resFactory)(expr.kwargs, expr.args.toList)
              yield m.fromTuple(res)

            case _ =>
              Left("Expected a constructed value")
          }
      }

    override def optionalFieldCodec[Elem](elemCodec: OptionDecoder[R, E, Elem]): DecoderField[Option[Elem]] =
      new DecoderField[Option[Elem]] {
        override def defaultValue: Option[Option[Elem]] = Some(None)

        override def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, Option[Elem]] =
          elemCodec.decode(resFactory)(expr).map(Some.apply)
      }

    override def fieldCodecWithDefault[Elem](elemCodec: OptionDecoder[R, E, Elem], defValue: Elem)(using CanEqual[Elem, Elem]): DecoderField[Elem] =
      new DecoderField[Elem] {
        override def defaultValue: Option[Elem] =
          Some(defValue)

        override def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, Elem] =
          elemCodec.decode(resFactory)(expr)
      }

    override def codecToFieldCodec[Elem](elemCodec: OptionDecoder[R, E, Elem]): DecoderField[Elem] =
      new DecoderField[Elem] {
        override def defaultValue: Option[Elem] = None

        override def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, Elem] =
          elemCodec.decode(resFactory)(expr)
      }

    override def dictProductCodec[Elem, Tail <: Tuple](elemCodec: OptionDecoder[R, E, Elem], tailCodec: DecoderProduct[Tail]): DecoderProduct[Map[String, Elem] *: Tail] =
      new DecoderProduct[Map[String, Elem] *: Tail] {
        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Map[String, Elem] *: Tail] =
          for
            decoded <- kwargs.toSeq.traverse((k, v) =>
              for
                v2 <- elemCodec.decode(resFactory)(v)
              yield k -> v2
            )
            tailDecoded <- tailCodec.decode(resFactory)(Map.empty, args)
          yield decoded.toMap *: tailDecoded
      }

    override def keywordProductCodec[A, Tail <: Tuple](keyName: String, fieldCodec: DecoderField[A], tailCodec: DecoderProduct[Tail]): DecoderProduct[A *: Tail] =
      new DecoderProduct[A *: Tail] {
        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, A *: Tail] =
          for
            decoded <- kwargs.get(keyName) match {
              case Some(value) => fieldCodec.decode(resFactory)(value)
              case None => fieldCodec.defaultValue.toRight(s"Required key $keyName was not provided")
            }
            tailDecoded <- tailCodec.decode(resFactory)(Map.empty, args)
          yield decoded *: tailDecoded
      }

    override def varargsProductCodec[Elem](typeName: String, elemCodec: OptionDecoder[R, E, Elem]): DecoderProduct[Seq[Elem] *: EmptyTuple] =
      new DecoderProduct[Seq[Elem] *: EmptyTuple] {
        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Seq[Elem] *: EmptyTuple] =
          if kwargs.nonEmpty then
            Left(s"Extra keyword arguments were provided for $typeName: ${kwargs.keySet}")
          else
            args.traverse(elemCodec.decode(resFactory))
              .map(_ *: EmptyTuple)
      }

    override def positionalProductCodec[Elem, Tail <: Tuple](elemCodec: OptionDecoder[R, E, Elem], tailCodec: DecoderProduct[Tail]): DecoderProduct[Elem *: Tail] =
      new DecoderProduct[Elem *: Tail] {
        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Elem *: Tail] =
          args match {
            case h :: t =>
              for
                decoded <- elemCodec.decode(resFactory)(h)
                tailDecoded <- tailCodec.decode(resFactory)(kwargs, t)
              yield decoded *: tailDecoded

            case _ =>
              Left("Not enough arguments were provided")
          }
      }

    override def emptyProductCodec: DecoderProduct[EmptyTuple] =
      new DecoderProduct[EmptyTuple] {
        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, EmptyTuple] =
          if kwargs.nonEmpty || args.nonEmpty then
            Left("Extra arguments were provided")
          else
            Right(EmptyTuple)
      }

    override def inlineCodec[T <: Product, Elem](elemCodec: OptionDecoder[R, E, Elem])(using m: Mirror.ProductOf[T] {type MirroredElemTypes = Elem *: EmptyTuple}): OptionDecoder[R, E, T] =
      new OptionDecoder[R, E, T] {
        override lazy val tags: Set[ESExprTag] = elemCodec.tags

        override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, T] =
          elemCodec.decode(resFactory)(value).map(res => m.fromTuple(res *: EmptyTuple))
      }
  }


  inline def derive[R, E, A: Mirror.Of]: OptionDecoder[R, E, A] =
    new Derivation[R, E].derived
  end derive

  // ESExpr
  given esexprOptionDecoder[R, E, A: ESExprCodec]: OptionDecoder[R, E, A] with
    override lazy val tags: Set[ESExprTag] =
      summon[ESExprCodec[A]].tags

    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, A] =
      summon[ESExprCodec[A]].decode(value)

  end esexprOptionDecoder

  // DirectoryResource
  given directoryResourceOptionDecoder[Res[-R2, +E2] <: BinaryResource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, DirectoryResource[R, E, Res]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, DirectoryResource[R, E, Res]] =
      summon[ESExprCodec[String]].decode(value).map { str =>
        resFactory.directoryResource(str).decode[Res]
      }
  end directoryResourceOptionDecoder


  // BinaryResource
  given [Res[R2, E2] <: BinaryResource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionDecoder[R, E, Res[R, E]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, Res[R, E]] =
      summon[ESExprCodec[String]].decode(value).map { str =>
        summon[BinaryResourceDecoder[Res, R, E]].decode(resFactory.binaryResource(str))
      }
  end given

  given [R, E]: OptionDecoder[R, E, Nothing] with
    override lazy val tags: Set[ESExprTag] = Set.empty
    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, Nothing] =
      Left("Options are not supported")
  end given

}
