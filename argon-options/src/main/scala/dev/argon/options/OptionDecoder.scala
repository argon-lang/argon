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
import cats.*
import cats.implicits.given
import dev.argon.esexpr.ESExprCodec.{DecodeError, ErrorPath, ProductDecodeError, ProductErrorPath}


trait OptionDecoder[E, A] {
  lazy val tags: Set[ESExprTag]
  def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, A]
}

object OptionDecoder {

  class Derivation[E] extends ESExprCodecDerivation[[A] =>> OptionDecoder[E, A]] {
    override def simpleEnumCodec[T](caseNames: Array[String], caseValues: Map[String, T])(using m: Mirror.SumOf[T]): OptionDecoder[E, T] =
      esexprOptionDecoder(using ESExprCodec.simpleEnumCodec[T](caseNames, caseValues))

    override def getCodecTags[T](codec: OptionDecoder[E, T]): Set[ESExprTag] =
      codec.tags

    override inline def derivedSumCreateCodec[T](codecMap: => Map[ESExprTag, WrappedCodec[[A] =>> OptionDecoder[E, A], ? <: T]])(using m: Mirror.SumOf[T]): OptionDecoder[E, T] =
      new OptionDecoder[E, T] {
        override lazy val tags: Set[ESExprTag] = codecMap.keySet

        override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, T] =
          val tag = ESExprTag.fromExpr(value)

          codecMap.get(tag).toRight(DecodeError(s"Unexpected tag: $tag (valid tags: ${tags})", ErrorPath.Current))
            .flatMap { codec => codec.codec.decode(resFactory)(value) }
        end decode
      }

    override type TCodecProduct[T] = DecoderProduct[T]
    override type TCodecField[T] = DecoderField[T]

    trait DecoderProduct[T] {
      def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, T]
    }

    trait DecoderField[T] {
      def defaultValue: Option[T]
      def decode(resFactory: ResourceFactory[E])(expr: ESExpr): Either[DecodeError, T]
    }

    override inline def codecProductToCodec[T](using m: Mirror.ProductOf[T])(constructor: String, codecProduct: DecoderProduct[m.MirroredElemTypes]): OptionDecoder[E, T] =
      new OptionDecoder[E, T] {
        override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor(constructor))

        override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, T] =
          value match {
            case expr: ESExpr.Constructed =>
              for
                _ <- if expr.constructor == constructor then Right(()) else Left(DecodeError(s"Unexpected constructor name: ${expr.constructor}", ErrorPath.Current))
                res <- codecProduct.decode(resFactory)(expr.kwargs, expr.args.toList)
                  .left.map(_.toDecodeError(constructor))
              yield m.fromTuple(res)

            case _ =>
              Left(DecodeError("Expected a constructed value", ErrorPath.Current))
          }
      }

    override def optionalFieldCodec[Elem](elemCodec: OptionDecoder[E, Elem]): DecoderField[Option[Elem]] =
      new DecoderField[Option[Elem]] {
        override def defaultValue: Option[Option[Elem]] = Some(None)

        override def decode(resFactory: ResourceFactory[E])(expr: ESExpr): Either[DecodeError, Option[Elem]] =
          elemCodec.decode(resFactory)(expr).map(Some.apply)
      }

    override def fieldCodecWithDefault[Elem](elemCodec: OptionDecoder[E, Elem], defValue: Elem)(using CanEqual[Elem, Elem]): DecoderField[Elem] =
      new DecoderField[Elem] {
        override def defaultValue: Option[Elem] =
          Some(defValue)

        override def decode(resFactory: ResourceFactory[E])(expr: ESExpr): Either[DecodeError, Elem] =
          elemCodec.decode(resFactory)(expr)
      }

    override def codecToFieldCodec[Elem](elemCodec: OptionDecoder[E, Elem]): DecoderField[Elem] =
      new DecoderField[Elem] {
        override def defaultValue: Option[Elem] = None

        override def decode(resFactory: ResourceFactory[E])(expr: ESExpr): Either[DecodeError, Elem] =
          elemCodec.decode(resFactory)(expr)
      }

    override def dictProductCodec[Elem, Tail <: Tuple](elemCodec: OptionDecoder[E, Elem], tailCodec: DecoderProduct[Tail]): DecoderProduct[Map[String, Elem] *: Tail] =
      new DecoderProduct[Map[String, Elem] *: Tail] {
        override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, Map[String, Elem] *: Tail] =
          for
            decoded <- kwargs.toSeq.traverse((k, v) =>
              for
                v2 <- elemCodec.decode(resFactory)(v)
                  .left.map { error => ProductDecodeError(error.message, ProductErrorPath.Keyword(k, error.path)) }
              yield k -> v2
            )
            tailDecoded <- tailCodec.decode(resFactory)(Map.empty, args)
          yield decoded.toMap *: tailDecoded
      }

    override def keywordProductCodec[A, Tail <: Tuple](keyName: String, fieldCodec: DecoderField[A], tailCodec: DecoderProduct[Tail]): DecoderProduct[A *: Tail] =
      new DecoderProduct[A *: Tail] {
        override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, A *: Tail] =
          for
            decoded <- kwargs.get(keyName) match {
              case Some(value) => fieldCodec.decode(resFactory)(value)
                .left.map { error => ProductDecodeError(error.message, ProductErrorPath.Keyword(keyName, error.path)) }
              case None => fieldCodec.defaultValue.toRight(ProductDecodeError(s"Required key $keyName was not provided", ProductErrorPath.Current))
            }
            tailDecoded <- tailCodec.decode(resFactory)(kwargs.removed(keyName), args)
          yield decoded *: tailDecoded
      }

    override def varargsProductCodec[Elem](typeName: String, elemCodec: OptionDecoder[E, Elem]): DecoderProduct[Seq[Elem] *: EmptyTuple] =
      new DecoderProduct[Seq[Elem] *: EmptyTuple] {
        override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, Seq[Elem] *: EmptyTuple] =
          if kwargs.nonEmpty then
            Left(ProductDecodeError(s"Extra keyword arguments were provided for $typeName: ${kwargs.keySet}", ProductErrorPath.Current))
          else
            args.zipWithIndex
              .traverse((arg, i) =>
                elemCodec.decode(resFactory)(arg)
                  .left.map(error => ProductDecodeError(error.message, ProductErrorPath.Positional(i, error.path)))
              )
              .map(_ *: EmptyTuple)
      }

    override def positionalProductCodec[Elem, Tail <: Tuple](elemCodec: OptionDecoder[E, Elem], tailCodec: DecoderProduct[Tail]): DecoderProduct[Elem *: Tail] =
      new DecoderProduct[Elem *: Tail] {
        override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, Elem *: Tail] =
          args match {
            case h :: t =>
              for
                decoded <- elemCodec.decode(resFactory)(h)
                  .left.map(error => ProductDecodeError(error.message, ProductErrorPath.Positional(0, error.path)))
                tailDecoded <- tailCodec.decode(resFactory)(kwargs, t)
                  .left.map(error => error.path match {
                    case ProductErrorPath.Positional(pos, path) =>
                      ProductDecodeError(error.message, ProductErrorPath.Positional(pos, path))

                    case _ => error
                  })
              yield decoded *: tailDecoded

            case _ =>
              Left(ProductDecodeError("Not enough arguments were provided", ProductErrorPath.Current))
          }
      }

    override def emptyProductCodec: DecoderProduct[EmptyTuple] =
      new DecoderProduct[EmptyTuple] {
        override def decode(resFactory: ResourceFactory[E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, EmptyTuple] =
          if kwargs.nonEmpty || args.nonEmpty then
            Left(ProductDecodeError("Extra arguments were provided", ProductErrorPath.Current))
          else
            Right(EmptyTuple)
      }

    override def inlineCodec[T <: Product, Elem](elemCodec: OptionDecoder[E, Elem])(using m: Mirror.ProductOf[T] {type MirroredElemTypes = Elem *: EmptyTuple}): OptionDecoder[E, T] =
      new OptionDecoder[E, T] {
        override lazy val tags: Set[ESExprTag] = elemCodec.tags

        override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, T] =
          elemCodec.decode(resFactory)(value).map(res => m.fromTuple(res *: EmptyTuple))
      }
  }


  inline def derive[E, A: Mirror.Of]: OptionDecoder[E, A] =
    new Derivation[E].derived
  end derive

  // ESExpr
  given esexprOptionDecoder[E, A: ESExprCodec]: OptionDecoder[E, A] with
    override lazy val tags: Set[ESExprTag] =
      summon[ESExprCodec[A]].tags

    override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, A] =
      summon[ESExprCodec[A]].decode(value)

  end esexprOptionDecoder

  // DirectoryResource
  given directoryResourceOptionDecoder[Res[+E2] <: BinaryResource[E2], E](using BinaryResourceDecoder[Res, E]): OptionDecoder[E, DirectoryResource[E, Res]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

    override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, DirectoryResource[E, Res]] =
      summon[ESExprCodec[String]].decode(value).map { str =>
        resFactory.directoryResource(str).decode[Res]
      }
  end directoryResourceOptionDecoder


  // BinaryResource
  given [Res[E2] <: BinaryResource[E2], E](using BinaryResourceDecoder[Res, E]): OptionDecoder[E, Res[E]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

    override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, Res[E]] =
      summon[ESExprCodec[String]].decode(value).map { str =>
        summon[BinaryResourceDecoder[Res, E]].decode(resFactory.binaryResource(str))
      }
  end given

  given [E, A](using elementDecoder: OptionDecoder[E, A], notSimpleESExprCodec: NotGiven[ESExprCodec[A]]): OptionDecoder[E, Seq[A]] with
    override lazy val tags: Set[ESExprTag] = summon[ESExprCodec[Seq[ESExpr]]].tags

    override def decode(resFactory: ResourceFactory[E])(value: ESExpr): Either[DecodeError, Seq[A]] =
      summon[ESExprCodec[Seq[ESExpr]]].decode(value).flatMap { items =>
        items.traverse(elementDecoder.decode(resFactory))
      }
  end given

}
