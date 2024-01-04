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


trait OptionCodec[R, E, A] {
  lazy val tags: Set[ESExprTag]
  def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, ESExpr]
  def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, A]
}

object OptionCodec {

  class Derivation[R, E] extends ESExprCodecDerivation[[A] =>> OptionCodec[R, E, A]] {
    override def simpleEnumCodec[T](caseNames: Array[String], caseValues: Map[String, T])(using m: Mirror.SumOf[T]): OptionCodec[R, E, T] =
      new OptionCodec[R, E, T] {
        override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

        override def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, ESExpr] =
          ZIO.succeed(ESExpr.Str(caseNames(m.ordinal(value))))

        override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, T] =
          value match {
            case ESExpr.Str(s) =>
              caseValues.get(s).toRight { s"Invalid simple enum value: $s" }

            case _ =>
              Left("Expected a string for enum value")
          }

      }

    override def getCodecTags[T](codec: OptionCodec[R, E, T]): Set[ESExprTag] =
      codec.tags

    override inline def derivedSumCreateCodec[T](codecMap: => Map[ESExprTag, WrappedCodec[[A] =>> OptionCodec[R, E, A], ? <: T]])(using m: Mirror.SumOf[T]): OptionCodec[R, E, T] =
      new OptionCodec[R, E, T] {
        override lazy val tags: Set[ESExprTag] = codecMap.keySet

        override def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, ESExpr] =
          derivedSumEncode(recorder)(value)

        override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, T] =
          val tag = ESExprTag.fromExpr(value)
          codecMap.get(tag) match {
            case Some(codec) => codec.codec.decode(resFactory)(value)
            case None => Left(s"Unexpected tag: ${tag}")
          }
        end decode
      }

    inline def derivedSumEncode[T](recorder: ResourceRecorder[R, E])(value: T)(using m: Mirror.SumOf[T]): ZIO[R, E, ESExpr] =
      ${ Derivation.derivedSumEncodeMacro[R, E, T, m.MirroredElemTypes]('this, 'recorder, 'value) }

    override type TCodecProduct[T] = CodecProduct[T]
    override type TCodecField[T] = CodecField[T]

    trait CodecProduct[T] {
      def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, (Map[String, ESExpr], List[ESExpr])]
      def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, T]
    }

    trait CodecField[T] {
      def defaultValue: Option[T]
      def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, Option[ESExpr]]
      def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, T]
    }

    override inline def codecProductToCodec[T](using m: Mirror.ProductOf[T])(constructor: String, codecProduct: CodecProduct[m.MirroredElemTypes]): OptionCodec[R, E, T] =
      new OptionCodec[R, E, T] {
        override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor(constructor))

        override def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, ESExpr] =
          codecProduct.encode(recorder)(
            Tuple.fromProductTyped[T & Product](
              summonInline[T =:= (T & Product)](value)
            )(using summonInline[Mirror.ProductOf[T] {type MirroredElemTypes = m.MirroredElemTypes} =:= Mirror.ProductOf[T & Product] {type MirroredElemTypes = m.MirroredElemTypes}](m))
          )
            .map { (kwargs, args) =>
              ESExpr.Constructed(constructor, kwargs, args)
            }

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

    override def optionalFieldCodec[Elem](elemCodec: OptionCodec[R, E, Elem]): CodecField[Option[Elem]] =
      new CodecField[Option[Elem]] {
        override def defaultValue: Option[Option[Elem]] = Some(None)

        override def encode(recorder: ResourceRecorder[R, E])(value: Option[Elem]): ZIO[R, E, Option[ESExpr]] =
          value.traverse(elemCodec.encode(recorder))

        override def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, Option[Elem]] =
          elemCodec.decode(resFactory)(expr).map(Some.apply)
      }

    override def fieldCodecWithDefault[Elem](elemCodec: OptionCodec[R, E, Elem], defValue: Elem)(using CanEqual[Elem, Elem]): CodecField[Elem] =
      new CodecField[Elem] {
        override def defaultValue: Option[Elem] =
          Some(defValue)

        override def encode(recorder: ResourceRecorder[R, E])(value: Elem): ZIO[R, E, Option[ESExpr]] =
          if value == defValue then
            elemCodec.encode(recorder)(value).map(Some.apply)
          else
            ZIO.none

        override def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, Elem] =
          elemCodec.decode(resFactory)(expr)
      }

    override def codecToFieldCodec[Elem](elemCodec: OptionCodec[R, E, Elem]): CodecField[Elem] =
      new CodecField[Elem] {
        override def defaultValue: Option[Elem] = None

        override def encode(recorder: ResourceRecorder[R, E])(value: Elem): ZIO[R, E, Option[ESExpr]] =
          elemCodec.encode(recorder)(value).map(Some.apply)

        override def decode(resFactory: ResourceFactory[R, E])(expr: ESExpr): Either[String, Elem] =
          elemCodec.decode(resFactory)(expr)
      }

    override def dictProductCodec[Elem, Tail <: Tuple](elemCodec: OptionCodec[R, E, Elem], tailCodec: CodecProduct[Tail]): CodecProduct[Map[String, Elem] *: Tail] =
      new CodecProduct[Map[String, Elem] *: Tail] {
        override def encode(recorder: ResourceRecorder[R, E])(value: Map[String, Elem] *: Tail): ZIO[R, E, (Map[String, ESExpr], List[ESExpr])] =
          val (h *: t) = value
          tailCodec.encode(recorder)(t).flatMap { (kwargs, args) =>
            h.toSeq.traverse { (k, v) => elemCodec.encode(recorder)(v).map(k -> _) }.map { encodedH =>
              (kwargs ++ encodedH, args)
            }
          }
        end encode

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

    override def keywordProductCodec[A, Tail <: Tuple](keyName: String, fieldCodec: CodecField[A], tailCodec: CodecProduct[Tail]): CodecProduct[A *: Tail] =
      new CodecProduct[A *: Tail] {
        override def encode(recorder: ResourceRecorder[R, E])(value: A *: Tail): ZIO[R, E, (Map[String, ESExpr], List[ESExpr])] =
          val (h *: t) = value
          tailCodec.encode(recorder)(t).flatMap { (kwargs, args) =>
            fieldCodec.encode(recorder)(h).map { encodedH =>
              (kwargs ++ encodedH.map(keyName -> _), args)
            }
          }
        end encode

        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, A *: Tail] =
          for
            decoded <- kwargs.get(keyName) match {
              case Some(value) => fieldCodec.decode(resFactory)(value)
              case None => fieldCodec.defaultValue.toRight(s"Required key $keyName was not provided")
            }
            tailDecoded <- tailCodec.decode(resFactory)(Map.empty, args)
          yield decoded *: tailDecoded
      }

    override def varargsProductCodec[Elem](typeName: String, elemCodec: OptionCodec[R, E, Elem]): CodecProduct[Seq[Elem] *: EmptyTuple] =
      new CodecProduct[Seq[Elem] *: EmptyTuple] {
        override def encode(recorder: ResourceRecorder[R, E])(value: Seq[Elem] *: EmptyTuple): ZIO[R, E, (Map[String, ESExpr], List[ESExpr])] =
          val (elems *: _) = value
          elems.traverse(elemCodec.encode(recorder)).map { encodedElems =>
            (Map.empty, encodedElems.toList)
          }
        end encode

        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Seq[Elem] *: EmptyTuple] =
          if kwargs.nonEmpty then
            Left(s"Extra keyword arguments were provided for $typeName: ${kwargs.keySet}")
          else
            args.traverse(elemCodec.decode(resFactory))
              .map(_ *: EmptyTuple)
      }

    override def positionalProductCodec[Elem, Tail <: Tuple](elemCodec: OptionCodec[R, E, Elem], tailCodec: CodecProduct[Tail]): CodecProduct[Elem *: Tail] =
      new CodecProduct[Elem *: Tail] {
        override def encode(recorder: ResourceRecorder[R, E])(value: Elem *: Tail): ZIO[R, E, (Map[String, ESExpr], List[ESExpr])] =
          val (h *: t) = value
          tailCodec.encode(recorder)(t).flatMap { (kwargs, args) =>
            elemCodec.encode(recorder)(h).map { encodedH =>
              (kwargs, encodedH :: args)
            }
          }
        end encode

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

    override def emptyProductCodec: CodecProduct[EmptyTuple] =
      new CodecProduct[EmptyTuple] {
        override def encode(recorder: ResourceRecorder[R, E])(value: EmptyTuple): ZIO[R, E, (Map[String, ESExpr], List[ESExpr])] =
          ZIO.succeed((Map.empty, List.empty))

        override def decode(resFactory: ResourceFactory[R, E])(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, EmptyTuple] =
          if kwargs.nonEmpty || args.nonEmpty then
            Left("Extra arguments were provided")
          else
            Right(EmptyTuple)
      }

    override def inlineCodec[T <: Product, Elem](elemCodec: OptionCodec[R, E, Elem])(using m: Mirror.ProductOf[T] {type MirroredElemTypes = Elem *: EmptyTuple}): OptionCodec[R, E, T] =
      new OptionCodec[R, E, T] {
        override lazy val tags: Set[ESExprTag] = elemCodec.tags

        override def encode(recorder: ResourceRecorder[R, E])(value: T): ZIO[R, E, ESExpr] = {
          val (elemValue *: EmptyTuple) = Tuple.fromProductTyped(value)
          elemCodec.encode(recorder)(elemValue)
        }

        override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, T] =
          elemCodec.decode(resFactory)(value).map(res => m.fromTuple(res *: EmptyTuple))
      }
  }

  object Derivation {
    def derivedSumEncodeMacro[R: Type, E: Type, T: Type, SubTypes <: Tuple: Type](self: Expr[Derivation[R, E]], recorder: Expr[ResourceRecorder[R, E]], value: Expr[T])(using q: Quotes): Expr[ZIO[R, E, ESExpr]] =
      MacroUtils.patternMatch[T, SubTypes, ZIO[R, E, ESExpr]](value)([U] => (uValue: Expr[U], uType: Type[U]) => {
        given Type[U] = uType

        '{
          $self.derivedProduct[U](using summonInline[Mirror.ProductOf[U]]).encode($recorder)($uValue)
        }
      })
  }


  inline def derive[R, E, A: Mirror.Of]: OptionCodec[R, E, A] =
    new Derivation[R, E].derived
  end derive

  // ESExpr
  given esexprOptionCodec[R, E, A: ESExprCodec]: OptionCodec[R, E, A] with
    override lazy val tags: Set[ESExprTag] =
      summon[ESExprCodec[A]].tags

    override def encode(recorder: ResourceRecorder[R, E])(value: A): ZIO[R, E, ESExpr] =
      ZIO.succeed(summon[ESExprCodec[A]].encode(value))

    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, A] =
      summon[ESExprCodec[A]].decode(value)

  end esexprOptionCodec

  // DirectoryResource
  given directoryResourceOptionCodec[Res[-R2, +E2] <: BinaryResource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionCodec[R, E, DirectoryResource[R, E, Res]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

    override def encode(recorder: ResourceRecorder[R, E])(value: DirectoryResource[R, E, Res]): ZIO[R, E, ESExpr] =
      recorder.recordDirectoryResource(value)
        .map(summon[ESExprCodec[String]].encode)

    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, DirectoryResource[R, E, Res]] =
      summon[ESExprCodec[String]].decode(value).map { str =>
        resFactory.directoryResource(str).decode[Res]
      }
  end directoryResourceOptionCodec


  // BinaryResource
  given [Res[R2, E2] <: BinaryResource[R2, E2], R, E](using BinaryResourceDecoder[Res, R, E]): OptionCodec[R, E, Res[R, E]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

    override def decode(resFactory: ResourceFactory[R, E])(value: ESExpr): Either[String, Res[R, E]] =
      summon[ESExprCodec[String]].decode(value).map { str =>
        summon[BinaryResourceDecoder[Res, R, E]].decode(resFactory.binaryResource(str))
      }

    override def encode(recorder: ResourceRecorder[R, E])(value: Res[R, E]): ZIO[R, E, ESExpr] =
      recorder.recordBinaryResource(value)
        .map(summon[ESExprCodec[String]].encode)
  end given

}
