package dev.argon.esexpr

import cats.*
import cats.data.NonEmptySeq
import cats.implicits.given
import dev.argon.esexpr.ESExprCodec.DecodeError
import dev.argon.util.{*, given}

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror.ProductOf

trait ESExprCodec[T] {
  lazy val tags: Set[ESExprTag]
  def encode(value: T): ESExpr
  def decode(expr: ESExpr): Either[DecodeError, T]
}

object ESExprCodec extends ESExprCodecDerivation[ESExprCodec] {

  enum ErrorPath {
    case Current
    case Constructor(constructor: String)
    case Positional(constructor: String, pos: Int, next: ErrorPath)
    case Keyword(constructor: String, name: String, next: ErrorPath)
  }

  final case class DecodeError(message: String, path: ErrorPath) extends ESExprDecodeException(message + " " + path.toString)


  given ESExprCodec[String] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)
    override def encode(value: String): ESExpr =
      ESExpr.Str(value)

    override def decode(expr: ESExpr): Either[DecodeError, String] =
      expr match {
        case ESExpr.Str(s) => Right(s)
        case _ => Left(DecodeError("Expected a string", ErrorPath.Current))
      }
  end given

  given ESExprCodec[Boolean] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Bool)
    override def encode(value: Boolean): ESExpr =
      ESExpr.Bool(value)

    override def decode(expr: ESExpr): Either[DecodeError, Boolean] =
      expr match {
        case ESExpr.Bool(b) => Right(b)
        case _ => Left(DecodeError("Expected a bool", ErrorPath.Current))
      }
  end given

  given ESExprCodec[BigInt] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Int)
    override def encode(value: BigInt): ESExpr =
      ESExpr.Int(value)

    override def decode(expr: ESExpr): Either[DecodeError, BigInt] =
      expr match {
        case ESExpr.Int(n) => Right(n)
        case _ => Left(DecodeError("Expected an int", ErrorPath.Current))
      }
  end given

  given ESExprCodec[Long] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Int)
    override def encode(value: Long): ESExpr =
      ESExpr.Int(value)

    override def decode(expr: ESExpr): Either[DecodeError, Long] =
      expr match {
        case ESExpr.Int(n) if n >= Long.MinValue && n <= Long.MaxValue => Right(n.toLong)
        case _ => Left(DecodeError("Expected an int within the range of a 64-bit siged integer", ErrorPath.Current))
      }
  end given

  given ESExprCodec[Double] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Float64)
    override def encode(value: Double): ESExpr =
      ESExpr.Float64(value)

    override def decode(expr: ESExpr): Either[DecodeError, Double] =
      expr match {
        case ESExpr.Float64(f) => Right(f)
        case _ => Left(DecodeError("Expected a float64", ErrorPath.Current))
      }
  end given

  given ESExprCodec[ESExpr] with
    override lazy val tags: Set[ESExprTag] = Set.empty
    override def encode(value: ESExpr): ESExpr = value
    override def decode(expr: ESExpr): Either[DecodeError, ESExpr] = Right(expr)
  end given

  given [A: ESExprCodec]: ESExprCodec[Seq[A]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor("list"))

    override def encode(value: Seq[A]): ESExpr =
      ESExpr.Constructed(
        "list",
        Map(),
        value.map(summon[ESExprCodec[A]].encode)
      )

    override def decode(expr: ESExpr): Either[DecodeError, Seq[A]] =
      expr match {
        case expr: ESExpr.Constructed =>
          for
            _ <- if expr.constructor == "list" then Right(()) else Left(DecodeError(s"Invalid constructor name for list: ${expr.constructor}", ErrorPath.Current))
            _ <- if expr.kwargs.isEmpty then Right(()) else Left(DecodeError(s"Unexpected keyword arguments for list: ${expr.constructor}", ErrorPath.Current))
            values <- expr.args.zipWithIndex
              .traverse((arg, i) => summon[ESExprCodec[A]].decode(arg).left.map(error => DecodeError(error.message, ErrorPath.Positional("list", i, error.path))))
          yield values

        case _ => Left(DecodeError("Expected constructor for list", ErrorPath.Current))
      }
  end given

  given[A: ESExprCodec]: ESExprCodec[NonEmptySeq[A]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor("list"))

    override def encode(value: NonEmptySeq[A]): ESExpr =
      summon[ESExprCodec[Seq[A]]].encode(value.toList)

    override def decode(expr: ESExpr): Either[DecodeError, NonEmptySeq[A]] =
      summon[ESExprCodec[Seq[A]]].decode(expr).flatMap { values =>
        NonEmptySeq.fromSeq(values.toList).toRight(DecodeError("List was expected to be non-empty", ErrorPath.Current))
      }
  end given

  given[A: ESExprCodec]: ESExprCodec[Option[A]] with
    override lazy val tags: Set[ESExprTag] = summon[ESExprCodec[A]].tags + ESExprTag.Null

    override def encode(value: Option[A]): ESExpr =
      value.fold(ESExpr.Null)(summon[ESExprCodec[A]].encode)

    override def decode(expr: ESExpr): Either[DecodeError, Option[A]] =
      expr match {
        case ESExpr.Null => Right(None)
        case _ => summon[ESExprCodec[A]].decode(expr).map(Some.apply)
      }


  override def simpleEnumCodec[T](caseNames: Array[String], caseValues: Map[String, T])(using m: Mirror.SumOf[T]): ESExprCodec[T] =
    new ESExprCodec[T] {
      override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

      override def encode(value: T): ESExpr =
        ESExpr.Str(caseNames(m.ordinal(value)))

      override def decode(expr: ESExpr): Either[DecodeError, T] =
        expr match {
          case ESExpr.Str(s) =>
            caseValues.get(s).toRight { DecodeError(s"Invalid simple enum value: $s", ErrorPath.Current) }

          case _ =>
            Left(DecodeError("Expected a string for enum value", ErrorPath.Current))
        }
    }

  override def getCodecTags[T](codec: ESExprCodec[T]): Set[ESExprTag] =
    codec.tags

  override inline def derivedSumCreateCodec[T](codecMap: => Map[ESExprTag, WrappedCodec[ESExprCodec, ? <: T]])(using m: Mirror.SumOf[T]): ESExprCodec[T] =
    ${ derivedSumMacro[T, m.MirroredElemTypes]('codecMap) }

  def derivedSumMacro[T: Type, SubTypes <: Tuple: Type](codecMap: Expr[Map[ESExprTag, WrappedCodec[ESExprCodec, ? <: T]]])(using q: Quotes): Expr[ESExprCodec[T]] =
    '{
      new ESExprCodec[T] {
        override lazy val tags: Set[ESExprTag] = ${codecMap}.keySet

        override def encode(value: T): ESExpr =
          ${
            MacroUtils.patternMatch[T, SubTypes, ESExpr]('value)([U] => (uValue: Expr[U], uType: Type[U]) => {
              given Type[U] = uType
              '{
                ESExprCodec.derived[U](using summonInline[Mirror.Of[U]]).encode($uValue)
              }
            })
          }

        override def decode(expr: ESExpr): Either[DecodeError, T] =
          val tag = ESExprTag.fromExpr(expr)

          ${codecMap}.get(tag).toRight(DecodeError(s"Unexpected tag: $tag (valid tags: ${tags})", ErrorPath.Current))
            .flatMap { codec => codec.codec.decode(expr) }
        end decode
      }
    }


  override type TCodecProduct[T] = ESExprCodecProduct[T]
  override type TCodecField[T] = ESExprCodecField[T]

  enum ProductErrorPath derives CanEqual {
    case Current
    case Positional(pos: Int, next: ErrorPath)
    case Keyword(name: String, next: ErrorPath)
  }

  final case class ProductDecodeError(message: String, path: ProductErrorPath) {
    def toDecodeError(constructor: String): DecodeError =
      DecodeError(message, path match {
        case ProductErrorPath.Current => ErrorPath.Constructor(constructor)
        case ProductErrorPath.Positional(pos, next) => ErrorPath.Positional(constructor, pos, next)
        case ProductErrorPath.Keyword(name, next) => ErrorPath.Keyword(constructor, name, next)
      })
  }

  trait ESExprCodecProduct[T] {
    def encode(value: T): (Map[String, ESExpr], List[ESExpr])
    def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, T]
  }

  trait ESExprCodecField[T] {
    def defaultValue: Option[T]
    def encode(value: T): Option[ESExpr]
    def decode(expr: ESExpr): Either[DecodeError, T]
  }


  override inline def codecProductToCodec[T](using m: Mirror.ProductOf[T])(constructor: String, codecProduct: ESExprCodecProduct[m.MirroredElemTypes]): ESExprCodec[T] =
    new ESExprCodec[T] {
      override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor(constructor))

      override def encode(value: T): ESExpr =
        val (kwargs, args) = codecProduct.encode(
          Tuple.fromProductTyped[T & Product](
            summonInline[T =:= (T & Product)](value)
          )(using summonInline[Mirror.ProductOf[T] {type MirroredElemTypes = m.MirroredElemTypes} =:= Mirror.ProductOf[T & Product] {type MirroredElemTypes = m.MirroredElemTypes}](m))
        )

        ESExpr.Constructed(constructor, kwargs, args)
      end encode

      override def decode(expr: ESExpr): Either[DecodeError, T] =
        expr match {
          case expr: ESExpr.Constructed =>
            for
              _ <- if expr.constructor == constructor then Right(()) else Left(DecodeError(s"Unexpected constructor name: ${expr.constructor}", ErrorPath.Current))
              res <- codecProduct.decode(expr.kwargs, expr.args.toList)
                .left.map(_.toDecodeError(constructor))
            yield m.fromTuple(res)

          case _ =>
            Left(DecodeError("Expected a constructed value", ErrorPath.Current))
        }
    }

  override def optionalFieldCodec[Elem](elemCodec: ESExprCodec[Elem]): ESExprCodecField[Option[Elem]] =
    new ESExprCodecField[Option[Elem]] {
      override def defaultValue: Option[Option[Elem]] = Some(None)

      override def encode(value: Option[Elem]): Option[ESExpr] =
        value.map(elemCodec.encode)

      override def decode(expr: ESExpr): Either[DecodeError, Option[Elem]] =
        elemCodec.decode(expr).map(Some.apply)
    }


  override def fieldCodecWithDefault[Elem](elemCodec: ESExprCodec[Elem], defValue: Elem)(using CanEqual[Elem, Elem]): ESExprCodecField[Elem] =
    new ESExprCodecField[Elem] {
      override def defaultValue: Option[Elem] =
        Some(defValue)

      override def encode(value: Elem): Option[ESExpr] =
        if value == defValue then
          Some(elemCodec.encode(value))
        else
          None

      override def decode(expr: ESExpr): Either[DecodeError, Elem] = elemCodec.decode(expr)
    }

  override def codecToFieldCodec[Elem](elemCodec: ESExprCodec[Elem]): ESExprCodecField[Elem] =
    new ESExprCodecField[Elem] {
      override def defaultValue: Option[Elem] = None
      override def encode(value: Elem): Option[ESExpr] = Some(elemCodec.encode(value))
      override def decode(expr: ESExpr): Either[DecodeError, Elem] = elemCodec.decode(expr)
    }


  override def dictProductCodec[Elem, Tail <: Tuple](elemCodec: ESExprCodec[Elem], tailCodec: ESExprCodecProduct[Tail]): ESExprCodecProduct[Map[String, Elem] *: Tail] =
    new ESExprCodecProduct[Map[String, Elem] *: Tail] {
      override def encode(value: Map[String, Elem] *: Tail): (Map[String, ESExpr], List[ESExpr]) =
        val (h *: t) = value
        val (kwargs, args) = tailCodec.encode(t)
        (kwargs ++ h.view.mapValues(elemCodec.encode), args)
      end encode

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, Map[String, Elem] *: Tail] =
        for
          decoded <- kwargs.toSeq.traverse((k, v) =>
            for
              v2 <- elemCodec.decode(v)
                .left.map { error => ProductDecodeError(error.message, ProductErrorPath.Keyword(k, error.path)) }
            yield k -> v2
          )
          tailDecoded <- tailCodec.decode(Map.empty, args)
        yield decoded.toMap *: tailDecoded
    }


  override def keywordProductCodec[A, Tail <: Tuple](keyName: String, fieldCodec: ESExprCodecField[A], tailCodec: ESExprCodecProduct[Tail]): ESExprCodecProduct[A *: Tail] =
    new ESExprCodecProduct[A *: Tail] {
      override def encode(value: A *: Tail): (Map[String, ESExpr], List[ESExpr]) =
        val (h *: t) = value
        val (kwargs, args) = tailCodec.encode(t)
        (fieldCodec.encode(h).fold(kwargs)(h => kwargs + (keyName -> h)), args)
      end encode

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, A *: Tail] =
        for
          decoded <- kwargs.get(keyName) match {
            case Some(value) => fieldCodec.decode(value)
              .left.map { error => ProductDecodeError(error.message, ProductErrorPath.Keyword(keyName, error.path)) }
            case None => fieldCodec.defaultValue.toRight(ProductDecodeError(s"Required key $keyName was not provided", ProductErrorPath.Current))
          }
          tailDecoded <- tailCodec.decode(kwargs.removed(keyName), args)
        yield decoded *: tailDecoded

    }


  override def varargsProductCodec[Elem](typeName: String, elemCodec: ESExprCodec[Elem]): ESExprCodecProduct[Seq[Elem] *: EmptyTuple] =
    new ESExprCodecProduct[Seq[Elem] *: EmptyTuple] {
      override def encode(value: Seq[Elem] *: EmptyTuple): (Map[String, ESExpr], List[ESExpr]) =
        val (elems *: _) = value
        (Map.empty, elems.map(elemCodec.encode).toList)
      end encode

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, Seq[Elem] *: EmptyTuple] =
        if kwargs.nonEmpty then
          Left(ProductDecodeError(s"Extra keyword arguments were provided for $typeName: ${kwargs.keySet}", ProductErrorPath.Current))
        else
          args.zipWithIndex
            .traverse((arg, i) =>
              elemCodec.decode(arg)
                .left.map(error => ProductDecodeError(error.message, ProductErrorPath.Positional(i, error.path)))
            )
            .map(_ *: EmptyTuple)
    }

  override def positionalProductCodec[Elem, Tail <: Tuple](elemCodec: ESExprCodec[Elem], tailCodec: ESExprCodecProduct[Tail]): ESExprCodecProduct[Elem *: Tail] =
    new ESExprCodecProduct[Elem *: Tail] {
      override def encode(value: Elem *: Tail): (Map[String, ESExpr], List[ESExpr]) =
        val (h *: t) = value
        val (kwargs, args) = tailCodec.encode(t)
        (kwargs, elemCodec.encode(h) :: args)
      end encode

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, Elem *: Tail] =
        args match {
          case h :: t =>
            for
              decoded <- elemCodec.decode(h)
                .left.map(error => ProductDecodeError(error.message, ProductErrorPath.Positional(0, error.path)))
              tailDecoded <- tailCodec.decode(kwargs, t)
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


  override def emptyProductCodec: ESExprCodecProduct[EmptyTuple] =
    new ESExprCodecProduct[EmptyTuple] {
      override def encode(value: EmptyTuple): (Map[String, ESExpr], List[ESExpr]) = (Map.empty, List.empty)

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[ProductDecodeError, EmptyTuple] =
        if kwargs.nonEmpty || args.nonEmpty then
          Left(ProductDecodeError("Extra arguments were provided", ProductErrorPath.Current))
        else
          Right(EmptyTuple)
    }

  override def inlineCodec[T <: Product, Elem](elemCodec: ESExprCodec[Elem])(using m: ProductOf[T] {type MirroredElemTypes = Elem *: EmptyTuple}): ESExprCodec[T] =
    new ESExprCodec[T] {
      override lazy val tags: Set[ESExprTag] = elemCodec.tags

      override def encode(value: T): ESExpr = {
        val (elemValue *: EmptyTuple) = Tuple.fromProductTyped(value)
        elemCodec.encode(elemValue)
      }

      override def decode(expr: ESExpr): Either[DecodeError, T] =
        elemCodec.decode(expr).map(res => m.fromTuple(res *: EmptyTuple))
    }
}

