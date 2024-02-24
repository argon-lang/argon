package dev.argon.esexpr

import cats.*
import cats.data.NonEmptyList
import cats.implicits.given
import dev.argon.util.{*, given}

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror.ProductOf

trait ESExprCodec[T] {
  lazy val tags: Set[ESExprTag]
  def encode(value: T): ESExpr
  def decode(expr: ESExpr): Either[String, T]
}

object ESExprCodec extends ESExprCodecDerivation[ESExprCodec] {

  given ESExprCodec[String] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)
    override def encode(value: String): ESExpr =
      ESExpr.Str(value)

    override def decode(expr: ESExpr): Either[String, String] =
      expr match {
        case ESExpr.Str(s) => Right(s)
        case _ => Left("Expected a string")
      }
  end given

  given ESExprCodec[Boolean] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Bool)
    override def encode(value: Boolean): ESExpr =
      ESExpr.Bool(value)

    override def decode(expr: ESExpr): Either[String, Boolean] =
      expr match {
        case ESExpr.Bool(b) => Right(b)
        case _ => Left("Expected a bool")
      }
  end given

  given ESExprCodec[BigInt] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Int)
    override def encode(value: BigInt): ESExpr =
      ESExpr.Int(value)

    override def decode(expr: ESExpr): Either[String, BigInt] =
      expr match {
        case ESExpr.Int(n) => Right(n)
        case _ => Left("Expected an int")
      }
  end given

  given ESExprCodec[ESExpr] with
    override lazy val tags: Set[ESExprTag] = Set.empty
    override def encode(value: ESExpr): ESExpr = value
    override def decode(expr: ESExpr): Either[String, ESExpr] = Right(expr)
  end given

  given [A: ESExprCodec]: ESExprCodec[Seq[A]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor("list"))

    override def encode(value: Seq[A]): ESExpr =
      ESExpr.Constructed(
        "list",
        Map(),
        value.map(summon[ESExprCodec[A]].encode)
      )

    override def decode(expr: ESExpr): Either[String, Seq[A]] =
      expr match {
        case expr: ESExpr.Constructed =>
          for
            _ <- if expr.constructor == "list" then Right(()) else Left("Invalid constructor name for list")
            _ <- if expr.kwargs.isEmpty then Right(()) else Left("Unexpected keyword arguments for list")
            values <- expr.args.traverse(summon[ESExprCodec[A]].decode)
          yield values

        case _ => Left("Expected constructor for list")
      }
  end given

  given[A: ESExprCodec]: ESExprCodec[NonEmptyList[A]] with
    override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Constructor("list"))

    override def encode(value: NonEmptyList[A]): ESExpr =
      summon[ESExprCodec[Seq[A]]].encode(value.toList)

    override def decode(expr: ESExpr): Either[String, NonEmptyList[A]] =
      summon[ESExprCodec[Seq[A]]].decode(expr).flatMap { values =>
        NonEmptyList.fromList(values.toList).toRight("List was expected to be non-empty")
      }
  end given

  override def simpleEnumCodec[T](caseNames: Array[String], caseValues: Map[String, T])(using m: Mirror.SumOf[T]): ESExprCodec[T] =
    new ESExprCodec[T] {
      override lazy val tags: Set[ESExprTag] = Set(ESExprTag.Str)

      override def encode(value: T): ESExpr =
        ESExpr.Str(caseNames(m.ordinal(value)))

      override def decode(expr: ESExpr): Either[String, T] =
        expr match {
          case ESExpr.Str(s) =>
            caseValues.get(s).toRight { s"Invalid simple enum value: $s" }

          case _ =>
            Left("Expected a string for enum value")
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
                ESExprCodec.derivedProduct[U](using summonInline[Mirror.ProductOf[U]]).encode($uValue)
              }
            })
          }

        override def decode(expr: ESExpr): Either[String, T] =
          val tag = ESExprTag.fromExpr(expr)

          ${codecMap}.get(tag).toRight(s"Unexpected tag: $tag (valid tags: ${tags})")
            .flatMap { codec => codec.codec.decode(expr) }
        end decode
      }
    }


  override type TCodecProduct[T] = ESExprCodecProduct[T]
  override type TCodecField[T] = ESExprCodecField[T]

  trait ESExprCodecProduct[T] {
    def encode(value: T): (Map[String, ESExpr], List[ESExpr])
    def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, T]
  }

  trait ESExprCodecField[T] {
    def defaultValue: Option[T]
    def encode(value: T): Option[ESExpr]
    def decode(expr: ESExpr): Either[String, T]
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

      override def decode(expr: ESExpr): Either[String, T] =
        expr match {
          case expr: ESExpr.Constructed =>
            for
              _ <- if expr.constructor == constructor then Right(()) else Left("Unexpected constructor name")
              res <- codecProduct.decode(expr.kwargs, expr.args.toList)
            yield m.fromTuple(res)

          case _ =>
            Left("Expected a constructed value")
        }
    }

  override def optionalFieldCodec[Elem](elemCodec: ESExprCodec[Elem]): ESExprCodecField[Option[Elem]] =
    new ESExprCodecField[Option[Elem]] {
      override def defaultValue: Option[Option[Elem]] = Some(None)

      override def encode(value: Option[Elem]): Option[ESExpr] =
        value.map(elemCodec.encode)

      override def decode(expr: ESExpr): Either[String, Option[Elem]] =
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

      override def decode(expr: ESExpr): Either[String, Elem] = elemCodec.decode(expr)
    }

  override def codecToFieldCodec[Elem](elemCodec: ESExprCodec[Elem]): ESExprCodecField[Elem] =
    new ESExprCodecField[Elem] {
      override def defaultValue: Option[Elem] = None
      override def encode(value: Elem): Option[ESExpr] = Some(elemCodec.encode(value))
      override def decode(expr: ESExpr): Either[String, Elem] = elemCodec.decode(expr)
    }


  override def dictProductCodec[Elem, Tail <: Tuple](elemCodec: ESExprCodec[Elem], tailCodec: ESExprCodecProduct[Tail]): ESExprCodecProduct[Map[String, Elem] *: Tail] =
    new ESExprCodecProduct[Map[String, Elem] *: Tail] {
      override def encode(value: Map[String, Elem] *: Tail): (Map[String, ESExpr], List[ESExpr]) =
        val (h *: t) = value
        val (kwargs, args) = tailCodec.encode(t)
        (kwargs ++ h.view.mapValues(elemCodec.encode), args)
      end encode

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Map[String, Elem] *: Tail] =
        for
          decoded <- kwargs.toSeq.traverse((k, v) =>
            for
              v2 <- elemCodec.decode(v)
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

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, A *: Tail] =
        for
          decoded <- kwargs.get(keyName) match {
            case Some(value) => fieldCodec.decode(value)
            case None => fieldCodec.defaultValue.toRight(s"Required key $keyName was not provided")
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

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Seq[Elem] *: EmptyTuple] =
        if kwargs.nonEmpty then
          Left(s"Extra keyword arguments were provided for $typeName: ${kwargs.keySet}")
        else
          args.traverse(elemCodec.decode)
            .map(_ *: EmptyTuple)
    }

  override def positionalProductCodec[Elem, Tail <: Tuple](elemCodec: ESExprCodec[Elem], tailCodec: ESExprCodecProduct[Tail]): ESExprCodecProduct[Elem *: Tail] =
    new ESExprCodecProduct[Elem *: Tail] {
      override def encode(value: Elem *: Tail): (Map[String, ESExpr], List[ESExpr]) =
        val (h *: t) = value
        val (kwargs, args) = tailCodec.encode(t)
        (kwargs, elemCodec.encode(h) :: args)
      end encode

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Elem *: Tail] =
        args match {
          case h :: t =>
            for
              decoded <- elemCodec.decode(h)
              tailDecoded <- tailCodec.decode(kwargs, t)
            yield decoded *: tailDecoded

          case _ =>
            Left("Not enough arguments were provided")
        }
    }


  override def emptyProductCodec: ESExprCodecProduct[EmptyTuple] =
    new ESExprCodecProduct[EmptyTuple] {
      override def encode(value: EmptyTuple): (Map[String, ESExpr], List[ESExpr]) = (Map.empty, List.empty)

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, EmptyTuple] =
        if kwargs.nonEmpty || args.nonEmpty then
          Left("Extra arguments were provided")
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

      override def decode(expr: ESExpr): Either[String, T] =
        elemCodec.decode(expr).map(res => m.fromTuple(res *: EmptyTuple))
    }
}

