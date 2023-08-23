package dev.argon.esexpr

import dev.argon.util.{*, given}

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.{erasedValue, summonInline, constValue}

trait ESExprCodec[T] {
  lazy val constructors: Set[String]
  def encode(value: T): ESExpr
  def decode(expr: ESExpr): Either[String, T]
}

object ESExprCodec extends ESExprCodecObjectPlatformSpecific {


  given ESExprCodec[ESExpr] with
    override lazy val constructors: Set[String] = Set.empty
    override def encode(value: ESExpr): ESExpr = value
    override def decode(expr: ESExpr): Either[String, ESExpr] = Right(expr)
  end given



  final case class ConstructedInfo(constructor: String, kwargs: Map[String, ESExpr], args: Seq[ESExpr])

  inline def derived[T](using m: Mirror.Of[T]): ESExprCodec[T] =
    inline m match {
      case m: Mirror.SumOf[T] => derivedSum[T](using m)
      case m: Mirror.ProductOf[T] => derivedProduct[T](using m)
    }

  inline def typeHasInlineAnn[T]: Boolean =
    ${ typeHasInlineAnnMacro[T] }

  private def typeHasInlineAnnMacro[T: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect.{*, given}

    val tRep = TypeRepr.of[T]
    val t = tRep.typeSymbol

    val res = t.hasAnnotation(TypeRepr.of[inlineValue].typeSymbol)

    Expr(res)
  end typeHasInlineAnnMacro

  inline def derivedSum[T](using m: Mirror.SumOf[T]): ESExprCodec[T] =
    lazy val (codecMap, codecList) = buildSumCodecs[T, m.MirroredElemTypes]
    derivedSumCreateCodec(codecMap, codecList)
  end derivedSum

  inline def buildSumCodecs[T, SubTypes <: Tuple]: (Map[String, ESExprCodec[? <: T]], List[ESExprCodec[? <: T]]) =
    inline erasedValue[SubTypes] match
      case _: (htype *: ttypes) =>
        val hcodec =  derivedProduct[htype](using summonInline[Mirror.ProductOf[htype]])
        val (tailMap, tailList) = buildSumCodecs[T, ttypes]
        val hcodec2 = summonInline[ESExprCodec[htype] <:< ESExprCodec[? <: T]](hcodec)

        if hcodec.constructors.nonEmpty then
          (tailMap ++ hcodec.constructors.map(_ -> hcodec2), tailList)
        else
          (tailMap, hcodec2 :: tailList)

      case _: EmptyTuple => (Map.empty, Nil)
    end match

  inline def derivedSumCreateCodec[T](codecMap: => Map[String, ESExprCodec[? <: T]], codecList: => List[ESExprCodec[? <: T]])(using m: Mirror.SumOf[T]): ESExprCodec[T] =
    ${ derivedSumMacro[T, m.MirroredElemTypes]('codecMap, 'codecList) }

  def derivedSumMacro[T: Type, SubTypes <: Tuple: Type](codecMap: Expr[Map[String, ESExprCodec[? <: T]]], codecList: Expr[List[ESExprCodec[? <: T]]])(using q: Quotes): Expr[ESExprCodec[T]] =
    '{
      new ESExprCodec[T] {
        override lazy val constructors: Set[String] = ${codecMap}.keySet

        override def encode(value: T): ESExpr =
          ${
            MacroUtils.patternMatch[T, SubTypes, ESExpr]('value)([U] => (uValue: Expr[U], uType: Type[U]) => {
              given Type[U] = uType
              '{
                derivedProduct[U](using summonInline[Mirror.ProductOf[U]]).encode($uValue)
              }
            })
          }

        override def decode(expr: ESExpr): Either[String, T] =
          ESExprCodec.getConstructedInfo(expr) match {
            case Some(ConstructedInfo(constructor, _, _)) =>
              ${codecMap}.get(constructor).toRight(s"Unexpected constructor: $constructor (valid constructors: ${constructors})")
                .flatMap { codec => codec.decode(expr) }

            case _: None.type =>
              ${codecList}
                .view
                .map(_.decode(expr))
                .collectFirst { case Right(value) => value }
                .toRight(s"Unexpected value: ${expr}")
          }
      }
    }


  inline def derivedProduct[T](using m: Mirror.ProductOf[T]): ESExprCodec[T] =
    inline if typeHasInlineAnn[T] then
      derivedInline[T](using m)
    else
      val derivedTuple = derivedProductTuple[T, m.MirroredLabel, m.MirroredElemLabels, m.MirroredElemTypes]
      val constructor =
        inline if typeHasConstructorAnn[T] then
          typeGetConstructorAnn[T].name
        else
          constValue[m.MirroredLabel]
            .split("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])").nn
            .map(_.nn.toLowerCase)
            .mkString("-")

      new ESExprCodec[T] {
        override lazy val constructors: Set[String] = Set(constructor)

        override def encode(value: T): ESExpr =
          val (kwargs, args) = derivedTuple.encode(
            Tuple.fromProductTyped[T & Product](
              summonInline[T =:= (T & Product)](value)
            )(using summonInline[Mirror.ProductOf[T] {type MirroredElemTypes = m.MirroredElemTypes} =:= Mirror.ProductOf[T & Product] {type MirroredElemTypes = m.MirroredElemTypes}](m))
          )

          constructedInfoToExpr(ConstructedInfo(constructor, kwargs, args))
        end encode

        override def decode(expr: ESExpr): Either[String, T] =
          for
            info <- getConstructedInfo(expr).toRight("Expected a constructed value")
            _ <- if info.constructor == constructor then Right(()) else Left("Unexpected constructor name")
            res <- derivedTuple.decode(info.kwargs, info.args.toList)
          yield m.fromTuple(res)
      }
    end if

  trait ESExprCodecProduct[T] {
    def encode(value: T): (Map[String, ESExpr], List[ESExpr])
    def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, T]
  }

  trait ESExprCodecField[T] {
    def defaultValue: Option[T]
    def encode(value: T): Option[ESExpr]
    def decode(expr: ESExpr): Either[String, T]
  }

  inline def typeHasConstructorAnn[T]: Boolean =
    ${ typeHasConstructorAnnMacro[T] }

  private def typeHasConstructorAnnMacro[T: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect.{*, given}

    val tRep = TypeRepr.of[T]
    val t = tRep match {
      case tRep: TermRef => tRep.termSymbol
      case _ => tRep.typeSymbol
    }

    val res = t.hasAnnotation(TypeRepr.of[constructor].typeSymbol)

    Expr(res)
  end typeHasConstructorAnnMacro


  inline def typeGetConstructorAnn[T]: constructor =
    ${ typeGetConstructorAnnMacro[T] }

  private def typeGetConstructorAnnMacro[T: Type](using q: Quotes): Expr[constructor] =
    import q.reflect.{*, given}

    val tRep = TypeRepr.of[T]
    val t = tRep match {
      case tRep: TermRef => tRep.termSymbol
      case _ => tRep.typeSymbol
    }

    val res = t.getAnnotation(TypeRepr.of[constructor].typeSymbol).getOrElse(throw new Exception("Type is missing constructor annotation"))

    res.asExprOf[constructor]
  end typeGetConstructorAnnMacro

  inline def typeHasVarArgs[T]: Boolean =
    ${ typeHasVarArgsMacro[T] }

  private def asMatchable[A](a: A): A & Matchable =
    a.asInstanceOf[A & Matchable]

  private def typeHasVarArgsMacro[T: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect.{*, given}

    val tRep = TypeRepr.of[T]
    val t = tRep.typeSymbol

    val res = t
      .caseFields
      .lastOption
      .map(tRep.memberType)
      .map[TypeRepr & Matchable](asMatchable)
      .collect {
        case AnnotatedType(_, Apply(Select(New(t), "<init>"), List())) => t
      }
      .map[TypeRepr & Matchable] { t => asMatchable(t.tpe) }
      .collect {
        case TypeRef(t, "Repeated") => t
      }
      .map[TypeRepr & Matchable](asMatchable)
      .collect {
        case ThisType(t) => t
      }
      .map[TypeRepr & Matchable](asMatchable)
      .collect {
        case TypeRef(t, "internal") => t
      }
      .map[TypeRepr & Matchable](asMatchable)
      .collect {
        case NoPrefix() =>
      }
      .isDefined

    Expr(res)
  end typeHasVarArgsMacro


  inline def caseFieldIsKeywordArg[T](fieldName: String): Boolean =
    ${ caseFieldIsKeywordArgMacro[T]('fieldName) }

  private def caseFieldIsKeywordArgMacro[T: Type](fieldName: Expr[String])(using q: Quotes): Expr[Boolean] =
    import q.reflect.{*, given}

    val fieldNameStr = fieldName.valueOrAbort

    val tRep = TypeRepr.of[T]
    val t = tRep.typeSymbol

    val res = t.primaryConstructor
      .paramSymss
      .flatten
      .exists { param =>
        param.name == fieldNameStr &&
          param.hasAnnotation(TypeRepr.of[keyword].typeSymbol)
      }

    Expr(res)
  end caseFieldIsKeywordArgMacro


  inline def caseFieldGetKeywordAnn[T](fieldName: String): keyword =
    ${ caseFieldGetKeywordAnnMacro[T]('fieldName) }

  private def caseFieldGetKeywordAnnMacro[T: Type](fieldName: Expr[String])(using q: Quotes): Expr[keyword] =
    import q.reflect.{*, given}

    val fieldNameStr = fieldName.valueOrAbort

    val tRep = TypeRepr.of[T]
    val t = tRep.typeSymbol

    val res = t.primaryConstructor
      .paramSymss
      .flatten
      .filter(param => param.name == fieldNameStr)
      .flatMap(param => param.getAnnotation(TypeRepr.of[keyword].typeSymbol))
      .headOption
      .getOrElse(throw new Exception("Could not find keyword annotation"))

    res.asExprOf[keyword]
  end caseFieldGetKeywordAnnMacro

  inline def caseFieldHasDefaultValue[T, A](fieldName: String): Boolean =
    ${ caseFieldHasDefaultValueMacro[T, A]('fieldName) }

  private def caseFieldHasDefaultValueMacro[T: Type, A: Type](fieldName: Expr[String])(using q: Quotes): Expr[Boolean] =
    import q.reflect.{*, given}

    val fieldNameStr = fieldName.valueOrAbort

    val tRep = TypeRepr.of[T]
    val t = tRep.typeSymbol
    val compClass = t.companionClass

    val index = t.caseFields.indexWhere(_.name == fieldNameStr)

    val res = compClass.tree match {
      case ClassDef(_, _, _, _, body) =>
        body
          .exists {
            case dd: DefDef => dd.name == "$lessinit$greater$default$" + index
            case _ => false
          }

      case _ => false
    }

    Expr(res)
  end caseFieldHasDefaultValueMacro

  inline def caseFieldDefaultValue[T, A](fieldName: String): A =
    ${ caseFieldDefaultValueMacro[T, A]('fieldName) }

  def caseFieldDefaultValueMacro[T: Type, A: Type](fieldName: Expr[String])(using q: Quotes): Expr[A] =
    import q.reflect.{*, given}

    val fieldNameStr = fieldName.valueOrAbort

    val tRep = TypeRepr.of[T]
    val t = tRep.typeSymbol.companionClass
    val compMod = Ref(t.companionModule)

    val index = t.caseFields.indexWhere(_.name == fieldNameStr)

    val defaultValue = t.tree match {
      case ClassDef(_, _, _, _, body) =>
        body
          .collectFirst {
            case dd: DefDef if dd.name == "$lessinit$greater$default$" + index =>
              compMod.select(dd.symbol)
          }
          .getOrElse { throw new Exception("Could not find default case field value") }

      case _ => throw new Exception("Could not find default case field value")
    }

    defaultValue.asExprOf[A]
  end caseFieldDefaultValueMacro


  inline def derivedProductTuple[T, TypeLabel <: String, Labels <: Tuple, Types <: Tuple]: ESExprCodecProduct[Types] =
    inline (erasedValue[Labels], erasedValue[Types]) match
      case _: ((? *: EmptyTuple), (Seq[elem] *: EmptyTuple)) if typeHasVarArgs[T] =>
        lazy val elemCodec = summonInline[ESExprCodec[elem]]
        val codec = new ESExprCodecProduct[Seq[elem] *: EmptyTuple] {
          override def encode(value: Seq[elem] *: EmptyTuple): (Map[String, ESExpr], List[ESExpr]) =
            val (elems *: _) = value
            (Map.empty, elems.map(elemCodec.encode).toList)
          end encode

          override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, Seq[elem] *: EmptyTuple] =
            if kwargs.nonEmpty then
              Left("Extra keyword arguments were provided")
            else
              args.traverse(elemCodec.decode)
                .map(_ *: EmptyTuple)
        }

        summonInline[ESExprCodecProduct[Seq[elem] *: EmptyTuple] =:= ESExprCodecProduct[Types]](codec)

      case _: ((hlabel *: tlabels), (Option[elem] *: ttypes)) if caseFieldIsKeywordArg[T](constValue[hlabel & String]) =>

        val elemCodec = summonInline[ESExprCodec[elem]]
        val hcodec = new ESExprCodecField[Option[elem]] {
          override def defaultValue: Option[Option[elem]] = Some(None)

          override def encode(value: Option[elem]): Option[ESExpr] =
            value.map(elemCodec.encode)

          override def decode(expr: ESExpr): Either[String, Option[elem]] =
            elemCodec.decode(expr).map(Some.apply)
        }

        summonInline[ESExprCodecProduct[Option[elem] *: ttypes] =:= ESExprCodecProduct[Types]](
          deriveProductKeyword[T, TypeLabel, hlabel, tlabels, Option[elem], ttypes](hcodec)
        )

      case _: ((hlabel *: tlabels), (htype *: ttypes)) if caseFieldIsKeywordArg[T](constValue[hlabel & String]) =>
        val elemCodec = summonInline[ESExprCodec[htype]]
        lazy val headCodec =
          inline if caseFieldHasDefaultValue[T, htype](constValue[hlabel & String]) then
            given CanEqual[htype, htype] = summonInline[CanEqual[htype, htype]]

            new ESExprCodecField[htype] {
              override def defaultValue: Option[htype] =
                Some(caseFieldDefaultValue[T, htype](constValue[hlabel & String]))

              override def encode(value: htype): Option[ESExpr] =
                if value == caseFieldDefaultValue[T, htype](constValue[hlabel & String]) then
                  Some(elemCodec.encode(value))
                else
                  None

              override def decode(expr: ESExpr): Either[String, htype] = elemCodec.decode(expr)
            }
          else
            new ESExprCodecField[htype] {
              override def defaultValue: Option[htype] = None
              override def encode(value: htype): Option[ESExpr] = Some(elemCodec.encode(value))
              override def decode(expr: ESExpr): Either[String, htype] = elemCodec.decode(expr)
            }
          end if

        summonInline[ESExprCodecProduct[htype *: ttypes] =:= ESExprCodecProduct[Types]](
          deriveProductKeyword[T, TypeLabel, hlabel, tlabels, htype, ttypes](headCodec)
        )


      case _: ((? *: tlabels), (htype *: ttypes)) =>
        lazy val headCodec = summonInline[ESExprCodec[htype]]
        val tailCodec = derivedProductTuple[T, TypeLabel, tlabels, ttypes]
        val codec = new ESExprCodecProduct[htype *: ttypes] {
          override def encode(value: htype *: ttypes): (Map[String, ESExpr], List[ESExpr]) =
            val (h *: t) = value
            val (kwargs, args) = tailCodec.encode(t)
            (kwargs, headCodec.encode(h) :: args)
          end encode

          override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, htype *: ttypes] =
            args match {
              case h :: t =>
                for
                  decoded <- headCodec.decode(h)
                  tailDecoded <- tailCodec.decode(kwargs, t)
                yield decoded *: tailDecoded

              case _ =>
                Left("Not enough arguments were provided")
            }
        }

        summonInline[ESExprCodecProduct[htype *: ttypes] =:= ESExprCodecProduct[Types]](codec)

      case _: (EmptyTuple, EmptyTuple) =>
        summonInline[ESExprCodecProduct[EmptyTuple] =:= ESExprCodecProduct[Types]](new ESExprCodecProduct[EmptyTuple] {
          override def encode(value: EmptyTuple): (Map[String, ESExpr], List[ESExpr]) = (Map.empty, List.empty)
          override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, EmptyTuple] =
            if kwargs.nonEmpty || args.nonEmpty then
              Left("Extra arguments were provided")
            else
              Right(EmptyTuple)
        })
    end match

  inline def deriveProductKeyword[T, TypeLabel <: String, HLabel, TLabels <: Tuple, HType, TTypes <: Tuple](headCodec: ESExprCodecField[HType]): ESExprCodecProduct[HType *: TTypes] =
    val keyName =
      val name = caseFieldGetKeywordAnn[T](constValue[HLabel & String]).name
      if name.isEmpty then
        constValue[HLabel & String]
      else
        name
    end keyName

    val tailCodec = derivedProductTuple[T, TypeLabel, TLabels, TTypes]
    new ESExprCodecProduct[HType *: TTypes] {
      override def encode(value: HType *: TTypes): (Map[String, ESExpr], List[ESExpr]) =
        val (h *: t) = value
        val (kwargs, args) = tailCodec.encode(t)
        (headCodec.encode(h).fold(kwargs)(h => kwargs + (keyName -> h)), args)
      end encode

      override def decode(kwargs: Map[String, ESExpr], args: List[ESExpr]): Either[String, HType *: TTypes] =
        for
          decoded <- kwargs.get(keyName) match {
            case Some(value) => headCodec.decode(value)
            case None => headCodec.defaultValue.toRight("Required key was not provided")
          }
          tailDecoded <- tailCodec.decode(kwargs.removed(keyName), args)
        yield decoded *: tailDecoded

    }
  end deriveProductKeyword


  inline def derivedInline[T](using m: Mirror.ProductOf[T]): ESExprCodec[T] =
    inline erasedValue[m.MirroredElemTypes] match {
      case _: (elem *: EmptyTuple) =>
        lazy val elemCodec = summonInline[ESExprCodec[elem]]
        new ESExprCodec[T] {
          override lazy val constructors: Set[String] = elemCodec.constructors

          override def encode(value: T): ESExpr = {
            val valueProduct: T & Product = summonInline[T =:= (T & Product)](value)
            val mProduct = summonInline[Mirror.ProductOf[T] { type MirroredElemTypes = m.MirroredElemTypes } =:= Mirror.ProductOf[T & Product] { type MirroredElemTypes = m.MirroredElemTypes }](m)
            val (elemValue *: _) = summonInline[m.MirroredElemTypes =:= elem *: EmptyTuple](Tuple.fromProductTyped(valueProduct)(using mProduct))
            elemCodec.encode(elemValue)
          }

          override def decode(expr: ESExpr): Either[String, T] =
            elemCodec.decode(expr).map { elemValue => m.fromTuple(summonInline[elem *: EmptyTuple =:= m.MirroredElemTypes](elemValue *: EmptyTuple)) }
        }
    }
}

