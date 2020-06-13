package dev.argon.compiler.types

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.AnyExtensions._
import cats._
import cats.implicits._
import cats.data._
import cats.evidence.===
import dev.argon.compiler.core.ErasedSignature.TupleType
import dev.argon.compiler.expr._
import dev.argon.compiler.expr.ArExpr._
import shapeless.Nat
import zio.IO
import zio.interop.catz.core._

import scala.collection.immutable.Vector

trait TypeSystem {

  val context: Context

  type TTypeWrapper[+_]

  type SimpleExpr = ArExpr[context.type, TTypeWrapper]
  type WrapExpr = TTypeWrapper[SimpleExpr]
  type TType = WrapExpr
  
  type TSubTypeInfo = SubTypeInfo[TType]

  type TFieldVariable = FieldVariable[context.type, TTypeWrapper]

  type TClassType = ClassType[context.type, TTypeWrapper]
  type TTraitType = TraitType[context.type, TTypeWrapper]
  type TDataConstructorType = DataConstructorType[context.type, TTypeWrapper]

  def unwrapType[A](t: TTypeWrapper[A]): Option[A]

  implicit val typeWrapperInstances: WrapperInstance[TTypeWrapper]

  final def fromSimpleType(simpleType: ArExpr[context.type, TTypeWrapper]): TType =
    simpleType.pure[TTypeWrapper]

  def wrapExprType(expr: WrapExpr): Comp[TType]

  def isSubTypeWrapper(a: TType, b: TType): Comp[Option[SubTypeInfo[TType]]] =
    isSubTypeWrapperImpl(a, b).flatMap {
      case Left((aInner, bInner)) => isSimpleSubType(aInner, bInner)
      case Right(result) => IO.succeed(result)
    }

  def isSubTypeWrapperImpl[A](a: TTypeWrapper[A], b: TTypeWrapper[A]): Comp[Either[(A, A), Option[SubTypeInfo[TTypeWrapper[A]]]]]

  def universeOfWrapExpr(expr: WrapExpr): Comp[UniverseExpr] =
    universeOfWrapExprImpl(expr).flatMap {
      case Left(expr) => universeOfExpr(expr)
      case Right(universe) => IO.succeed(universe)
    }

  def universeOfWrapExprImpl[A](expr: TTypeWrapper[A]): Comp[Either[A, UniverseExpr]]

  final def isSubType(a: TType, b: TType): Comp[Option[TSubTypeInfo]] = for {
    a2 <- a.flatTraverse(reduceExprToValue)
    b2 <- b.flatTraverse(reduceExprToValue)
    res <- isSubTypeWrapper(a2, b2)
  } yield res

  // B <: A
  protected final def isSimpleSubType(a: ArExpr[context.type, TTypeWrapper], b: ArExpr[context.type, TTypeWrapper]): Comp[Option[TSubTypeInfo]] = {

    val notSubType = Option.empty[TSubTypeInfo].pure[Comp]

    def compareTypeArg(a: WrapExpr, b: WrapExpr): Comp[Option[Vector[TSubTypeInfo]]] = {
      def fromIsSame(b: Boolean): Comp[Option[Vector[TSubTypeInfo]]] =
        if(b)
          Some(Vector.empty).upcast[Option[Vector[TSubTypeInfo]]].pure[Comp]
        else
          Option.empty[Vector[TSubTypeInfo]].pure[Comp]

      (unwrapType(a), unwrapType(b)) match {

        case (Some(LoadConstantString(a, _)), Some(LoadConstantString(b, _))) =>
          fromIsSame(a === b)

        case (Some(LoadConstantInt(a, _)), Some(LoadConstantInt(b, _))) =>
          fromIsSame(a === b)

        case (Some(LoadConstantBool(a, _)), Some(LoadConstantBool(b, _))) =>
          fromIsSame(a === b)

        case (Some(LoadUnit(_)), Some(LoadUnit(_))) => fromIsSame(true)

        case (Some(LoadLambda(aVar, aBody)), Some(LoadLambda(bVar, bBody))) =>
          @SuppressWarnings(Array("org.wartremover.warts.Equals"))
          def isSameLambda = aBody == Substitutions(context)(bVar, fromSimpleType(LoadVariable(aVar))).substWrapExpr(bBody)
          fromIsSame(isSameLambda)

        case _ =>
          (
            for {
              proof1 <- OptionT(isSubType(a, b))
              proof2 <- OptionT(isSubType(b, a))
            } yield Vector(SubTypeInfo(a, b, Vector(proof1, proof2)))
          ).value
      }
    }

    def compareArguments(aType: TType, bType: TType)(a: Vector[WrapExpr])(b: Vector[WrapExpr]): Comp[Option[TSubTypeInfo]] =
      if(a.size === b.size)
        a.zip(b)
          .traverse { case (aArg, bArg) => OptionT(compareTypeArg(aArg, bArg)) }
          .map { args => SubTypeInfo(aType, bType, args.flatten) }
          .value
      else
        notSubType

    def isSubTrait(a: TTraitType)(b: TTraitType): Comp[Option[TSubTypeInfo]] =
      if(a.arTrait.value.id === b.arTrait.value.id)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        for {
          sig <- b.arTrait.value.signature
          resultInfo <- SignatureContext.liftSignatureResult(context)(sig, b.args)
          baseTypes <- resultInfo.baseTypes
          result <- baseTypes.baseTraits.collectFirstSomeM(isSubTrait(a))
        } yield result

    def isSubClass(a: TClassType)(b: TClassType): Comp[Option[TSubTypeInfo]] =
      if(a.arClass.value.id === b.arClass.value.id)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        for {
          sig <- b.arClass.value.signature
          resultInfo <- SignatureContext.liftSignatureResult(context)(sig, b.args)
          baseTypes <- resultInfo.baseTypes
          result <- baseTypes.baseClass.collectFirstSomeM(isSubClass(a))
        } yield result

    def classImplementsTrait(a: TTraitType)(b: TClassType): Comp[Option[TSubTypeInfo]] = for {
      sig <- b.arClass.value.signature
      resultInfo <- SignatureContext.liftSignatureResult(context)(sig, b.args)
      baseTypes <- resultInfo.baseTypes
      result <-
        Vector(
          () => baseTypes.baseTraits.collectFirstSomeM(isSubTrait(a)),
          () => baseTypes.baseClass.collectFirstSomeM(classImplementsTrait(a)),
        ).collectFirstSomeM(_())

    } yield result

    def isSameDataCtor(a: TDataConstructorType)(b: TDataConstructorType): Comp[Option[TSubTypeInfo]] =
      if(a.ctor.value.id === b.ctor.value.id)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        Option.empty[TSubTypeInfo].pure[Comp]

    def unMetaType(t: SimpleExpr): Vector[TType] =
      t match {
        case TypeOfType(inner) =>
          Vector(inner)

        case IntersectionType(b1, b2) =>
          unMetaTypeWrapped(b1) ++ unMetaTypeWrapped(b2)

        case LoadTuple(values) =>
          values
            .traverse {
              case TupleElement(value) =>
                unMetaTypeWrapped(value).map(TupleElement.apply)
            }
            .map { newValues => fromSimpleType(LoadTuple(newValues)) }

        case _ => Vector()
      }

    def unMetaTypeWrapped(t: TType): Vector[TType] =
      unwrapType(t).toList.toVector.flatMap(unMetaType)

    Vector(
      () => a match {
        case a @ IntersectionType(_, _) =>
          isSubType(a.first, fromSimpleType(b)).flatMap {
            case Some(left) =>
              isSubType(a.second, fromSimpleType(b)).map { _.map { right =>
                SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(left, right))
              } }

            case None => notSubType
          }
        case _ => notSubType
      },
      () => b match {
        case b @ UnionType(_, _) =>
          isSubType(fromSimpleType(a), b.first).flatMap {
            case Some(left) =>
              isSubType(fromSimpleType(a), b.second).map { _.map { right =>
                SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(left, right))
              } }

            case None => notSubType
          }

        case _ => notSubType
      },
      () => b match {
        case b @ IntersectionType(_, _) =>
          Vector(
            () => isSubType(fromSimpleType(a), b.first),
            () => isSubType(fromSimpleType(a), b.second),
          )
            .collectFirstSomeM(_())
            .map { _.map { info => SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(info)) } }

        case _ => notSubType
      },
      () => a match {
        case a @ UnionType(_, _) =>
          Vector(
            () => isSubType(a.first, fromSimpleType(b)),
            () => isSubType(a.second, fromSimpleType(b)),
          )
            .collectFirstSomeM(_())
            .map { _.map { info => SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(info)) } }
        case _ => notSubType
      },
      () => a match {
        case LoadTuple(NonEmptyList(TupleElement(a), Nil)) =>
          isSubType(a, fromSimpleType(b))
        case _ => notSubType
      },
      () => b match {
        case LoadTuple(NonEmptyList(TupleElement(b), Nil)) =>
          isSubType(fromSimpleType(a), b)
        case _ => notSubType
      },
      () => (a, b) match {

        case (aTrait @ TraitType(_, _), bTrait @ TraitType(_, _)) => isSubTrait(aTrait)(bTrait)
        case (aClass @ ClassType(_, _), bClass @ ClassType(_, _)) => isSubClass(aClass)(bClass)
        case (aTrait @ TraitType(_, _), bClass @ ClassType(_, _)) => classImplementsTrait(aTrait)(bClass)
        case (ClassType(_, _), TraitType(_, _)) => notSubType

        case (aDataCtor @ DataConstructorType(_, _, _), bDataCtor @ DataConstructorType(_, _, _)) =>
          isSameDataCtor(aDataCtor)(bDataCtor)

        case (aTrait @ TraitType(_, _), bDataCtor @ DataConstructorType(_, _, _)) =>
          isSubTrait(aTrait)(bDataCtor.instanceType)


        case (aTuple @ LoadTuple(_), bTuple @ LoadTuple(_)) =>
          if(aTuple.values.size =!= bTuple.values.size)
            notSubType
          else
            aTuple.values.toList.toVector.zip(bTuple.values.toList.toVector)
              .traverse { case (aElem, bElem) =>
                OptionT(isSubType(aElem.value, bElem.value))
              }
              .map(SubTypeInfo(fromSimpleType(a), fromSimpleType(b), _))
              .value

        case (FunctionType(argA, resA), FunctionType(argB, resB)) =>
          (
            for {
              argCheck <- OptionT(isSubType(argB, argA))
              resCheck <- OptionT(isSubType(resA, resB))
            } yield SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(argCheck, resCheck))
          ).value

        case (TypeOfType(innerA), TypeOfType(innerB)) =>
          (
            for {
              c1 <- OptionT(isSubType(innerA, innerB))
              c2 <- OptionT(isSubType(innerB, innerA))
            } yield SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(c1, c2))
          ).value

        case (TypeN(uA, subtypeConstraint, supertypeConstraint), TypeOfType(innerB)) =>
          universeOfExpr(b)
            .flatMap { uB => universeSubsumes(uA, uB) }
            .flatMap {
              case false => notSubType
              case true =>
                (
                  subtypeConstraint.map { sub => OptionT(isSubType(innerB, sub)) }.toList ++
                    supertypeConstraint.map { sup => OptionT(isSubType(sup, innerB)) }.toList
                  ).toVector.sequence.value
                  .map { _.map {
                    SubTypeInfo(fromSimpleType(a), fromSimpleType(b), _)
                  } }
            }

        case (TypeN(uA, _, _), b @ LoadTuple(_)) =>
          universeOfExpr(b)
            .flatMap { uB => universeSubsumes(uA, uB) }
            .flatMap {
              case false => notSubType
              case true =>
                SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector.empty).pure[Option].pure[Comp]
            }


        case (TypeN(uA, subA, supA), TypeN(uB, subB, supB)) =>
          universeSubsumes(uA, uB)
            .flatMap {
              case false => notSubType
              case true =>
                (
                  ((subA, subB) match {
                    case (Some(subA), Some(subB)) => Vector(OptionT(isSubType(subB, subA)))
                    case _ => Vector()
                  }) ++
                    ((supA, supB) match {
                      case (Some(supA), Some(supB)) => Vector(OptionT(isSubType(supA, supB)))
                      case _ => Vector()
                    })
                  ).sequence.value
                  .map { _.map {
                    SubTypeInfo(fromSimpleType(a), fromSimpleType(b), _)
                  } }
            }

        case (LoadVariable(varA), LoadVariable(varB)) if varA === varB =>
          SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector.empty).pure[Option].pure[Comp]

        case (FunctionObjectCall(functionA, argA, _), FunctionObjectCall(functionB, argB, _)) =>
          compareArguments(fromSimpleType(a), fromSimpleType(b))(
            Vector(functionA, argA)
          )(
            Vector(functionB, argB)
          )

        case (_, _) => notSubType
      },
      () => b match {
        case _: TypeIsTypeOfTypeExpr[context.type, TTypeWrapper] | LoadTuple(_) => notSubType
        case _ =>
          def handleBType(bType: TType): Comp[Option[TSubTypeInfo]] =
            reduceWrapExprToValue(bType).flatMap { bType =>
              unwrapType(bType) match {
                case Some(TypeN(_, Some(subtypeConstraint), _)) =>
                  isSubType(fromSimpleType(a), subtypeConstraint)

                case Some(TypeOfType(inner)) =>
                  isSubType(fromSimpleType(a), inner)

                case Some(IntersectionType(b1, b2)) =>
                  Vector(
                    () => handleBType(b1),
                    () => handleBType(b2),
                  )
                    .collectFirstSomeM(_())

                case Some(t @ LoadTuple(_)) =>
                  unMetaType(t).collectFirstSomeM { t =>
                    isSubType(fromSimpleType(a), t)
                  }


                case _ => notSubType
              }
            }

          getExprType(b, includeExtraTypeOfType = false).flatMap(handleBType)
      },
      () => a match {
        case _: TypeIsTypeOfTypeExpr[context.type, TTypeWrapper] | LoadTuple(_) => notSubType
        case _ =>
          def handleAType(aType: TType): Comp[Option[TSubTypeInfo]] =
            reduceWrapExprToValue(aType).flatMap { aType =>
              unwrapType(aType) match {
                case Some(TypeN(_, _, Some(supertypeConstraint))) =>
                  isSubType(supertypeConstraint, fromSimpleType(b))

                case Some(TypeOfType(inner)) =>
                  isSubType(inner, fromSimpleType(b))

                case Some(IntersectionType(a1, a2)) =>
                  Vector(
                    () => handleAType(a1),
                    () => handleAType(a2),
                  )
                    .collectFirstSomeM(_())

                case Some(t @ LoadTuple(_)) =>
                  unMetaType(t).collectFirstSomeM { t =>
                    isSubType(t, fromSimpleType(b))
                  }


                case _ => notSubType
              }
            }

          getExprType(a, includeExtraTypeOfType = false).flatMap(handleAType)
      },
    ).collectFirstSomeM(_())
  }


  final def reduceExprToValue(expr: ArExpr[context.type, TTypeWrapper]): Comp[WrapExpr] =
    expr match {
      // Already reduced, has a constructor at the top level
      case ClassConstructorCall(_, _, _) | DataConstructorCall(_, _) |
           LoadConstantBool(_, _) |
           LoadConstantInt(_, _) |
           LoadConstantString(_, _) |
           LoadLambda(_, _) |
           LoadTuple(_) |
           LoadUnit(_) |
           LoadVariable(_) |
           _: TypeIsTypeOfTypeExpr[context.type, TTypeWrapper] =>
        fromSimpleType(expr).pure[Comp]


      // Potentially reducible after adding inline support
      case FunctionCall(_, _, _) => fromSimpleType(expr).pure[Comp]
      case MethodCall(_, _, _, _, _) => fromSimpleType(expr).pure[Comp]

      case FunctionObjectCall(function, arg, _) =>
        reduceWrapExprToValue(function).flatMap { func =>
          unwrapType(func) match {
            case Some(LoadLambda(argVariable, body)) =>
              for {
                argValue <- reduceWrapExprToValue(arg)
              } yield Substitutions(context)(argVariable, argValue).substWrapExpr(body)

            case _ => fromSimpleType(expr).pure[Comp]
          }
        }

      case EnsureExecuted(body, _) => reduceWrapExprToValue(body)
      case IfElse(_, _, _) => ???
      case PatternMatch(_, _) => ???

      case LetBinding(_, _, _) => ???
      case LoadTupleElement(_, _, _) => ???
      case Sequence(_, second) => reduceWrapExprToValue(second)
      case StoreVariable(_, _, _) => ???
    }

  final def reduceWrapExprToValue(expr: WrapExpr): Comp[WrapExpr] =
    expr.flatTraverse(reduceExprToValue)

  final def getExprType(expr: ArExpr[context.type, TTypeWrapper], includeExtraTypeOfType: Boolean = true): Comp[TType] = {
    def withTypeOfExpr(result: Comp[TType]): Comp[TType] =
      if(includeExtraTypeOfType)
        result.map { t => fromSimpleType(IntersectionType(t, fromSimpleType(TypeOfType(fromSimpleType(expr))))) }
      else
        result

    expr match {
      case ClassConstructorCall(classType, _, _) => fromSimpleType(classType).pure[Comp]
      case DataConstructorCall(dataCtorInstanceType, _) => fromSimpleType(dataCtorInstanceType).pure[Comp]
      case EnsureExecuted(body, _) => getWrapExprType(body)
      case FunctionCall(_, _, returnType) => withTypeOfExpr(returnType.pure[Comp])
      case FunctionObjectCall(_, _, returnType) => withTypeOfExpr(returnType.pure[Comp])
      case IfElse(_, ifBody, elseBody) =>
        for {
          ifType <- getWrapExprType(ifBody)
          elseType <- getWrapExprType(elseBody)
        } yield fromSimpleType(UnionType(ifType, elseType))
      case LetBinding(_, _, next) => getWrapExprType(next)
      case LoadConstantBool(_, exprType) => exprType.pure[Comp]
      case LoadConstantInt(_, exprType) => exprType.pure[Comp]
      case LoadConstantString(_, exprType) => exprType.pure[Comp]
      case LoadLambda(argVariable, body) =>
        for {
          bodyType <- getWrapExprType(body)
        } yield fromSimpleType(FunctionType(argVariable.varType, bodyType))
      case LoadTuple(values) =>
        values
          .traverse { case TupleElement(elem) => wrapExprType(elem).map(TupleElement.apply) }
          .map { elems => fromSimpleType(LoadTuple(elems)) }
      case LoadTupleElement(_, elemType, _) => elemType.pure[Comp]
      case LoadUnit(exprType) => exprType.pure[Comp]
      case LoadVariable(variable) => withTypeOfExpr(variable.varType.pure[Comp])
      case MethodCall(_, _, _, _, returnType) => withTypeOfExpr(returnType.pure[Comp])
      case PatternMatch(_, cases) =>
        cases
          .traverse { patCase => getWrapExprType(patCase.body) }
          .map { _.reduceLeft { (a, b) => fromSimpleType(UnionType(a, b)) } }
      case Sequence(_, second) => getWrapExprType(second)
      case StoreVariable(_, _, exprType) => exprType.pure[Comp]
      case expr: TypeIsTypeOfTypeExpr[context.type, TTypeWrapper] => fromSimpleType(TypeOfType(fromSimpleType(expr))).pure[Comp]
    }
  }

  final def getWrapExprType(expr: WrapExpr, includeExtraTypeOfType: Boolean = true): Comp[TType] =
    unwrapType(expr) match {
      case Some(expr) => getExprType(expr, includeExtraTypeOfType)
      case None => fromSimpleType(TypeOfType(expr)).pure[Comp]
    }

  def universeOfExpr(expr: ArExpr[context.type, TTypeWrapper]): Comp[UniverseExpr] = {
    def universeOfTypeArgs(args: Vector[WrapExpr]): Comp[UniverseExpr] =
      args
        .traverse(universeOfWrapExpr)
        .map { _.reduceLeftOption(LargestUniverse).getOrElse { FixedUniverse(0) } }

    expr match {
      case ClassConstructorCall(classType, _, _) => universeOfTypeArgs(classType.args).map(NextLargestUniverse)
      case DataConstructorCall(dataCtorInstanceType, _) => universeOfTypeArgs(dataCtorInstanceType.args).map(NextLargestUniverse)
      case EnsureExecuted(body, _) => universeOfWrapExpr(body)
      case FunctionCall(_, _, returnType) => universeOfWrapExpr(returnType).map(PreviousUniverse)
      case FunctionObjectCall(_, _, returnType) => universeOfWrapExpr(returnType).map(PreviousUniverse)
      case IfElse(_, ifBody, elseBody) =>
        for {
          ifUniv <- universeOfWrapExpr(ifBody)
          elseUniv <- universeOfWrapExpr(elseBody)
        } yield LargestUniverse(ifUniv, elseUniv)
      case LetBinding(_, _, next) => universeOfWrapExpr(next)
      case LoadConstantBool(_, _) => FixedUniverse(0).upcast[UniverseExpr].pure[Comp]
      case LoadConstantInt(_, _) => FixedUniverse(0).upcast[UniverseExpr].pure[Comp]
      case LoadConstantString(_, _) => FixedUniverse(0).upcast[UniverseExpr].pure[Comp]
      case LoadLambda(argVariable, body) =>
        for {
          argUniv <- universeOfWrapExpr(argVariable.varType)
          resUniv <- universeOfWrapExpr(body)
        } yield LargestUniverse(argUniv, resUniv)
      case LoadTuple(values) =>
        values
          .traverse { case TupleElement(elem) => universeOfWrapExpr(elem) }
          .map { _.reduceLeft(LargestUniverse) }
      case LoadTupleElement(_, elemType, _) => universeOfWrapExpr(elemType).map(PreviousUniverse)
      case LoadUnit(_) => FixedUniverse(0).upcast[UniverseExpr].pure[Comp]
      case LoadVariable(variable) => universeOfWrapExpr(variable.varType).map(PreviousUniverse)
      case MethodCall(_, _, _, _, returnType) => universeOfWrapExpr(returnType).map(PreviousUniverse)
      case PatternMatch(_, cases) =>
        cases
          .traverse { patCase => universeOfWrapExpr(patCase.body) }
          .map { _.reduceLeft(LargestUniverse) }
      case Sequence(_, second) => universeOfWrapExpr(second)
      case StoreVariable(_, _, exprType) => universeOfWrapExpr(exprType).map(PreviousUniverse)
      case TraitType(_, args) => universeOfTypeArgs(args)
      case ClassType(_, args) => universeOfTypeArgs(args)
      case DataConstructorType(_, args, _) => universeOfTypeArgs(args)
      case TypeOfType(inner) => universeOfWrapExpr(inner).map(NextLargestUniverse)
      case TypeN(universe, _, _) => universe.pure[Comp]
      case FunctionType(argumentType, resultType) =>
        for {
          argUniv <- universeOfWrapExpr(argumentType)
          resUniv <- universeOfWrapExpr(resultType)
        } yield LargestUniverse(argUniv, resUniv)
      case UnionType(first, second) =>
        for {
          firstUniv <- universeOfWrapExpr(first)
          secondUniv <- universeOfWrapExpr(second)
        } yield LargestUniverse(firstUniv, secondUniv)

      case IntersectionType(first, second) =>
        for {
          firstUniv <- universeOfWrapExpr(first)
          secondUniv <- universeOfWrapExpr(second)
        } yield LargestUniverse(firstUniv, secondUniv)

      case ExistentialType(variable, inner) =>
        for {
          varUniv <- universeOfWrapExpr(variable.varType)
          innerUniv <- universeOfWrapExpr(inner)
        } yield LargestUniverse(varUniv, innerUniv)
    }
  }

  def universeSubsumes(larger: UniverseExpr, smaller: UniverseExpr): Comp[Boolean] = {
    def reduceUniverse(u: UniverseExpr, by: BigInt): UniverseExpr =
      u match {
        case FixedUniverse(u) =>
          if(u < by) FixedUniverse(0)
          else FixedUniverse(u - by)

        case AbstractUniverse() => AbstractUniverse()
        case LargestUniverse(a, b) => LargestUniverse(reduceUniverse(a, by), reduceUniverse(b, by))
        case NextLargestUniverse(a) =>
          if(by > 1)
            reduceUniverse(a, by - 1)
          else
            a

        case PreviousUniverse(a) =>
          reduceUniverse(a, by + 1)
      }

    (larger, smaller) match {
      case (_, AbstractUniverse()) |
           (AbstractUniverse(), _) =>
        false.pure[Comp]

      case (FixedUniverse(larger), FixedUniverse(smaller)) => (larger >= smaller).pure[Comp]

      case (LargestUniverse(a, b), _) =>
        universeSubsumes(a, smaller).flatMap {
          case true => true.pure[Comp]
          case false => universeSubsumes(b, smaller)
        }

      case (_, LargestUniverse(a, b)) =>
        universeSubsumes(larger, a).flatMap {
          case false => false.pure[Comp]
          case true => universeSubsumes(larger, b)
        }

      case (FixedUniverse(a), NextLargestUniverse(smaller)) =>
        if(a > 0)
          universeSubsumes(FixedUniverse(a - 1), smaller)
        else
          false.pure[Comp]

      case (_, PreviousUniverse(smaller)) =>
        universeSubsumes(larger, reduceUniverse(smaller, by = 1))

      case (PreviousUniverse(larger), _) =>
        universeSubsumes(reduceUniverse(larger, by = 1), smaller)

      case (NextLargestUniverse(larger), FixedUniverse(b)) =>
        if(b > 0)
          universeSubsumes(larger, FixedUniverse(b - 1))
        else
          true.pure[Comp]


      case (NextLargestUniverse(larger), NextLargestUniverse(smaller)) => universeSubsumes(larger, smaller)

    }
  }

}

object TypeSystem {
  type Aux[TContext <: Context with Singleton] = TypeSystem { val context: TContext }
  type Aux2[TContext <: Context with Singleton, Wrap[+_]] = TypeSystem { val context: TContext; type TTypeWrapper[+A] = Wrap[A] }
}
