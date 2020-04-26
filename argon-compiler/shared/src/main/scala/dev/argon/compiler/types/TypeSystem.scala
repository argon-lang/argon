package dev.argon.compiler.types

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.AnyExtensions._
import cats._
import cats.implicits._
import cats.data._
import cats.evidence.===
import dev.argon.compiler.core.ErasedSignature.TupleType
import dev.argon.compiler.types.TypeSystem.PrimitiveOperation
import shapeless.Nat
import zio.interop.catz._

import scala.collection.immutable.Vector

trait TypeSystem[TContext <: Context with Singleton] {

  val context: TContext
  val contextProof: TContext === context.type

  type TTypeWrapper[+_]

  type WrapExpr = TTypeWrapper[ArExpr]
  type TType = WrapExpr
  
  type WrapRef[T[_ <: Context with Singleton, _[_, _]]] = AbsRef[context.type, T]
  type TSubTypeInfo = SubTypeInfo[TType]


  def liftSignatureResult[TResult[TContext2 <: Context with Singleton, _ <: TypeSystem[TContext2] with Singleton]](sig: context.signatureContext.Signature[TResult, _ <: Nat], args: Vector[TypeArgument]): Comp[TResult[TContext, this.type]]


  final def fromSimpleType(simpleType: ArExpr): TType = wrapType(simpleType)

  def wrapType[A](a: A): TTypeWrapper[A]
  def unwrapType[A](t: TTypeWrapper[A]): Option[A]
  def mapTypeWrapper[A, B](t: TTypeWrapper[A])(f: A => B): TTypeWrapper[B]
  def flatMapTypeWrapper[A, B](t: TTypeWrapper[A])(f: A => TTypeWrapper[B]): TTypeWrapper[B]
  def traverseTypeWrapper[A, B, F[_] : Applicative](t: TTypeWrapper[A])(f: A => F[B]): F[TTypeWrapper[B]]
  def flatTraverseTypeWrapper[A, B, F[_] : Applicative](t: TTypeWrapper[A])(f: A => F[TTypeWrapper[B]]): F[TTypeWrapper[B]] =
    traverseTypeWrapper(t)(f).map(flatMapTypeWrapper(_)(identity))

  def wrapExprType(expr: WrapExpr): Comp[TType]

  def isSubTypeWrapper(a: TType, b: TType): Comp[Option[SubTypeInfo[TType]]]

  def universeOfWrapExpr(expr: WrapExpr): Comp[UniverseExpr]

  final def isSubType(a: TType, b: TType): Comp[Option[TSubTypeInfo]] = for {
    a2 <- flatTraverseTypeWrapper(a)(reduceExprToValue)
    b2 <- flatTraverseTypeWrapper(b)(reduceExprToValue)
    res <- isSubTypeWrapper(a2, b2)
  } yield res




  sealed trait Variable {
    val descriptor: VariableLikeDescriptor
    val name: VariableName
    val mutability: Mutability
    val varType: TType
  }

  final case class LocalVariable(descriptor: VariableDescriptor, name: VariableName, mutability: Mutability, varType: TType) extends Variable
  final case class ParameterVariable(descriptor: ParameterDescriptor, name: VariableName, mutability: Mutability, varType: TType) extends Variable
  final case class FieldVariable(descriptor: FieldDescriptor, ownerClass: AbsRef[context.type, ArClass], name: VariableName.Normal, mutability: Mutability, varType: TType) extends Variable

  final case class ParameterElement(paramVar: ParameterVariable, name: VariableName, elemType: TType, index: Int)
  final case class Parameter(style: ParameterStyle, paramVar: ParameterVariable, elements: Vector[ParameterElement]) {
    def paramType: TType = paramVar.varType
  }


  sealed trait ArExpr

  final case class ClassConstructorCall(classType: ClassType, classCtor: AbsRef[context.type, ClassConstructor], args: Vector[WrapExpr]) extends ArExpr
  final case class DataConstructorCall(dataCtorInstanceType: DataConstructorType, args: Vector[WrapExpr]) extends ArExpr
  final case class EnsureExecuted(body: WrapExpr, ensuring: WrapExpr) extends ArExpr
  final case class FunctionCall(function: AbsRef[context.type, ArFunc], args: Vector[WrapExpr], returnType: TType) extends ArExpr
  final case class FunctionObjectCall(function: WrapExpr, arg: WrapExpr, returnType: TType) extends ArExpr
  final case class IfElse(condition: WrapExpr, ifBody: WrapExpr, elseBody: WrapExpr) extends ArExpr
  final case class LetBinding(variable: LocalVariable, value: WrapExpr, next: WrapExpr) extends ArExpr
  final case class LoadConstantBool(value: Boolean, exprType: TType) extends ArExpr
  final case class LoadConstantInt(value: BigInt, exprType: TType) extends ArExpr
  final case class LoadConstantString(value: String, exprType: TType) extends ArExpr
  final case class LoadLambda(argVariable: LocalVariable, body: WrapExpr) extends ArExpr
  final case class TupleElement(value: WrapExpr)
  final case class LoadTuple(values: NonEmptyList[TupleElement]) extends ArExpr
  final case class LoadTupleElement(tupleValue: WrapExpr, elemType: TType, index: Int) extends ArExpr
  final case class LoadUnit(exprType: TType) extends ArExpr
  final case class LoadVariable(variable: Variable) extends ArExpr
  final case class MethodCall(method: AbsRef[context.type, ArMethod], instance: WrapExpr, args: Vector[WrapExpr], returnType: TType) extends ArExpr


  sealed trait PatternExpr
  object PatternExpr {
    final case class DataDeconstructor(ctor: AbsRef[context.type, DataConstructor], args: Vector[PatternExpr]) extends PatternExpr
    final case class Binding(variable: LocalVariable) extends PatternExpr
    final case class CastBinding(variable: LocalVariable) extends PatternExpr
  }

  final case class PatternCase(pattern: PatternExpr, body: WrapExpr)

  final case class PatternMatch(expr: WrapExpr, cases: NonEmptyList[PatternCase]) extends ArExpr

  final case class PrimitiveOp(operation: PrimitiveOperation, left: WrapExpr, right: WrapExpr, exprType: TType) extends ArExpr
  final case class Sequence(first: WrapExpr, second: WrapExpr) extends ArExpr
  final case class StoreVariable(variable: Variable, value: WrapExpr, exprType: TType) extends ArExpr

  sealed trait TypeIsTypeOfTypeExpr extends ArExpr

  sealed trait TypeWithMethods extends TypeIsTypeOfTypeExpr

  sealed trait TypeArgument
  object TypeArgument {
    final case class Expr(expr: WrapExpr) extends TypeArgument
    final case class Wildcard(universe: UniverseExpr) extends TypeArgument
  }

  final case class TypeOfType(inner: TType) extends TypeIsTypeOfTypeExpr
  final case class TypeN(universe: UniverseExpr, subtypeConstraint: Option[TType], supertypeConstraint: Option[TType]) extends TypeIsTypeOfTypeExpr

  final case class TraitType(arTrait: WrapRef[ArTrait], args: Vector[TypeArgument]) extends TypeWithMethods
  final case class ClassType(arClass: WrapRef[ArClass], args: Vector[TypeArgument]) extends TypeWithMethods
  final case class DataConstructorType(ctor: WrapRef[DataConstructor], args: Vector[TypeArgument], instanceType: TraitType) extends TypeWithMethods

  final case class FunctionType(argumentType: TType, resultType: TType) extends TypeIsTypeOfTypeExpr
  final case class UnionType(first: TType, second: TType) extends TypeIsTypeOfTypeExpr
  final case class IntersectionType(first: TType, second: TType) extends TypeIsTypeOfTypeExpr


  final case class BaseTypeInfoTrait(baseTraits: Vector[TraitType])
  final case class BaseTypeInfoClass(baseClass: Option[ClassType], baseTraits: Vector[TraitType])


  sealed trait ClassConstructorStatement
  final case class ClassConstructorStatementExpr(expr: ArExpr) extends ClassConstructorStatement
  final case class InitializeFieldStatement(field: FieldVariable, value: ArExpr) extends ClassConstructorStatement

  final case class ClassConstructorBody
  (
    initStatements: Vector[ClassConstructorStatement],
    baseConstructorCall: Option[ClassConstructorCall],
    endExpr: ArExpr,
  )


  sealed trait UniverseExpr
  final case class FixedUniverse(u: BigInt) extends UniverseExpr
  final case class AbstractUniverse() extends UniverseExpr
  final case class LargestUniverse(a: UniverseExpr, b: UniverseExpr) extends UniverseExpr
  final case class NextLargestUniverse(a: UniverseExpr) extends UniverseExpr
  final case class PreviousUniverse(a: UniverseExpr) extends UniverseExpr




  protected final def isSimpleSubType(a: ArExpr, b: ArExpr): Comp[Option[TSubTypeInfo]] = {

    val notSubType = Option.empty[TSubTypeInfo].pure[Comp]

    def compareTypeArg(a: TypeArgument, b: TypeArgument): Comp[Option[Vector[TSubTypeInfo]]] = {
      def fromIsSame(b: Boolean): Comp[Option[Vector[TSubTypeInfo]]] =
        if(b)
          Some(Vector.empty).upcast[Option[Vector[TSubTypeInfo]]].pure[Comp]
        else
          Option.empty[Vector[TSubTypeInfo]].pure[Comp]

      (a, b) match {
        case (TypeArgument.Wildcard(ua), TypeArgument.Wildcard(ub)) => ???
        case (TypeArgument.Wildcard(ua), _) => ???
        case (_, TypeArgument.Wildcard(_)) => fromIsSame(false)

        case (TypeArgument.Expr(a), TypeArgument.Expr(b)) =>
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
              def isSameLambda = aBody == Substitutions(this)(bVar, fromSimpleType(LoadVariable(aVar))).substWrapExpr(bBody)
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
    }

    def compareArguments(aType: TType, bType: TType)(a: Vector[TypeArgument])(b: Vector[TypeArgument]): Comp[Option[TSubTypeInfo]] =
      if(a.size === b.size)
        a.zip(b)
          .traverse { case (aArg, bArg) => OptionT(compareTypeArg(aArg, bArg)) }
          .map { args => SubTypeInfo(aType, bType, args.flatten) }
          .value
      else
        notSubType

    def isSubTrait(a: TraitType)(b: TraitType): Comp[Option[TSubTypeInfo]] =
      if(a.arTrait.value.descriptor === b.arTrait.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        for {
          sig <- b.arTrait.value.signature
          resultInfo <- liftSignatureResult(sig, b.args)
          baseTypes <- resultInfo.baseTypes
          result <- baseTypes.baseTraits.collectFirstSomeM(isSubTrait(a))
        } yield result

    def isSubClass(a: ClassType)(b: ClassType): Comp[Option[TSubTypeInfo]] =
      if(a.arClass.value.descriptor === b.arClass.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        for {
          sig <- b.arClass.value.signature
          resultInfo <- liftSignatureResult(sig, b.args)
          baseTypes <- resultInfo.baseTypes
          result <- baseTypes.baseClass.collectFirstSomeM(isSubClass(a))
        } yield result

    def classImplementsTrait(a: TraitType)(b: ClassType): Comp[Option[TSubTypeInfo]] = for {
      sig <- b.arClass.value.signature
      resultInfo <- liftSignatureResult(sig, b.args)
      baseTypes <- resultInfo.baseTypes
      result <-
        Vector(
          () => baseTypes.baseTraits.collectFirstSomeM(isSubTrait(a)),
          () => baseTypes.baseClass.collectFirstSomeM(classImplementsTrait(a)),
        ).collectFirstSomeM(_())

    } yield result

    def isSameDataCtor(a: DataConstructorType)(b: DataConstructorType): Comp[Option[TSubTypeInfo]] =
      if(a.ctor.value.descriptor === b.ctor.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        Option.empty[TSubTypeInfo].pure[Comp]

    def unMetaType(t: ArExpr): Vector[TType] =
      t match {
        case TypeOfType(inner) =>
          Vector(inner)

        case IntersectionType(b1, b2) =>
          unMetaTypeWrapped(b1) ++ unMetaTypeWrapped(b2)

        case LoadTuple(values) =>
          values
            .traverse {
              case TupleElement(value) =>
                unMetaTypeWrapped(value).map(TupleElement)
            }
            .map { newValues => fromSimpleType(LoadTuple(newValues)) }

        case _ => Vector()
      }

    def unMetaTypeWrapped(t: TType): Vector[TType] =
      unwrapType(t).toList.toVector.flatMap(unMetaType)

    Vector(
      () => a match {
        case a: IntersectionType =>
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
        case b: UnionType =>
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
        case b: IntersectionType =>
          Vector(
            () => isSubType(fromSimpleType(a), b.first),
            () => isSubType(fromSimpleType(a), b.second),
          )
            .collectFirstSomeM(_())
            .map { _.map { info => SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(info)) } }

        case _ => notSubType
      },
      () => a match {
        case a: UnionType =>
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

        case (aTrait: TraitType, bTrait: TraitType) => isSubTrait(aTrait)(bTrait)
        case (aClass: ClassType, bClass: ClassType) => isSubClass(aClass)(bClass)
        case (aTrait: TraitType, bClass: ClassType) => classImplementsTrait(aTrait)(bClass)
        case (_: ClassType, _: TraitType) => notSubType

        case (aDataCtor: DataConstructorType, bDataCtor: DataConstructorType) =>
          isSameDataCtor(aDataCtor)(bDataCtor)

        case (aTrait: TraitType, bDataCtor: DataConstructorType) =>
          isSubTrait(aTrait)(bDataCtor.instanceType)


        case (aTuple: LoadTuple, bTuple: LoadTuple) =>
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

        case (LoadVariable(varA), LoadVariable(varB)) if varA.descriptor === varB.descriptor =>
          SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector.empty).pure[Option].pure[Comp]

        case (FunctionObjectCall(functionA, argA, _), FunctionObjectCall(functionB, argB, _)) =>
          compareArguments(fromSimpleType(a), fromSimpleType(b))(
            Vector(TypeArgument.Expr(functionA), TypeArgument.Expr(argA))
          )(
            Vector(TypeArgument.Expr(functionB), TypeArgument.Expr(argB))
          )

        case (_, _) => notSubType
      },
      () => b match {
        case _: TypeIsTypeOfTypeExpr | _: LoadTuple => notSubType
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
        case _: TypeIsTypeOfTypeExpr | _: LoadTuple => notSubType
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


  final def reduceExprToValue(expr: ArExpr): Comp[WrapExpr] =
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
           _: TypeIsTypeOfTypeExpr =>
        fromSimpleType(expr).pure[Comp]


      // Potentially reducible after adding inline support
      case FunctionCall(_, _, _) => fromSimpleType(expr).pure[Comp]
      case MethodCall(_, _, _, _) => fromSimpleType(expr).pure[Comp]

      case FunctionObjectCall(function, arg, _) =>
        reduceWrapExprToValue(function).flatMap { func =>
          unwrapType(func) match {
            case Some(LoadLambda(argVariable, body)) =>
              for {
                argValue <- reduceWrapExprToValue(arg)
              } yield Substitutions(this)(argVariable, argValue).substWrapExpr(body)

            case _ => fromSimpleType(expr).pure[Comp]
          }
        }

      case EnsureExecuted(body, _) => reduceWrapExprToValue(body)
      case IfElse(_, _, _) => ???
      case PatternMatch(_, _) => ???

      case LetBinding(_, _, _) => ???
      case LoadTupleElement(_, _, _) => ???
      case PrimitiveOp(_, _, _, _) => ???
      case Sequence(_, second) => reduceWrapExprToValue(second)
      case StoreVariable(_, _, _) => ???
    }

  final def reduceWrapExprToValue(expr: WrapExpr): Comp[WrapExpr] =
    flatTraverseTypeWrapper(expr)(reduceExprToValue)

  final def getExprType(expr: ArExpr, includeExtraTypeOfType: Boolean = true): Comp[TType] = {
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
          .traverse { case TupleElement(elem) => wrapExprType(elem).map(TupleElement) }
          .map { elems => fromSimpleType(LoadTuple(elems)) }
      case LoadTupleElement(_, elemType, _) => elemType.pure[Comp]
      case LoadUnit(exprType) => exprType.pure[Comp]
      case LoadVariable(variable) => withTypeOfExpr(variable.varType.pure[Comp])
      case MethodCall(_, _, _, returnType) => withTypeOfExpr(returnType.pure[Comp])
      case PatternMatch(_, cases) =>
        cases
          .traverse { patCase => getWrapExprType(patCase.body) }
          .map { _.reduceLeft { (a, b) => fromSimpleType(UnionType(a, b)) } }
      case PrimitiveOp(_, _, _, exprType) => exprType.pure[Comp]
      case Sequence(_, second) => getWrapExprType(second)
      case StoreVariable(_, _, exprType) => exprType.pure[Comp]
      case expr: TypeIsTypeOfTypeExpr => fromSimpleType(TypeOfType(fromSimpleType(expr))).pure[Comp]
    }
  }

  final def getWrapExprType(expr: WrapExpr, includeExtraTypeOfType: Boolean = true): Comp[TType] =
    unwrapType(expr) match {
      case Some(expr) => getExprType(expr, includeExtraTypeOfType)
      case None => fromSimpleType(TypeOfType(expr)).pure[Comp]
    }

  def universeOfExpr(expr: ArExpr): Comp[UniverseExpr] = {
    def universeOfTypeArgs(args: Vector[TypeArgument]): Comp[UniverseExpr] =
      args
        .traverse {
          case TypeArgument.Expr(expr) => universeOfWrapExpr(expr)
          case TypeArgument.Wildcard(universe) => universe.pure[Comp]
        }
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
      case MethodCall(_, _, _, returnType) => universeOfWrapExpr(returnType).map(PreviousUniverse)
      case PatternMatch(_, cases) =>
        cases
          .traverse { patCase => universeOfWrapExpr(patCase.body) }
          .map { _.reduceLeft(LargestUniverse) }
      case PrimitiveOp(_, _, _, exprType) => universeOfWrapExpr(exprType).map(PreviousUniverse)
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

  sealed trait PrimitiveOperation
  object PrimitiveOperation {
    case object AddInt extends PrimitiveOperation
    case object SubInt extends PrimitiveOperation
    case object MulInt extends PrimitiveOperation
    case object IntEqual extends PrimitiveOperation
  }

}
