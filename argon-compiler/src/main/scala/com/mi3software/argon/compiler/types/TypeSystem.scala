package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import scalaz._
import Scalaz._
import Compilation.Operators._

import scala.collection.immutable.Vector

trait TypeSystem[TContext <: Context with Singleton] {

  val context: TContext

  type TTypeWrapper[+_]

  type TType = TTypeWrapper[SimpleType]
  type WrapExpr = TTypeWrapper[ArExpr]
  
  type WrapRef[T[_ <: Context, _[_, _]]] = AbsRef[context.type, T]
  type TSubTypeInfo = SubTypeInfo[TType]


  final def fromSimpleType(simpleType: SimpleType): TType = wrapType(simpleType)

  def wrapType[A](a: A): TTypeWrapper[A]
  def mapTypeWrapper[A, B](t: TTypeWrapper[A])(f: A => B): TTypeWrapper[B]
  def traverseTypeWrapper[A, B, F[_] : Applicative](t: TTypeWrapper[A])(f: A => F[B]): F[TTypeWrapper[B]]

  def wrapExprType(expr: WrapExpr): TType

  def isSubTypeWrapper[TComp[_] : Compilation](a: TType, b: TType): TComp[Option[SubTypeInfo[TType]]]

  def universeOfExpr(expr: WrapExpr): Universe
  def universeOfType(t: TType): TypeUniverse

  final def largestUniverse[U <: Universe](default: U)(universes: Vector[U]): U =
    universes.foldLeft(default)(Universe.union)

  final def largestUniverse1[U <: Universe](universes: NonEmptyList[U]): U =
    universes.foldLeft1(Universe.union)

  final def isSubType[TComp[_] : Compilation](a: TType, b: TType): TComp[Option[TSubTypeInfo]] =
    isSubTypeWrapper(a, b)

  final def exprIsType(expr: WrapExpr): Option[TType] =
    traverseTypeWrapper(expr) {
      case t: SimpleType => Some(t)
      case _ => None
    }


  final case class Variable[+Desc <: VariableLikeDescriptor](descriptor: Desc, name: VariableName, mutability: Mutability, varType: TType)

  final case class Parameter(tupleVars: Vector[Variable[DeconstructedParameterDescriptor]], paramType: TType)


  trait ArExpr {
    val exprType: TType
    val universe: Universe
  }

  final case class ClassConstructorCall(classType: ClassType, classCtor: AbsRef[context.type, ClassConstructor], args: Vector[TType]) extends ArExpr {
    override val exprType: TType = fromSimpleType(classType)
    override val universe: Universe = ValueUniverse
  }
  final case class DataConstructorCall(dataCtorInstanceType: DataConstructorType, args: Vector[ArExpr]) extends ArExpr {
    override val exprType: TType = fromSimpleType(dataCtorInstanceType)
    override val universe: Universe = ValueUniverse
  }
  final case class FunctionCall(function: AbsRef[context.type, ArFunc], args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
    override lazy val universe: Universe = universeOfType(returnType).prev
  }
  final case class IfElse(condition: ArExpr, ifBody: ArExpr, elseBody: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(UnionType(ifBody.exprType, elseBody.exprType))
    override lazy val universe: Universe = Universe.union(ifBody.universe, elseBody.universe)
  }
  final case class LetBinding(variable: Variable[VariableDescriptor], value: ArExpr, next: ArExpr) extends ArExpr {
    override lazy val exprType: TType = next.exprType
    override lazy val universe: Universe = next.universe
  }
  final case class LoadConstantBool(value: Boolean, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadConstantInt(value: BigInt, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadConstantString(value: String, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadLambda(argVariable: Variable[VariableDescriptor], body: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(FunctionType(argVariable.varType, body.exprType))
    override lazy val universe: Universe = Universe.union(
      universeOfType(argVariable.varType).prev,
      universeOfType(body.exprType).prev
    )
  }
  final case class LoadUnit(exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }
  final case class LoadVariable(variable: Variable[VariableLikeDescriptor]) extends ArExpr {
    override val exprType: TType = variable.varType
    override lazy val universe: Universe = universeOfType(variable.varType).prev
  }
  final case class MethodCall(method: AbsRef[context.type, ArMethod], instance: ArExpr, args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
    override lazy val universe: Universe = universeOfType(returnType).prev
  }
  final case class Sequence(first: ArExpr, second: ArExpr) extends ArExpr {
    override lazy val exprType: TType = second.exprType
    override lazy val universe: Universe = second.universe
  }
  final case class StoreVariable(variable: Variable[VariableLikeDescriptor], value: ArExpr, exprType: TType) extends ArExpr {
    override val universe: Universe = ValueUniverse
  }


  sealed trait SimpleType extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(TypeOfType(TypeUniverse(universe)))
    override val universe: TypeUniverse
  }

  final case class TypeOfType(universe: TypeUniverse) extends SimpleType
  final case class TraitType(arTrait: WrapRef[ArTrait], args: Vector[TType], baseTypes: BaseTypeInfoTrait) extends SimpleType {
    override val universe: TypeUniverse = TypeUniverse(ValueUniverse)
  }
  final case class ClassType(arClass: WrapRef[ArClass], args: Vector[TType], baseTypes: BaseTypeInfoClass) extends SimpleType {
    override val universe: TypeUniverse = TypeUniverse(ValueUniverse)
  }
  final case class DataConstructorType(ctor: WrapRef[DataConstructor], args: Vector[TType], instanceType: TraitType) extends SimpleType {
    override val universe: TypeUniverse = TypeUniverse(ValueUniverse)
  }

  final case class MetaType(innerType: TType, baseType: TType) extends SimpleType {
    override lazy val universe: TypeUniverse = ???
  }

  final case class TupleElement[+A <: ArExpr](value: TTypeWrapper[A]) {
    lazy val elementTypeElement: TupleElement[SimpleType] = TupleElement[SimpleType](wrapExprType(value))
  }

  sealed trait LoadTuple extends ArExpr {
    val values: NonEmptyList[TupleElement[ArExpr]]

    override lazy val exprType: TType = fromSimpleType(LoadTupleType(values.map { _.elementTypeElement }))
    override lazy val universe: Universe = largestUniverse1(values.map { elem => universeOfExpr(elem.value) })
  }

  object LoadTuple {
    def apply(elems: NonEmptyList[TupleElement[ArExpr]]): LoadTuple =
      elems.traverse {
        case TupleElement(value) => exprIsType(value).map(TupleElement.apply)
      } match {
        case Some(typeElems) => LoadTupleType(typeElems)
        case None => new LoadTuple {
          override val values: NonEmptyList[TupleElement[ArExpr]] = elems
        }
      }
  }

  sealed trait LoadTupleType extends LoadTuple with SimpleType {
    val typeValues: NonEmptyList[TupleElement[SimpleType]]
    override lazy val universe: TypeUniverse = largestUniverse1(typeValues.map { elem => universeOfType(elem.value) })
  }

  object LoadTupleType {
    def apply(elems: NonEmptyList[TupleElement[SimpleType]]): LoadTupleType =
      new LoadTupleType {
        override val typeValues: NonEmptyList[TupleElement[SimpleType]] = elems
        override val values: NonEmptyList[TupleElement[ArExpr]] = elems.map(identity)
      }
  }

  final case class FunctionType(argumentType: TType, resultType: TType) extends SimpleType {
    override val universe: TypeUniverse = Universe.union(
      universeOfType(argumentType),
      universeOfType(resultType)
    )
  }
  final case class UnionType(first: TType, second: TType) extends SimpleType {
    override val universe: TypeUniverse = Universe.union(
      universeOfType(first),
      universeOfType(second)
    )
  }
  final case class IntersectionType(first: TType, second: TType) extends SimpleType {
    override val universe: TypeUniverse = Universe.union(
      universeOfType(first),
      universeOfType(second)
    )
  }


  final case class BaseTypeInfoTrait(baseTraits: Vector[TraitType])
  final case class BaseTypeInfoClass(baseClass: Option[ClassType], baseTraits: Vector[TraitType])



  protected final def isSimpleSubType[F[_] : Compilation](a: SimpleType, b: SimpleType): F[Option[TSubTypeInfo]] = {

    val notSubType = Option.empty[TSubTypeInfo].point[F]

    def invariant(a: TType, b: TType): F[Option[Vector[TSubTypeInfo]]] =
      isSubType[F](a, b).flatMap {
        case Some(left) =>
          isSubType[F](b, a).map { _.map {
            right => Vector(left, right)
          } }

        case None => Option.empty[Vector[TSubTypeInfo]].point[F]
      }


    def compareArguments(aType: TType, bType: TType)(a: Vector[TType])(b: Vector[TType]): F[Option[TSubTypeInfo]] =
      if(a.size == b.size)
        a.zip(b)
          .traverse { case (aArg, bArg) => OptionT(invariant(aArg, bArg)) }
          .map { args => SubTypeInfo(aType, bType, args.flatten) }
          .run
      else
        notSubType

    def isSubTrait(a: TraitType)(b: TraitType): F[Option[TSubTypeInfo]] =
      if(a.arTrait.value.descriptor === b.arTrait.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        b.baseTypes.baseTraits.findMapM(isSubTrait(a))

    def isSubClass(a: ClassType)(b: ClassType): F[Option[TSubTypeInfo]] =
      if(a.arClass.value.descriptor === b.arClass.value.descriptor)
        compareArguments(fromSimpleType(a), fromSimpleType(b))(a.args)(b.args)
      else
        b.baseTypes.baseClass.findMapM(isSubClass(a))

    def classImplementsTrait(a: TraitType)(b: ClassType): F[Option[TSubTypeInfo]] =
      Vector(
        () => b.baseTypes.baseTraits.findMapM(isSubTrait(a)),
        () => b.baseTypes.baseClass.findMapM(classImplementsTrait(a)),
      ).findMapM(_())

    Vector(
      () => a match {
        case a: UnionType =>
          isSubType[F](a.first, fromSimpleType(b)).flatMap {
            case Some(left) =>
              isSubType[F](a.second, fromSimpleType(b)).map { _.map { right =>
                SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(left, right))
              } }

            case None => notSubType
          }
        case _ => notSubType
      },
      () => b match {
        case b: IntersectionType =>
          isSubType[F](fromSimpleType(a), b.first).flatMap {
            case Some(left) =>
              isSubType[F](fromSimpleType(a), b.second).map { _.map { right =>
                SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(left, right))
              } }

            case None => notSubType
          }

        case _ => notSubType
      },
      () => b match {
        case b: UnionType =>
          Vector(
            () => isSubType[F](fromSimpleType(a), b.first),
            () => isSubType[F](fromSimpleType(a), b.second),
          )
            .findMapM(_())
            .map { _.map { info => SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(info)) } }

        case _ => notSubType
      },
      () => a match {
        case a: IntersectionType =>
          Vector(
            () => isSubType[F](a.first, fromSimpleType(b)),
            () => isSubType[F](a.second, fromSimpleType(b)),
          )
            .findMapM(_())
            .map { _.map { info => SubTypeInfo(fromSimpleType(a), fromSimpleType(b), Vector(info)) } }
        case _ => notSubType
      },
      () => (a, b) match {

        case (aTrait: TraitType, bTrait: TraitType) => isSubTrait(aTrait)(bTrait)
        case (aClass: ClassType, bClass: ClassType) => isSubClass(aClass)(bClass)
        case (aTrait: TraitType, bClass: ClassType) => classImplementsTrait(aTrait)(bClass)
        case (_: ClassType, _: TraitType) => notSubType

        case (_, _) => notSubType
      },
    ).findMapM(_())
  }

}

object TypeSystem {

  def convertTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (t1: ts.TType)
  : F[otherTS.TType] =
    ts.traverseTypeWrapper(t1)(convertSimpleTypeSystem(context)(ts)(otherTS)(converter)(_))
      .flatMap(converter.convertType(ts)(otherTS)(identity)(_))

  def convertTraitType[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (traitType: ts.TraitType)
  : F[otherTS.TraitType] = for {
    newArgs <- traitType.args.traverse(convertTypeSystem(context)(ts)(otherTS)(converter)(_))
    newBaseTraits <- traitType.baseTypes.baseTraits.traverse(convertTraitType(context)(ts)(otherTS)(converter)(_))
  } yield otherTS.TraitType(
    traitType.arTrait,
    newArgs,
    otherTS.BaseTypeInfoTrait(newBaseTraits)
  )

  def convertClassType[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (classType: ts.ClassType)
  : F[otherTS.ClassType] = for {
    newArgs <- classType.args.traverse(convertTypeSystem(context)(ts)(otherTS)(converter)(_))
    newBaseClass <- classType.baseTypes.baseClass.traverse(convertClassType(context)(ts)(otherTS)(converter)(_))
    newBaseTraits <- classType.baseTypes.baseTraits.traverse(convertTraitType(context)(ts)(otherTS)(converter)(_))
  } yield otherTS.ClassType(
    classType.arClass,
    newArgs,
    otherTS.BaseTypeInfoClass(
      newBaseClass,
      newBaseTraits
    )
  )


  def convertDataConstructorType[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (dataCtorType: ts.DataConstructorType)
  : F[otherTS.DataConstructorType] = for {
    newArgs <- dataCtorType.args.traverse(convertTypeSystem(context)(ts)(otherTS)(converter)(_))
    newInstanceType <- convertTraitType(context)(ts)(otherTS)(converter)(dataCtorType.instanceType)
  } yield otherTS.DataConstructorType(
    dataCtorType.ctor,
    newArgs,
    newInstanceType
  )

  final def convertSimpleTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (t1: ts.SimpleType)
  : F[otherTS.SimpleType] = t1 match {
    case t1: ts.TraitType => convertTraitType(context)(ts)(otherTS)(converter)(t1).map(identity)
    case t1: ts.ClassType => convertClassType(context)(ts)(otherTS)(converter)(t1).map(identity)
    case t1: ts.DataConstructorType => convertDataConstructorType(context)(ts)(otherTS)(converter)(t1).map(identity)

    case ts.TypeOfType(universe) =>
      (otherTS.TypeOfType(universe) : otherTS.SimpleType).point[F]

    case ts.MetaType(innerType, baseType) =>
      for {
        newInnerType <- convertTypeSystem(context)(ts)(otherTS)(converter)(innerType)
        newBaseType <- convertTypeSystem(context)(ts)(otherTS)(converter)(baseType)
      } yield otherTS.MetaType(newInnerType, newBaseType)

    case t1: ts.LoadTupleType =>
      t1.typeValues
        .traverse { case ts.TupleElement(elementType) =>
          convertTypeSystem(context)(ts)(otherTS)(converter)(elementType).map(otherTS.TupleElement(_))
        }
        .map(otherTS.LoadTupleType(_))

    case ts.FunctionType(argumentType, resultType) =>
      for {
        newArgType <- convertTypeSystem(context)(ts)(otherTS)(converter)(argumentType)
        newResultType <- convertTypeSystem(context)(ts)(otherTS)(converter)(resultType)
      } yield otherTS.FunctionType(newArgType, newResultType)

    case ts.UnionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(context)(ts)(otherTS)(converter)(first)
        newSecond <- convertTypeSystem(context)(ts)(otherTS)(converter)(second)
      } yield otherTS.UnionType(newFirst, newSecond)

    case ts.IntersectionType(first, second) =>
      for {
        newFirst <- convertTypeSystem(context)(ts)(otherTS)(converter)(first)
        newSecond <- convertTypeSystem(context)(ts)(otherTS)(converter)(second)
      } yield otherTS.IntersectionType(newFirst, newSecond)

  }

  final def convertVariableTypeSystem[F[_]: Monad, Desc <: VariableLikeDescriptor]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (v: ts.Variable[Desc])
  : F[otherTS.Variable[Desc]] =
    for {
      newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(v.varType)
    } yield otherTS.Variable(
      v.descriptor,
      v.name,
      v.mutability,
      newType
    )

  final def convertParameterTypeSystem[F[_]: Monad, Desc <: VariableLikeDescriptor]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (p: ts.Parameter)
  : F[otherTS.Parameter] =
    for {
      newVars <- p.tupleVars.traverse(convertVariableTypeSystem(context)(ts)(otherTS)(converter)(_))
      newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(p.paramType)
    } yield otherTS.Parameter(newVars, newType)

  def convertExprTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (expr: ts.ArExpr)
  : F[otherTS.ArExpr] = expr match {
    case t: ts.SimpleType =>
      convertSimpleTypeSystem(context)(ts)(otherTS)(converter)(t).map(identity)

    case ts.ClassConstructorCall(classType, classCtor, args) =>
      for {
        newClassType <- convertClassType(context)(ts)(otherTS)(converter)(classType)
        newArgs <- args.traverse(convertTypeSystem(context)(ts)(otherTS)(converter)(_))
      } yield otherTS.ClassConstructorCall(newClassType, classCtor, newArgs)

    case expr: ts.LoadTuple =>
      expr.values
        .traverse { case ts.TupleElement(elementType) =>
          convertWrapExprTypeSystem(context)(ts)(otherTS)(converter)(elementType).map(otherTS.TupleElement(_))
        }
        .map(otherTS.LoadTuple(_))

    case ts.DataConstructorCall(dataCtor, args) =>
      for {
        newType <- convertDataConstructorType(context)(ts)(otherTS)(converter)(dataCtor)
        newArgs <- args.traverse(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
      } yield otherTS.DataConstructorCall(newType, newArgs)

    case ts.FunctionCall(function, args, returnType) =>
      for {
        newArgs <- args.traverse(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
        newReturnType <- convertTypeSystem(context)(ts)(otherTS)(converter)(returnType)
      } yield otherTS.FunctionCall(function, newArgs, newReturnType)

    case ts.IfElse(condition, ifBody, elseBody) =>
      for {
        newCondition <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(condition)
        newIfBody <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(ifBody)
        newElseBody <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(elseBody)
      } yield otherTS.IfElse(newCondition, newIfBody, newElseBody)

    case ts.LetBinding(variable, value, next) =>
      for {
        newVar <- convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
        newValue <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(value)
        newNext <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(next)
      } yield otherTS.LetBinding(newVar, newValue, newNext)

    case ts.LoadConstantBool(value, exprType) =>
      for {
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.LoadConstantBool(value, newType)

    case ts.LoadConstantInt(value, exprType) =>
      for {
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.LoadConstantInt(value, newType)

    case ts.LoadConstantString(value, exprType) =>
      for {
        newType <- convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      } yield otherTS.LoadConstantString(value, newType)

    case ts.LoadLambda(variable, body) =>
      for {
        newVar <- convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
        newBody <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(body)
      } yield otherTS.LoadLambda(newVar, newBody)

    case ts.LoadVariable(variable) =>
      for {
        newVar <- convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
      } yield otherTS.LoadVariable(newVar)

    case ts.MethodCall(method, instance, args, returnType) =>
      for {
        newInstance <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(instance)
        newArgs <- args.traverse(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
        newReturnType <- convertTypeSystem(context)(ts)(otherTS)(converter)(returnType)
      } yield otherTS.MethodCall(method, newInstance, newArgs, newReturnType)

    case ts.Sequence(first, second) =>
      for {
        newFirst <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(first)
        newSecond <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(second)
      } yield otherTS.Sequence(newFirst, newSecond)

    case ts.StoreVariable(variable, value, unitType) =>
      for {
        newVar <- convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
        newValue <- convertExprTypeSystem(context)(ts)(otherTS)(converter)(value)
        newUnitType <- convertTypeSystem(context)(ts)(otherTS)(converter)(unitType)
      } yield otherTS.StoreVariable(newVar, newValue, newUnitType)

  }


  def convertWrapExprTypeSystem[F[_]: Monad]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type, F])
  (expr: ts.WrapExpr)
  : F[otherTS.WrapExpr] =
    ts.traverseTypeWrapper(expr)(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
      .flatMap(converter.convertType[otherTS.ArExpr](ts)(otherTS)(t => t)(_))



}
