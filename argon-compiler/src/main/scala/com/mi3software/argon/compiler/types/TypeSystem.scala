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
  type TUniverse
  type TTypeUniverse <: TUniverse

  type TType = TTypeWrapper[SimpleType]
  type WrapExpr = TTypeWrapper[ArExpr]
  
  type WrapRef[T[_ <: Context, _[_, _]]] = AbsRef[context.type, T]



  final def fromSimpleType(simpleType: SimpleType): TType = wrapType(simpleType)

  def wrapType[A](a: A): TTypeWrapper[A]
  def mapTypeWrapper[A, B](t: TTypeWrapper[A])(f: A => B): TTypeWrapper[B]
  def traverseTypeWrapper[A, B, F[_] : Applicative](t: TTypeWrapper[A])(f: A => F[B]): F[TTypeWrapper[B]]

  def wrapExprType(expr: WrapExpr): TType

  def isSubTypeWrapper[TComp[_] : Compilation, T]
  (f: (T, T) => TComp[Boolean])
  (a: TTypeWrapper[T], b: TTypeWrapper[T])
  : TComp[Boolean]

  val valueUniverse: TUniverse
  def nextUniverse(universe: TUniverse): TTypeUniverse
  def previousUniverse(universe: TTypeUniverse): TUniverse
  def universeUnion[U <: TUniverse](a: U, b: U): U

  def universeOfExpr(expr: WrapExpr): TUniverse
  def universeOfType(t: TType): TTypeUniverse

  final def largestUniverse[U <: TUniverse](default: U)(universes: Vector[U]): U =
    universes.foldLeft(default)(universeUnion)

  final def largestUniverse1[U <: TUniverse](universes: NonEmptyList[U]): U =
    universes.foldLeft1(universeUnion)

  final def isSubType[TComp[_] : Compilation](a: TType, b: TType): TComp[Boolean] =
    isSubTypeWrapper(isSimpleSubType[TComp])(a, b)

  final def exprIsType(expr: WrapExpr): Option[TType] =
    traverseTypeWrapper(expr) {
      case t: SimpleType => Some(t)
      case _ => None
    }


  final case class Variable[+Desc <: VariableLikeDescriptor](descriptor: Desc, name: VariableName, mutability: Mutability, varType: TType)

  final case class Parameter(tupleVars: Vector[Variable[DeconstructedParameterDescriptor]], paramType: TType)


  trait ArExpr {
    val exprType: TType
    val universe: TUniverse
  }

  final case class ClassConstructorCall(classType: ClassType, classCtor: AbsRef[context.type, ClassConstructor], args: Vector[TType]) extends ArExpr {
    override val exprType: TType = fromSimpleType(classType)
    override val universe: TUniverse = valueUniverse
  }
  final case class DataConstructorCall(dataCtorInstanceType: DataConstructorType, args: Vector[ArExpr]) extends ArExpr {
    override val exprType: TType = fromSimpleType(dataCtorInstanceType)
    override val universe: TUniverse = valueUniverse
  }
  final case class FunctionCall(function: AbsRef[context.type, ArFunc], args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
    override lazy val universe: TUniverse = previousUniverse(universeOfType(returnType))
  }
  final case class IfElse(condition: ArExpr, ifBody: ArExpr, elseBody: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(UnionType(ifBody.exprType, elseBody.exprType))
    override lazy val universe: TUniverse = universeUnion(ifBody.universe, elseBody.universe)
  }
  final case class LetBinding(variable: Variable[VariableDescriptor], value: ArExpr, next: ArExpr) extends ArExpr {
    override lazy val exprType: TType = next.exprType
    override lazy val universe: TUniverse = next.universe
  }
  final case class LoadConstantBool(value: Boolean, exprType: TType) extends ArExpr {
    override val universe: TUniverse = valueUniverse
  }
  final case class LoadConstantInt(value: BigInt, exprType: TType) extends ArExpr {
    override val universe: TUniverse = valueUniverse
  }
  final case class LoadConstantString(value: String, exprType: TType) extends ArExpr {
    override val universe: TUniverse = valueUniverse
  }
  final case class LoadLambda(argVariable: Variable[VariableDescriptor], body: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(FunctionType(argVariable.varType, body.exprType))
    override lazy val universe: TUniverse = universeUnion(
      previousUniverse(universeOfType(argVariable.varType)),
      previousUniverse(universeOfType(body.exprType))
    )
  }
  final case class LoadUnit(exprType: TType) extends ArExpr {
    override val universe: TUniverse = valueUniverse
  }
  final case class LoadVariable(variable: Variable[VariableLikeDescriptor]) extends ArExpr {
    override val exprType: TType = variable.varType
    override lazy val universe: TUniverse = previousUniverse(universeOfType(variable.varType))
  }
  final case class MethodCall(method: AbsRef[context.type, ArMethod], instance: ArExpr, args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
    override lazy val universe: TUniverse = previousUniverse(universeOfType(returnType))
  }
  final case class Sequence(first: ArExpr, second: ArExpr) extends ArExpr {
    override lazy val exprType: TType = second.exprType
    override lazy val universe: TUniverse = second.universe
  }
  final case class StoreVariable(variable: Variable[VariableLikeDescriptor], value: ArExpr, exprType: TType) extends ArExpr {
    override val universe: TUniverse = valueUniverse
  }


  sealed trait SimpleType extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(TypeOfType(nextUniverse(universe)))
    override val universe: TTypeUniverse
  }

  final case class TypeOfType(universe: TTypeUniverse) extends SimpleType
  final case class TraitType(arTrait: WrapRef[ArTrait], args: Vector[TType], baseTypes: BaseTypeInfoTrait) extends SimpleType {
    override val universe: TTypeUniverse = nextUniverse(valueUniverse)
  }
  final case class ClassType(arClass: WrapRef[ArClass], args: Vector[TType], baseTypes: BaseTypeInfoClass) extends SimpleType {
    override val universe: TTypeUniverse = nextUniverse(valueUniverse)
  }
  final case class DataConstructorType(ctor: WrapRef[DataConstructor], args: Vector[TType], instanceType: TraitType) extends SimpleType {
    override val universe: TTypeUniverse = nextUniverse(valueUniverse)
  }

  final case class MetaType(innerType: TType, baseType: TType) extends SimpleType {
    override lazy val universe: TTypeUniverse = ???
  }

  final case class TupleElement[+A <: ArExpr](value: TTypeWrapper[A]) {
    lazy val elementTypeElement: TupleElement[SimpleType] = TupleElement[SimpleType](wrapExprType(value))
  }

  sealed trait LoadTuple extends ArExpr {
    val values: NonEmptyList[TupleElement[ArExpr]]

    override lazy val exprType: TType = fromSimpleType(LoadTupleType(values.map { _.elementTypeElement }))
    override lazy val universe: TUniverse = largestUniverse1(values.map { elem => universeOfExpr(elem.value) })
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
    override lazy val universe: TTypeUniverse = largestUniverse1(typeValues.map { elem => universeOfType(elem.value) })
  }

  object LoadTupleType {
    def apply(elems: NonEmptyList[TupleElement[SimpleType]]): LoadTupleType =
      new LoadTupleType {
        override val typeValues: NonEmptyList[TupleElement[SimpleType]] = elems
        override val values: NonEmptyList[TupleElement[ArExpr]] = elems.map(identity)
      }
  }

  final case class FunctionType(argumentType: TType, resultType: TType) extends SimpleType {
    override val universe: TTypeUniverse = universeUnion(
      universeOfType(argumentType),
      universeOfType(resultType)
    )
  }
  final case class UnionType(first: TType, second: TType) extends SimpleType {
    override val universe: TTypeUniverse = universeUnion(
      universeOfType(first),
      universeOfType(second)
    )
  }
  final case class IntersectionType(first: TType, second: TType) extends SimpleType {
    override val universe: TTypeUniverse = universeUnion(
      universeOfType(first),
      universeOfType(second)
    )
  }


  final case class BaseTypeInfoTrait(baseTraits: Vector[TraitType])
  final case class BaseTypeInfoClass(baseClass: Option[ClassType], baseTraits: Vector[TraitType])



  private def isSimpleSubType[F[_] : Compilation](a: SimpleType, b: SimpleType): F[Boolean] = {

    def compareArguments(a: Vector[TType])(b: Vector[TType]): F[Boolean] = true.pure[F]

    def isSubTrait(a: TraitType)(b: TraitType): F[Boolean] =
      if(a.arTrait.value.descriptor === b.arTrait.value.descriptor)
        compareArguments(a.args)(b.args)
      else
        b.baseTypes.baseTraits.anyM(isSubTrait(a))

    def isSubClass(a: ClassType)(b: ClassType): F[Boolean] =
      if(a.arClass.value.descriptor === b.arClass.value.descriptor)
        compareArguments(a.args)(b.args)
      else
        b.baseTypes.baseClass.anyM(isSubClass(a))

    def classImplementsTrait(a: TraitType)(b: ClassType): F[Boolean] =
      b.baseTypes.baseTraits.anyM(isSubTrait(a)) ||
        b.baseTypes.baseClass.anyM(classImplementsTrait(a))

    (a match {
      case a: UnionType =>
        isSubType[F](a.first, fromSimpleType(b)) && isSubType[F](a.second, fromSimpleType(b))
      case _ => false.pure[F]
    }) || (b match {
      case b: IntersectionType =>
        isSubType[F](fromSimpleType(a), b.first) && isSubType[F](fromSimpleType(a), b.second)
      case _ => false.pure[F]
    }) || (b match {
      case b: UnionType =>
        isSubType[F](fromSimpleType(a), b.first) || isSubType[F](fromSimpleType(a), b.second)
      case _ => false.pure[F]
    }) || (a match {
      case a: IntersectionType =>
        isSubType[F](a.first, fromSimpleType(b)) || isSubType[F](a.second, fromSimpleType(b))
      case _ => false.pure[F]
    }) || ((a, b) match {

      case (aTrait: TraitType, bTrait: TraitType) => isSubTrait(aTrait)(bTrait)
      case (aClass: ClassType, bClass: ClassType) => isSubClass(aClass)(bClass)
      case (aTrait: TraitType, bClass: ClassType) => classImplementsTrait(aTrait)(bClass)
      case (_: ClassType, _: TraitType) => false.pure[F]

      case (_, _) => false.pure[F]
    })



  }

}

object TypeSystem {

  def convertTypeSystem
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (t1: ts.TType)
  : otherTS.TType =
    converter.convertType(ts)(otherTS)(identity)(ts.mapTypeWrapper(t1)(convertSimpleTypeSystem(context)(ts)(otherTS)(converter)(_)))


  def convertTraitType
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (traitType: ts.TraitType): otherTS.TraitType =
    otherTS.TraitType(
      traitType.arTrait,
      traitType.args.map(convertTypeSystem(context)(ts)(otherTS)(converter)(_)),
      otherTS.BaseTypeInfoTrait(traitType.baseTypes.baseTraits.map(convertTraitType(context)(ts)(otherTS)(converter)(_)))
    )

  def convertClassType
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (classType: ts.ClassType): otherTS.ClassType =
    otherTS.ClassType(
      classType.arClass,
      classType.args.map(convertTypeSystem(context)(ts)(otherTS)(converter)(_)),
      otherTS.BaseTypeInfoClass(
        classType.baseTypes.baseClass.map(convertClassType(context)(ts)(otherTS)(converter)(_)),
        classType.baseTypes.baseTraits.map(convertTraitType(context)(ts)(otherTS)(converter)(_))
      )
    )


  def convertDataConstructorType
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (dataCtorType: ts.DataConstructorType): otherTS.DataConstructorType =
    otherTS.DataConstructorType(
      dataCtorType.ctor,
      dataCtorType.args.map(convertTypeSystem(context)(ts)(otherTS)(converter)(_)),
      convertTraitType(context)(ts)(otherTS)(converter)(dataCtorType.instanceType)
    )

  final def convertSimpleTypeSystem
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (t1: ts.SimpleType)
  : otherTS.SimpleType = t1 match {
    case t1: ts.TraitType => convertTraitType(context)(ts)(otherTS)(converter)(t1)
    case t1: ts.ClassType => convertClassType(context)(ts)(otherTS)(converter)(t1)
    case t1: ts.DataConstructorType => convertDataConstructorType(context)(ts)(otherTS)(converter)(t1)

    case ts.TypeOfType(universe) =>
      otherTS.TypeOfType(converter.convertTypeUniverse(ts)(otherTS)(universe))

    case ts.MetaType(innerType, baseType) =>
      otherTS.MetaType(
        convertTypeSystem(context)(ts)(otherTS)(converter)(innerType),
        convertTypeSystem(context)(ts)(otherTS)(converter)(innerType)
      )

    case t1: ts.LoadTupleType =>
      otherTS.LoadTupleType(t1.typeValues.map { case ts.TupleElement(elementType) =>
        otherTS.TupleElement(convertTypeSystem(context)(ts)(otherTS)(converter)(elementType))
      })

    case ts.FunctionType(argumentType, resultType) =>
      otherTS.FunctionType(
        convertTypeSystem(context)(ts)(otherTS)(converter)(argumentType),
        convertTypeSystem(context)(ts)(otherTS)(converter)(resultType)
      )

    case ts.UnionType(first, second) =>
      otherTS.UnionType(
        convertTypeSystem(context)(ts)(otherTS)(converter)(first),
        convertTypeSystem(context)(ts)(otherTS)(converter)(second)
      )

    case ts.IntersectionType(first, second) =>
      otherTS.IntersectionType(
        convertTypeSystem(context)(ts)(otherTS)(converter)(first),
        convertTypeSystem(context)(ts)(otherTS)(converter)(second)
      )

  }

  final def convertVariableTypeSystem[Desc <: VariableLikeDescriptor]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (v: ts.Variable[Desc])
  : otherTS.Variable[Desc] =
    otherTS.Variable(
      v.descriptor,
      v.name,
      v.mutability,
      convertTypeSystem(context)(ts)(otherTS)(converter)(v.varType)
    )

  final def convertParameterTypeSystem[Desc <: VariableLikeDescriptor]
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (p: ts.Parameter)
  : otherTS.Parameter =
    otherTS.Parameter(
      p.tupleVars.map(convertVariableTypeSystem(context)(ts)(otherTS)(converter)(_)),
      convertTypeSystem(context)(ts)(otherTS)(converter)(p.paramType)
    )

  def convertExprTypeSystem
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (expr: ts.ArExpr)
  : otherTS.ArExpr = expr match {
    case t: ts.SimpleType =>
      convertSimpleTypeSystem(context)(ts)(otherTS)(converter)(t)

    case ts.ClassConstructorCall(classType, classCtor, args) =>
      otherTS.ClassConstructorCall(
        convertClassType(context)(ts)(otherTS)(converter)(classType),
        classCtor,
        args.map(convertTypeSystem(context)(ts)(otherTS)(converter)(_))
      )

    case expr: ts.LoadTuple =>
      otherTS.LoadTuple(expr.values.map { case ts.TupleElement(elementType) =>
        otherTS.TupleElement(convertWrapExprTypeSystem(context)(ts)(otherTS)(converter)(elementType))
      })

    case ts.DataConstructorCall(dataCtor, args) =>
      otherTS.DataConstructorCall(
        convertDataConstructorType(context)(ts)(otherTS)(converter)(dataCtor),
        args.map(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_))
      )

    case ts.FunctionCall(function, args, returnType) =>
      otherTS.FunctionCall(
        function,
        args.map(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_)),
        convertTypeSystem(context)(ts)(otherTS)(converter)(returnType)
      )

    case ts.IfElse(condition, ifBody, elseBody) =>
      otherTS.IfElse(
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(condition),
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(ifBody),
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(elseBody)
      )

    case ts.LetBinding(variable, value, next) =>
      otherTS.LetBinding(
        convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable),
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(value),
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(next)
      )

    case ts.LoadConstantBool(value, exprType) =>
      otherTS.LoadConstantBool(
        value,
        convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      )

    case ts.LoadConstantInt(value, exprType) =>
      otherTS.LoadConstantInt(
        value,
        convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      )

    case ts.LoadConstantString(value, exprType) =>
      otherTS.LoadConstantString(
        value,
        convertTypeSystem(context)(ts)(otherTS)(converter)(exprType)
      )

    case ts.LoadLambda(variable, body) =>
      otherTS.LoadLambda(
        convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable),
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(body)
      )

    case ts.LoadVariable(variable) =>
      otherTS.LoadVariable(
        convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable)
      )

    case ts.MethodCall(method, instance, args, returnType) =>
      otherTS.MethodCall(
        method,
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(instance),
        args.map(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_)),
        convertTypeSystem(context)(ts)(otherTS)(converter)(returnType)
      )

    case ts.Sequence(first, second) =>
      otherTS.Sequence(
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(first),
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(second)
      )

    case ts.StoreVariable(variable, value, unitType) =>
      otherTS.StoreVariable(
        convertVariableTypeSystem(context)(ts)(otherTS)(converter)(variable),
        convertExprTypeSystem(context)(ts)(otherTS)(converter)(value),
        convertTypeSystem(context)(ts)(otherTS)(converter)(unitType)
      )

  }


  def convertWrapExprTypeSystem
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (expr: ts.WrapExpr)
  : otherTS.WrapExpr =
    converter.convertType[otherTS.ArExpr](ts)(otherTS)(t => t)(ts.mapTypeWrapper(expr)(convertExprTypeSystem(context)(ts)(otherTS)(converter)(_)))



  }
