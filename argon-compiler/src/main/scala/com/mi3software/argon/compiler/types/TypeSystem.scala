package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import scalaz._
import Scalaz._
import Compilation.Operators._

import scala.collection.immutable.Vector

trait TypeSystem[TContext <: Context with Singleton] {

  val context: TContext

  type TTypeWrapper[A]
  type TType = TTypeWrapper[SimpleType]
  
  type WrapRef[T[_ <: Context, _[_, _]]] = AbsRef[context.type, T]

  final def fromSimpleType(simpleType: SimpleType): TType = wrapType(simpleType)
  def fromArType(arType: context.typeSystem.TType): TType

  def wrapType[A](a: A): TTypeWrapper[A]
  def mapTypeWrapper[A, B](t: TTypeWrapper[A])(f: A => B): TTypeWrapper[B]

  def isSubTypeWrapper[TComp[_] : Compilation, T]
  (f: (T, T) => TComp[Boolean])
  (a: TTypeWrapper[T], b: TTypeWrapper[T])
  : TComp[Boolean]

  final def isSubType[TComp[_] : Compilation](a: TType, b: TType): TComp[Boolean] =
    isSubTypeWrapper(isSimpleSubType[TComp])(a, b)


  sealed trait SimpleType
  final case class TraitType(arTrait: WrapRef[ArTrait], args: Vector[TType], baseTypes: BaseTypeInfoTrait) extends SimpleType
  final case class ClassType(arClass: WrapRef[ArClass], args: Vector[TType], baseTypes: BaseTypeInfoClass) extends SimpleType
  final case class DataConstructorType(ctor: WrapRef[DataConstructor], args: Vector[TType], instanceType: TraitType) extends SimpleType

  final case class MetaType(innerType: TType, baseType: TType) extends SimpleType

  final case class TupleTypeElement(elementType: TType)
  final case class TupleType(elements: Vector[TupleTypeElement]) extends SimpleType
  final case class FunctionType(argumentType: TType, resultType: TType) extends SimpleType
  final case class UnionType(first: TType, second: TType) extends SimpleType
  final case class IntersectionType(first: TType, second: TType) extends SimpleType


  final case class BaseTypeInfoTrait(baseTraits: Vector[TraitType])
  final case class BaseTypeInfoClass(baseClass: Option[ClassType], baseTraits: Vector[TraitType])



  final case class Variable[+Desc <: VariableLikeDescriptor](descriptor: Desc, name: VariableName, mutability: Mutability, varType: TType)

  final case class Parameter(tupleVars: Vector[Variable[DeconstructedParameterDescriptor]])

  object Parameter {

    def paramType(param: Parameter): TType =
      fromSimpleType(TupleType(param.tupleVars.map { tupleVar => TupleTypeElement(tupleVar.varType) }))

  }




  trait ArExpr {
    val exprType: TType

    def convertTypeSystem
    (otherTS: TypeSystem[context.type])
    (f: TType => otherTS.TType)
    : otherTS.ArExpr = ???
  }

  final case class ClassConstructorCall(classType: ClassType, classCtor: AbsRef[context.type, ClassConstructor], args: Vector[TType]) extends ArExpr {
    override val exprType: TType = fromSimpleType(classType)
  }
  final case class TupleElement(value: ArExpr)
  final case class CreateTuple(values: Vector[TupleElement]) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(TupleType(values.map { elem => TupleTypeElement(elem.value.exprType) }))
  }
  final case class DataConstructorCall(dataCtorInstanceType: DataConstructorType, args: Vector[TType]) extends ArExpr {
    override val exprType: TType = fromSimpleType(dataCtorInstanceType)
  }
  final case class FunctionCall(function: AbsRef[context.type, ArFunc], args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
  }
  final case class IfElse(condition: ArExpr, ifBody: ArExpr, elseBody: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(UnionType(ifBody.exprType, elseBody.exprType))
  }
  final case class LetBinding(variable: Variable[VariableDescriptor], value: ArExpr, next: ArExpr) extends ArExpr {
    override lazy val exprType: TType = next.exprType
  }
  final case class LoadConstantBool(value: Boolean, exprType: TType) extends ArExpr
  final case class LoadConstantInt(value: BigInt, exprType: TType) extends ArExpr
  final case class LoadConstantString(value: String, exprType: TType) extends ArExpr
  final case class LoadLambda(argVariable: Variable[VariableDescriptor], body: ArExpr) extends ArExpr {
    override lazy val exprType: TType = fromSimpleType(FunctionType(argVariable.varType, body.exprType))
  }
  final case class LoadTypeValue(value: TType) extends ArExpr {
    override lazy val exprType: TType = ???
  }
  final case class LoadVariable(variable: Variable[VariableLikeDescriptor]) extends ArExpr {
    override val exprType: TType = variable.varType
  }
  final case class MethodCall(method: AbsRef[context.type, ArMethod], instance: ArExpr, args: Vector[ArExpr], returnType: TType) extends ArExpr {
    override val exprType: TType = returnType
  }
  final case class Sequence(first: ArExpr, second: ArExpr) extends ArExpr {
    override lazy val exprType: TType = second.exprType
  }
  final case class StoreVariable(variable: Variable[VariableLikeDescriptor]) extends ArExpr {
    override val exprType: TType = fromSimpleType(TupleType(Vector.empty))
  }





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

  final def convertTypeSystem
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (t1: ts.TType)
  : otherTS.TType =
    converter.convertType(ts)(otherTS)(ts.mapTypeWrapper(t1)(convertSimpleTypeSystem(context)(ts)(otherTS)(converter)(_)))

  final def convertSimpleTypeSystem
  (context: Context)
  (ts: TypeSystem[context.type])
  (otherTS: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts.type, otherTS.type])
  (t1: ts.SimpleType)
  : otherTS.SimpleType = {
    import ts.{ context => _, _ }

    def convertTraitType(traitType: TraitType): otherTS.TraitType =
      otherTS.TraitType(
        traitType.arTrait,
        traitType.args.map(convertTypeSystem(context)(ts)(otherTS)(converter)(_)),
        otherTS.BaseTypeInfoTrait(traitType.baseTypes.baseTraits.map(convertTraitType(_)))
      )

    def convertClassType(classType: ClassType): otherTS.ClassType =
      otherTS.ClassType(
        classType.arClass,
        classType.args.map(convertTypeSystem(context)(ts)(otherTS)(converter)(_)),
        otherTS.BaseTypeInfoClass(
          classType.baseTypes.baseClass.map(convertClassType(_)),
          classType.baseTypes.baseTraits.map(convertTraitType(_))
        )
      )

    t1 match {
      case t1: TraitType => convertTraitType(t1)
      case t1: ClassType => convertClassType(t1)
      case DataConstructorType(ctor, args, instanceType) =>
        otherTS.DataConstructorType(
          ctor,
          args.map(convertTypeSystem(context)(ts)(otherTS)(converter)(_)),
          convertTraitType(instanceType)
        )

      case MetaType(innerType, baseType) =>
        otherTS.MetaType(
          convertTypeSystem(context)(ts)(otherTS)(converter)(innerType),
          convertTypeSystem(context)(ts)(otherTS)(converter)(innerType)
        )

      case TupleType(elements) =>
        otherTS.TupleType(elements.map { case TupleTypeElement(elementType) =>
          otherTS.TupleTypeElement(convertTypeSystem(context)(ts)(otherTS)(converter)(elementType))
        })

      case FunctionType(argumentType, resultType) =>
        otherTS.FunctionType(
          convertTypeSystem(context)(ts)(otherTS)(converter)(argumentType),
          convertTypeSystem(context)(ts)(otherTS)(converter)(resultType)
        )

      case UnionType(first, second) =>
        otherTS.UnionType(
          convertTypeSystem(context)(ts)(otherTS)(converter)(first),
          convertTypeSystem(context)(ts)(otherTS)(converter)(second)
        )

      case IntersectionType(first, second) =>
        otherTS.IntersectionType(
          convertTypeSystem(context)(ts)(otherTS)(converter)(first),
          convertTypeSystem(context)(ts)(otherTS)(converter)(second)
        )

    }

  }




}
