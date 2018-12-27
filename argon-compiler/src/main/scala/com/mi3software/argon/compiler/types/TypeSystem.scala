package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import scalaz._
import Scalaz._
import Compilation.Operators._

import scala.collection.immutable.Vector

trait TypeSystem {

  val context: Context
  type TContext = context.type

  type TTypeWrapper[A]
  type TType = TTypeWrapper[SimpleType]
  
  type WrapRef[T[_ <: Context, _[_, _]]] = AbsRef[context.type, T]

  final def fromSimpleType(simpleType: SimpleType): TType = wrapType(simpleType)
  def fromArType(arType: context.typeSystem.TType): TType

  def wrapType[A](a: A): TTypeWrapper[A]
  def mapTypeWrapper[A, B](t: TTypeWrapper[A])(f: A => B): TTypeWrapper[B]

  def isSubTypeWrapper[TComp[+_] : Compilation, T]
  (f: (T, T) => TComp[Boolean])
  (a: TTypeWrapper[T], b: TTypeWrapper[T])
  : TComp[Boolean]

  final def isSubType[TComp[+_] : Compilation](a: TType, b: TType): TComp[Boolean] =
    isSubTypeWrapper(isSimpleSubType[TComp])(a, b)

  final def convertTypeSystem(otherTS: TypeSystem { val context: TypeSystem.this.context.type })(converter: TypeSystemConverter[this.type, otherTS.type])(t1: TType): otherTS.TType =
    converter.convertType(this)(otherTS)(mapTypeWrapper(t1)(convertSimpleTypeSystem(otherTS)(converter)(_)))


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




  private def isSimpleSubType[F[+_] : Compilation](a: SimpleType, b: SimpleType): F[Boolean] = {

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

  final def convertSimpleTypeSystem(otherTS: TypeSystem { val context: TypeSystem.this.context.type })(converter: TypeSystemConverter[this.type, otherTS.type])(t1: SimpleType): otherTS.SimpleType = {

    def convertTraitType(traitType: TraitType): otherTS.TraitType =
      otherTS.TraitType(
        traitType.arTrait,
        traitType.args.map(convertTypeSystem(otherTS)(converter)(_)),
        otherTS.BaseTypeInfoTrait(traitType.baseTypes.baseTraits.map(convertTraitType(_)))
      )

    def convertClassType(classType: ClassType): otherTS.ClassType =
      otherTS.ClassType(
        classType.arClass,
        classType.args.map(convertTypeSystem(otherTS)(converter)(_)),
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
          args.map(convertTypeSystem(otherTS)(converter)(_)),
          convertTraitType(instanceType)
        )

      case MetaType(innerType, baseType) =>
        otherTS.MetaType(
          convertTypeSystem(otherTS)(converter)(innerType),
          convertTypeSystem(otherTS)(converter)(innerType)
        )

      case TupleType(elements) =>
        otherTS.TupleType(elements.map { case TupleTypeElement(elementType) =>
            otherTS.TupleTypeElement(convertTypeSystem(otherTS)(converter)(elementType))
        })

      case FunctionType(argumentType, resultType) =>
        otherTS.FunctionType(
          convertTypeSystem(otherTS)(converter)(argumentType),
          convertTypeSystem(otherTS)(converter)(resultType)
        )

      case UnionType(first, second) =>
        otherTS.UnionType(
          convertTypeSystem(otherTS)(converter)(first),
          convertTypeSystem(otherTS)(converter)(second)
        )

      case IntersectionType(first, second) =>
        otherTS.IntersectionType(
          convertTypeSystem(otherTS)(converter)(first),
          convertTypeSystem(otherTS)(converter)(second)
        )

    }

  }



}
