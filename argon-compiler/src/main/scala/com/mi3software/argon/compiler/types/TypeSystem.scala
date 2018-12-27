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

  def fromSimpleType(typeBase: SimpleType): TType

  def isSubTypeWrapper[TComp[+_] : Compilation, T]
  (f: (T, T) => TComp[Boolean])
  (a: TTypeWrapper[T], b: TTypeWrapper[T])
  : TComp[Boolean]

  final def isSubType[TComp[+_] : Compilation](a: TType, b: TType): TComp[Boolean] =
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

}
