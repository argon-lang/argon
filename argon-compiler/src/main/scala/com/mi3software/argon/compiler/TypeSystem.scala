package com.mi3software.argon.compiler

trait TypeSystem {

  type TType

  def fromTypeBaseConcrete(typeBase: TypeBaseConcrete[this.type]): TType

  type TTraitInfo
  type TClassInfo
  type TDataConstructorInfo
  type TTupleElementType
  type TFunctionArgumentType
  type TFunctionResultType

}

trait TypeSystemUnerased extends TypeSystem {

  def fromTypeBase(typeBase: TypeBase[this.type]): TType
  override def fromTypeBaseConcrete(typeBase: TypeBaseConcrete[TypeSystemUnerased.this.type]): TType =
    fromTypeBase(typeBase)

  override type TTupleElementType = TType
  override type TFunctionArgumentType = TType
  override type TFunctionResultType = TType
}


sealed trait TypeBase[+TS <: TypeSystem]
sealed trait TypeBaseConcrete[+TS <: TypeSystem] extends TypeBase[TS]
final case class TraitType[+TS <: TypeSystem](traitInfo: TS#TTraitInfo) extends TypeBaseConcrete[TS]
final case class ClassType[+TS <: TypeSystem](classInfo: TS#TClassInfo) extends TypeBaseConcrete[TS]
final case class DataConstructorType[+TS <: TypeSystem](ctor: TS#TDataConstructorInfo) extends TypeBaseConcrete[TS]

final case class MetaType[+TS <: TypeSystem](innerType: TS#TType, baseType: TS#TType) extends TypeBase[TS]

final case class TupleTypeElement[+TS <: TypeSystem](elementType: TS#TTupleElementType)
final case class TupleType[+TS <: TypeSystem](elements: Vector[TupleTypeElement[TS]]) extends TypeBaseConcrete[TS]
final case class FunctionType[+TS <: TypeSystem](argumentType: TS#TFunctionArgumentType, resultType: TS#TFunctionResultType) extends TypeBaseConcrete[TS]
final case class UnionType[+TS <: TypeSystem](a: TS#TType, b: TS#TType) extends TypeBase[TS]
final case class IntersectionType[+TS <: TypeSystem](a: TS#TType, b: TS#TType) extends TypeBase[TS]
