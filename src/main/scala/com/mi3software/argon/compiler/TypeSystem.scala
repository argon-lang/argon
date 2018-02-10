package com.mi3software.argon.compiler

trait TypeSystem {

  type TType

  type TTraitInfo
  type TClassInfo
  type TDataConstructorInfo

}


sealed trait TypeBase[TS <: TypeSystem]
final case class TraitType[TS <: TypeSystem](traitInfo: TS#TTraitInfo) extends TypeBase[TS]
final case class ClassType[TS <: TypeSystem](classInfo: TS#TClassInfo) extends TypeBase[TS]
final case class DataConstructorType[TS <: TypeSystem](ctor: TS#TDataConstructorInfo) extends TypeBase[TS]

final case class TraitMetaType[TS <: TypeSystem](traitInfo: TS#TTraitInfo) extends TypeBase[TS]
final case class ClassMetaType[TS <: TypeSystem](classInfo: TS#TClassInfo) extends TypeBase[TS]

final case class TupleElement[TS <: TypeSystem](elementType: TS#TType)
final case class TupleType[TS <: TypeSystem](elements: Vector[TupleElement[TS]]) extends TypeBase[TS]
final case class FunctionType[TS <: TypeSystem](argumentType: TS#TType, resultType: TS#TType) extends TypeBase[TS]
final case class UnionType[TS <: TypeSystem](a: TS#TType, b: TS#TType) extends TypeBase[TS]
final case class IntersectionType[TS <: TypeSystem](a: TS#TType, b: TS#TType) extends TypeBase[TS]
