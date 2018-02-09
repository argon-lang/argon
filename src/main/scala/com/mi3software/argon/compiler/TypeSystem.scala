package com.mi3software.argon.compiler

trait TypeSystem {

  type TType
  type TTrait
  type TClass
  type TDataConstructor

  sealed trait TypeBase
  sealed case class TraitType(traitInfo: TTrait, arguments: Vector[TType]) extends TypeBase
  sealed case class ClassType(classInfo: TClass, arguments: Vector[TType]) extends TypeBase
  sealed case class DataConstructorType(ctor: TDataConstructor, arguments: Vector[TType]) extends TypeBase

  sealed case class TupleElement(elementType: TType)
  sealed case class TupleType(elements: Vector[TupleElement]) extends TypeBase

  sealed case class TraitMetaType(traitType: TraitType) extends TypeBase
  sealed case class ClassMetaType(traitType: TraitType) extends TypeBase

  sealed case class FunctionType(argumentType: TType, resultType: TType) extends TypeBase
  sealed case class UnionType(a: TType, b: TType) extends TypeBase
  sealed case class IntersectionType(a: TType, b: TType) extends TypeBase

}
