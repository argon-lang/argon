package com.mi3software.argon.compiler

import scalaz.Monad

object ArgonToSignatureTypeSystemConverter {

  def convert(context: Context)(sigTypeSystem: SignatureTypeSystem[context.type])(t: context.typeSystem.TType): sigTypeSystem.TType =
    t match {
      case TraitType((arTrait, _, _)) => Some(TraitType[sigTypeSystem.type](arTrait))
      case ClassType((arClass, _, _)) => Some(ClassType[sigTypeSystem.type](arClass))
      case DataConstructorType((dataCtor, _, _)) => Some(DataConstructorType[sigTypeSystem.type](dataCtor))
      case MetaType(innerType, _) => convert(context)(sigTypeSystem)(innerType)
      case TupleType(elements) => Some(TupleType(elements.map(_ => TupleTypeElement[sigTypeSystem.type](None))))
      case FunctionType(_, _) => Some(FunctionType[sigTypeSystem.type](None, None))
      case UnionType(_, _) | IntersectionType(_, _) => None
    }

  def apply(context: Context)(sigTypeSystem: SignatureTypeSystem[context.type]): TypeSystemConverter[context.typeSystem.type, sigTypeSystem.type] =
    new TypeSystemConverter[context.typeSystem.type, sigTypeSystem.type] {

      override def convertType[TComp[+ _] : Monad : Compilation]
      (t: context.typeSystem.TType)
      : TComp[sigTypeSystem.TType] =
        implicitly[Monad[TComp]].point(convert(context)(sigTypeSystem)(t))

    }

}

/*
sealed trait TypeBase[+TS <: TypeSystem]
final case class ErrorType[+TS <: TypeSystem]() extends TypeBase[TS]
final case class TraitType[+TS <: TypeSystem](traitInfo: TS#TTraitInfo) extends TypeBase[TS]
final case class ClassType[+TS <: TypeSystem](classInfo: TS#TClassInfo) extends TypeBase[TS]
final case class DataConstructorType[+TS <: TypeSystem](ctor: TS#TDataConstructorInfo) extends TypeBase[TS]

final case class MetaType[+TS <: TypeSystem](innerType: TS#TType, baseType: TS#TType) extends TypeBase[TS]

final case class TupleTypeElement[+TS <: TypeSystem](elementType: TS#TType)
final case class TupleType[+TS <: TypeSystem](elements: Vector[TupleTypeElement[TS]]) extends TypeBase[TS]
final case class FunctionType[+TS <: TypeSystem](argumentType: TS#TType, resultType: TS#TType) extends TypeBase[TS]
final case class UnionType[+TS <: TypeSystem](a: TS#TType, b: TS#TType) extends TypeBase[TS]
final case class IntersectionType[+TS <: TypeSystem](a: TS#TType, b: TS#TType) extends TypeBase[TS]

 */