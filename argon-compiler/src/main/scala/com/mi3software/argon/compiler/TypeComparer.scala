package com.mi3software.argon.compiler

import com.thoughtworks.each.Monadic._
import scalaz._
import Scalaz._

trait TypeComparer[TS <: TypeSystem, TCompEff[A[_]] <: Compilation[A]] {

  def isSubTraitInfo[F[+_] : TCompEff](a: TS#TTraitInfo, b: TS#TTraitInfo): F[Boolean]
  def isSubClassInfo[F[+_] : TCompEff](a: TS#TClassInfo, b: TS#TClassInfo): F[Boolean]
  def classImplementsTrait[F[+_] : TCompEff](c: TS#TClassInfo, t: TS#TTraitInfo): F[Boolean]
  def isSameDataConstructorInfo[F[+_] : TCompEff](a: TS#TDataConstructorInfo, b: TS#TDataConstructorInfo): F[Boolean]

  def dataConstructorReturnType(ctor: TS#TDataConstructorInfo): TS#TType

  def typeBaseConcreteToType(typeBase: TypeBaseConcrete[TS]): TS#TType


  def isSubType[F[+_] : TCompEff](a: TS#TType, b: TS#TType): F[Boolean]

  @monadic[F]
  def isSameType[F[+_] : TCompEff](a: TS#TType, b: TS#TType): F[Boolean] =
    isSubType(a, b).each && isSubType(b, a).each

  def tupleElementIsSubType[F[+_] : TCompEff](a: TS#TTupleElementType, b: TS#TTupleElementType): F[Boolean]
  def functionArgIsSubType[F[+_] : TCompEff](a: TS#TFunctionArgumentType, b: TS#TFunctionArgumentType): F[Boolean]
  def functionResultIsSubType[F[+_] : TCompEff](a: TS#TFunctionResultType, b: TS#TFunctionResultType): F[Boolean]

  @monadic[F]
  def isSubTypeBaseConcrete[F[+_] : TCompEff](a: TypeBaseConcrete[TS], b: TypeBaseConcrete[TS]): F[Boolean] =
    (a, b) match {
      case (TraitType(aTrait), TraitType(bTrait)) => isSubTraitInfo[F](aTrait, bTrait).each
      case (ClassType(aClass), ClassType(bClass)) => isSubClassInfo(aClass, bClass).each
      case (TraitType(aTrait), ClassType(bClass)) => classImplementsTrait(bClass, aTrait).each
      case (ClassType(_), TraitType(_)) => false

      case (DataConstructorType(aCtor), DataConstructorType(bCtor)) => isSameDataConstructorInfo(aCtor, bCtor).each
      case (DataConstructorType(aCtor), _) => isSubType(dataConstructorReturnType(aCtor), typeBaseConcreteToType(b)).each
      case (_, DataConstructorType(_)) => false

      case (TupleType(elemsA), TupleType(elemsB)) =>
        elemsA.size === elemsB.size &&
          elemsA.zip(elemsB).allM {
            case (TupleTypeElement(elemA), TupleTypeElement(elemB)) =>
              tupleElementIsSubType(elemA, elemB)
          }.each

      case (TupleType(_), _) | (_, TupleType(_)) => false


      case (FunctionType(argA, retA), FunctionType(argB, retB)) =>
        functionArgIsSubType(argB, argA).each && functionResultIsSubType(retA, retB).each

      case (FunctionType(_, _), _) | (_, FunctionType(_, _)) => false

    }
}

trait TypeComparerUnerased[TS <: TypeSystemUnerased, TCompEff[A[_]] <: Compilation[A]] extends TypeComparer[TS, TCompEff] {

  def typeBaseToType(typeBase: TypeBase[TS]): TS#TType

  final override def typeBaseConcreteToType(typeBase: TypeBaseConcrete[TS]): TS#TType =
    typeBaseToType(typeBase)

  final override def tupleElementIsSubType[F[+_] : TCompEff](a: TS#TTupleElementType, b: TS#TTupleElementType): F[Boolean] =
    isSubType(a, b)

  final override def functionArgIsSubType[F[+_] : TCompEff](a: TS#TFunctionArgumentType, b: TS#TFunctionArgumentType): F[Boolean] =
    isSubType(a, b)

  final override def functionResultIsSubType[F[+_] : TCompEff](a: TS#TFunctionResultType, b: TS#TFunctionResultType): F[Boolean] =
    isSubType(a, b)

  protected def convertTupleToMetaType(tupleType: TupleType[TS]): Option[MetaType[TS]] =
    tupleType.elements
      .traverse[Option, (TS#TType, TS#TType)] {
      case TupleTypeElement(metaType: MetaType[TS]) => Some((metaType.innerType, metaType.baseType))
      case _ => None
    }
      .map { elemPairs =>
        MetaType[TS](
          typeBaseToType(TupleType[TS](elemPairs.map { case (innerType, _) => TupleTypeElement[TS](innerType) })),
          typeBaseToType(TupleType[TS](elemPairs.map { case (_, baseType) => TupleTypeElement[TS](baseType) }))
        )
      }

  @monadic[F]
  def isSubTypeBase[F[+_] : TCompEff](a: TypeBase[TS], b: TypeBase[TS]): F[Boolean] =
    (a match {
      case UnionType(leftA, rightA) =>
        isSubType[F](leftA, typeBaseToType(b)).each && isSubType[F](rightA, typeBaseToType(b)).each
      case _ => false
    }) || (b match {
      case IntersectionType(leftB, rightB) =>
        isSubType[F](typeBaseToType(a), leftB).each && isSubType[F](typeBaseToType(a), rightB).each
      case _ => false
    }) || (b match {
      case UnionType(leftB, rightB) =>
        isSubType[F](typeBaseToType(a), leftB).each || isSubType[F](typeBaseToType(a), rightB).each
      case _ => false
    }) || (a match {
      case IntersectionType(leftA, rightA) =>
        isSubType[F](leftA, typeBaseToType(b)).each || isSubType[F](rightA, typeBaseToType(b)).each
      case _ => false
    }) || ((a, b) match {

      case (MetaType(innerTypeA, _), MetaType(innerTypeB, _)) =>
        isSameType[F](innerTypeA, innerTypeB).each

      case (MetaType(_, _), tupleTypeB @ TupleType(_)) =>
        convertTupleToMetaType(tupleTypeB) match {
          case Some(metaTypeB) => isSubTypeBase[F](a, metaTypeB).each
          case None => false
        }

      case (tupleTypeA @ TupleType(_), MetaType(_, _)) =>
        convertTupleToMetaType(tupleTypeA) match {
          case Some(metaTypeA) => isSubTypeBase[F](metaTypeA, b).each
          case None => false
        }

      case (MetaType(_, baseType), _) => isSubType[F](baseType, typeBaseToType(b)).each
      case (_, MetaType(_, _)) => false

      case (a: TypeBaseConcrete[TS], b: TypeBaseConcrete[TS]) =>
        isSubTypeBaseConcrete[F](a, b).each

      case (FunctionType(_, _), _) | (_, FunctionType(_, _)) => false
      case (IntersectionType(_, _), _) | (_, IntersectionType(_, _)) => false
      case (UnionType(_, _), _) | (_, UnionType(_, _)) => false

    })

}
