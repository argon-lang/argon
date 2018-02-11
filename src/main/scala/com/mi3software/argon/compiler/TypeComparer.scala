package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

trait TypeComparer[TS <: TypeSystem] {

  def isSubTraitInfo(a: TS#TTraitInfo, b: TS#TTraitInfo): Boolean
  def isSubClassInfo(a: TS#TClassInfo, b: TS#TClassInfo): Boolean
  def classImplementsTrait(c: TS#TClassInfo, t: TS#TTraitInfo): Boolean
  def isSameDataConstructorInfo(a: TS#TDataConstructorInfo, b: TS#TDataConstructorInfo): Boolean

  def dataConstructorReturnType(ctor: TS#TDataConstructorInfo): TS#TType
  def traitMetaClass(traitInfo: TS#TTraitInfo): ClassType[TS]
  def classMetaClass(classInfo: TS#TClassInfo): ClassType[TS]

  def typeBaseToType(typeBase: TypeBase[TS]): TS#TType


  def isSubType(a: TS#TType, b: TS#TType): Boolean

  def isSameType(a: TS#TType, b: TS#TType): Boolean =
    isSubType(a, b) && isSubType(b, a)


  private def convertTupleToMetaType(tupleType: TupleType[TS]): Option[MetaType[TS]] =
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

  def isSubTypeBase(a: TypeBase[TS], b: TypeBase[TS]): Boolean =
    (a match {
      case UnionType(leftA, rightA) =>
        isSubType(leftA, typeBaseToType(b)) && isSubType(rightA, typeBaseToType(b))
      case _ => false
    }) || (b match {
      case IntersectionType(leftB, rightB) =>
        isSubType(typeBaseToType(a), leftB) && isSubType(typeBaseToType(a), rightB)
      case _ => false
    }) || (b match {
      case UnionType(leftB, rightB) =>
        isSubType(typeBaseToType(a), leftB) || isSubType(typeBaseToType(a), rightB)
      case _ => false
    }) || (a match {
      case IntersectionType(leftA, rightA) =>
        isSubType(leftA, typeBaseToType(b)) || isSubType(rightA, typeBaseToType(b))
      case _ => false
    }) || ((a, b) match {
      case (TraitType(aTrait), TraitType(bTrait)) => isSubTraitInfo(aTrait, bTrait)
      case (ClassType(aClass), ClassType(bClass)) => isSubClassInfo(aClass, bClass)
      case (TraitType(aTrait), ClassType(bClass)) => classImplementsTrait(bClass, aTrait)
      case (ClassType(_), TraitType(_)) => false

      case (DataConstructorType(aCtor), DataConstructorType(bCtor)) => isSameDataConstructorInfo(aCtor, bCtor)
      case (DataConstructorType(aCtor), _) => isSubType(dataConstructorReturnType(aCtor), typeBaseToType(b))
      case (_, DataConstructorType(_)) => false

      case (MetaType(innerTypeA, _), MetaType(innerTypeB, _)) =>
        isSameType(innerTypeA, innerTypeB)

      case (MetaType(_, _), tupleTypeB @ TupleType(_)) =>
        convertTupleToMetaType(tupleTypeB) match {
          case Some(metaTypeB) => isSubTypeBase(a, metaTypeB)
          case None => false
        }

      case (tupleTypeA @ TupleType(_), MetaType(_, _)) =>
        convertTupleToMetaType(tupleTypeA) match {
          case Some(metaTypeA) => isSubTypeBase(metaTypeA, b)
          case None => false
        }

      case (MetaType(_, baseType), _) => isSubType(baseType, typeBaseToType(b))
      case (_, MetaType(_, _)) => false

      case (TupleType(elemsA), TupleType(elemsB)) =>
        elemsA.size === elemsB.size &&
          elemsA.zip(elemsB).forall {
            case (TupleTypeElement(elemA), TupleTypeElement(elemB)) =>
              isSubType(elemA, elemB)
          }

      case (TupleType(_), _) | (_, TupleType(_)) => false


      case (FunctionType(argA, retA), FunctionType(argB, retB)) =>
        isSubType(argB, argA) && isSubType(retA, retB)

      case (FunctionType(_, _), _) | (_, FunctionType(_, _)) => false
      case (IntersectionType(_, _), _) | (_, IntersectionType(_, _)) => false
      case (UnionType(_, _), _) | (_, UnionType(_, _)) => false

    })

}
