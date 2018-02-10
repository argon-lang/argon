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

  def isSameTypeBase(a: TypeBase[TS], b: TypeBase[TS]): Boolean =
    isSubTypeBase(a, b) && isSubTypeBase(b, a)


  def isSubTypeBase(a: TypeBase[TS], b: TypeBase[TS]): Boolean =
    (a, b) match {
      case (TraitType(aTrait), TraitType(bTrait)) => isSubTraitInfo(aTrait, bTrait)
      case (ClassType(aClass), ClassType(bClass)) => isSubClassInfo(aClass, bClass)
      case (TraitType(aTrait), ClassType(bClass)) => classImplementsTrait(bClass, aTrait)
      case (ClassType(_), TraitType(_)) => false

      case (DataConstructorType(aCtor), DataConstructorType(bCtor)) => isSameDataConstructorInfo(aCtor, bCtor)
      case (DataConstructorType(aCtor), _) => isSubType(dataConstructorReturnType(aCtor), typeBaseToType(b))
      case (_, DataConstructorType(_)) => false

      case (TraitMetaType(traitInfoA), TraitMetaType(traitInfoB)) =>
        isSubTraitInfo(traitInfoA, traitInfoB) && isSubTraitInfo(traitInfoB, traitInfoA)
      case (TraitMetaType(traitInfo), _) => isSubTypeBase(traitMetaClass(traitInfo), b)
      case (_, TraitMetaType(_)) => false

      case (ClassMetaType(classInfoA), ClassMetaType(classInfoB)) =>
        isSubClassInfo(classInfoA, classInfoB) && isSubClassInfo(classInfoB, classInfoA)
      case (ClassMetaType(classInfo), _) => isSubTypeBase(classMetaClass(classInfo), b)
      case (_, ClassMetaType(_)) => false


      case (TupleType(elemsA), TupleType(elemsB)) =>
        elemsA.size === elemsB.size &&
          elemsA.zip(elemsB).forall {
            case (TupleElement(elemA), TupleElement(elemB)) =>
              isSubType(elemA, elemB)
          }


      case (FunctionType(argA, retA), FunctionType(argB, retB)) =>
        isSubType(argB, argA) && isSubType(retA, retB)

      case (UnionType(leftA, rightA), _) =>
        isSubType(leftA, typeBaseToType(b)) && isSubType(rightA, typeBaseToType(b))

      case (_, UnionType(leftB, rightB)) =>
        isSubType(typeBaseToType(a), leftB) || isSubType(typeBaseToType(a), rightB)

      case (IntersectionType(leftA, rightA), _) =>
        isSubType(leftA, typeBaseToType(b)) || isSubType(rightA, typeBaseToType(b))

      case (_, IntersectionType(leftB, rightB)) =>
        isSubType(typeBaseToType(a), leftB) && isSubType(typeBaseToType(a), rightB)

      case (TupleType(_), _) | (_, TupleType(_)) => false
      case (FunctionType(_, _), _) | (_, FunctionType(_, _)) => false


    }

}
