package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

object ArgonTypeComparer {
  def apply[TContext <: Context](ts: ArgonTypeSystem[TContext]): TypeComparerUnerased[ts.type] =
    new TypeComparerUnerased[ts.type] {

      override def typeBaseToType(typeBase: TypeBase[ts.type]): TypeBase[ts.type] = typeBase

      override def isSubTraitInfo(a: (ArTrait[TContext], Vector[TypeBase[ts.type]], ArTrait.ResultInfo[ts.type]), b: (ArTrait[TContext], Vector[TypeBase[ts.type]], ArTrait.ResultInfo[ts.type])): Boolean = {
        val (traitA, paramsA, ArTrait.ResultInfo(baseTypes)) = a
        val (traitB, paramsB, _) = b

        (
          traitA.descriptor === traitB.descriptor &&
            paramsA.length === paramsB.length &&
            paramsA.iterator.zip(paramsB.iterator).forall((isSameType _).tupled)
        ) || baseTypes.baseTraits.any(traitType => isSubTraitInfo(traitType.traitInfo, b))
      }

      override def isSubClassInfo(a: (ArClass[TContext], Vector[TypeBase[ts.type]], ArClass.ResultInfo[ts.type]), b: (ArClass[TContext], Vector[TypeBase[ts.type]], ArClass.ResultInfo[ts.type])): Boolean = {
        val (classA, paramsA, ArClass.ResultInfo(baseTypes)) = a
        val (classB, paramsB, _) = b

        (
          classA.descriptor === classB.descriptor &&
            paramsA.length === paramsB.length &&
            paramsA.iterator.zip(paramsB.iterator).forall((isSameType _).tupled)
        ) || baseTypes.baseClass.any(classType => isSubClassInfo(classType.classInfo, b))
      }

      override def classImplementsTrait(c: (ArClass[TContext], Vector[TypeBase[ts.type]], ArClass.ResultInfo[ts.type]), t: (ArTrait[TContext], Vector[TypeBase[ts.type]], ArTrait.ResultInfo[ts.type])): Boolean = {
        val (_, _, ArClass.ResultInfo(baseTypes)) = c

        baseTypes.baseTraits.any(traitType => isSubTraitInfo(t, traitType.traitInfo)) ||
          baseTypes.baseClass.any(baseClass => classImplementsTrait(baseClass.classInfo, t))
      }

      override def isSameDataConstructorInfo(a: (DataConstructor[TContext], Vector[TypeBase[ts.type]], DataConstructor.ResultInfo[ts.type]), b: (DataConstructor[TContext], Vector[TypeBase[ts.type]], DataConstructor.ResultInfo[ts.type])): Boolean = {
        val (ctorA, paramsA, _) = a
        val (ctorB, paramsB, _) = b

        ctorA.descriptor === ctorB.descriptor &&
          paramsA.length === paramsB.length &&
          paramsA.iterator.zip(paramsB.iterator).forall((isSameType _).tupled)
      }

      override def dataConstructorReturnType(ctor: (DataConstructor[TContext], Vector[TypeBase[ts.type]], DataConstructor.ResultInfo[ts.type])): TypeBase[ts.type] =
        ctor._3.instanceType

      override def isSubType(a: TypeBase[ts.type], b: TypeBase[ts.type]): Boolean =
        isSubTypeBase(a, b)
    }
}
