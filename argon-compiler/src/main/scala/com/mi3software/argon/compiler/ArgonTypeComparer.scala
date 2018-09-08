package com.mi3software.argon.compiler

import com.thoughtworks.each.Monadic._
import scalaz._
import Scalaz._

object ArgonTypeComparer {
  def apply[TContext <: Context](ts: ArgonTypeSystem[TContext]): TypeComparerUnerased[ts.type, Compilation] =
    new TypeComparerUnerased[ts.type, Compilation] {

      override def typeBaseToType(typeBase: TypeBase[ts.type]): TypeBase[ts.type] = typeBase

      @monadic[F]
      override def isSubTraitInfo[F[+_] : Compilation](a: (ArTrait[TContext], Vector[TypeBase[ts.type]], ArTrait.ResultInfo[ts.type]), b: (ArTrait[TContext], Vector[TypeBase[ts.type]], ArTrait.ResultInfo[ts.type])): F[Boolean] = {
        val (traitA, paramsA, ArTrait.ResultInfo(baseTypes)) = a
        val (traitB, paramsB, _) = b

        (
          traitA.descriptor === traitB.descriptor &&
            paramsA.length === paramsB.length &&
            paramsA.zip(paramsB).allM((isSameType[F] _).tupled).each
        ) || baseTypes.baseTraits.anyM(traitType => isSubTraitInfo[F](traitType.traitInfo, b)).each
      }

      @monadic[F]
      override def isSubClassInfo[F[+_] : Compilation](a: (ArClass[TContext], Vector[TypeBase[ts.type]], ArClass.ResultInfo[ts.type]), b: (ArClass[TContext], Vector[TypeBase[ts.type]], ArClass.ResultInfo[ts.type])): F[Boolean] = {
        val (classA, paramsA, ArClass.ResultInfo(baseTypes)) = a
        val (classB, paramsB, _) = b

        (
          classA.descriptor === classB.descriptor &&
            paramsA.length === paramsB.length &&
            paramsA.zip(paramsB).allM((isSameType[F] _).tupled).each
        ) || baseTypes.baseClass.anyM(classType => isSubClassInfo[F](classType.classInfo, b)).each
      }

      @monadic[F]
      override def classImplementsTrait[F[+_] : Compilation](c: (ArClass[TContext], Vector[TypeBase[ts.type]], ArClass.ResultInfo[ts.type]), t: (ArTrait[TContext], Vector[TypeBase[ts.type]], ArTrait.ResultInfo[ts.type])): F[Boolean] = {
        val (_, _, ArClass.ResultInfo(baseTypes)) = c

        baseTypes.baseTraits.anyM(traitType => isSubTraitInfo[F](t, traitType.traitInfo)).each ||
          baseTypes.baseClass.anyM(baseClass => classImplementsTrait[F](baseClass.classInfo, t)).each
      }

      @monadic[F]
      override def isSameDataConstructorInfo[F[+_] : Compilation](a: (DataConstructor[TContext], Vector[TypeBase[ts.type]], DataConstructor.ResultInfo[ts.type]), b: (DataConstructor[TContext], Vector[TypeBase[ts.type]], DataConstructor.ResultInfo[ts.type])): F[Boolean] = {
        val (ctorA, paramsA, _) = a
        val (ctorB, paramsB, _) = b

        ctorA.descriptor === ctorB.descriptor &&
          paramsA.length === paramsB.length &&
          paramsA.zip(paramsB).allM((isSameType[F] _).tupled).each
      }

      override def dataConstructorReturnType(ctor: (DataConstructor[TContext], Vector[TypeBase[ts.type]], DataConstructor.ResultInfo[ts.type])): TypeBase[ts.type] =
        ctor._3.instanceType

      override def isSubType[F[+_] : Compilation](a: TypeBase[ts.type], b: TypeBase[ts.type]): F[Boolean] =
        isSubTypeBase(a, b)
    }
}
