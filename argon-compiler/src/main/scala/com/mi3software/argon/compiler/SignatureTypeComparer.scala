package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

object SignatureTypeComparer {

  def apply[TContext <: Context](ts: SignatureTypeSystem[TContext]): TypeComparer[ts.type] =
    new TypeComparer[ts.type] {
      override def isSubTraitInfo(a: ArTrait[TContext], b: ArTrait[TContext]): Boolean =
        a === b

      override def isSubClassInfo(a: ArClass[TContext], b: ArClass[TContext]): Boolean =
        a === b

      override def classImplementsTrait(c: ArClass[TContext], t: ArTrait[TContext]): Boolean =
        false

      override def isSameDataConstructorInfo(a: DataConstructor[TContext], b: DataConstructor[TContext]): Boolean =
        a === b

      override def dataConstructorReturnType(ctor: DataConstructor[TContext]): TypeBase[ts.type] = ???

      override def traitMetaClass(traitInfo: ArTrait[TContext]): ClassType[ts.type] = ???

      override def classMetaClass(classInfo: ArClass[TContext]): ClassType[ts.type] = ???

      override def typeBaseToType(typeBase: TypeBase[ts.type]): TypeBase[ts.type] = ???

      override def isSubType(a: TypeBase[ts.type], b: TypeBase[ts.type]): Boolean = ???
    }

}
