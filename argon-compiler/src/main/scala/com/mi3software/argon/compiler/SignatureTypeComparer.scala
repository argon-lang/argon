package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

import scala.None

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

      override def dataConstructorReturnType(ctor: DataConstructor[TContext]): Option[TypeBaseConcrete[ts.type]] = ???

      override def typeBaseConcreteToType(typeBase: TypeBaseConcrete[ts.type]): Option[TypeBaseConcrete[ts.type]] =
        Some(typeBase)

      override def isSubType(a: Option[TypeBaseConcrete[ts.type]], b: Option[TypeBaseConcrete[ts.type]]): Boolean =
        (a, b) match {
          case (Some(a), Some(b)) => isSubTypeBaseConcrete(a, b)
          case (None, None) => true
          case _ => false
        }

      override def tupleElementIsSubType(a: None.type, b: None.type): Boolean = true

      override def functionArgIsSubType(a: None.type, b: None.type): Boolean = true

      override def functionResultIsSubType(a: None.type, b: None.type): Boolean = true
    }

}
