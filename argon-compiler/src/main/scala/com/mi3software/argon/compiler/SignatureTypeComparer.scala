package com.mi3software.argon.compiler

import com.thoughtworks.each.Monadic._
import scalaz._
import Scalaz._

import scala.None

object SignatureTypeComparer {

  def apply[TContext <: Context](ts: SignatureTypeSystem[TContext]): TypeComparer[ts.type, Compilation] =
    new TypeComparer[ts.type, Compilation] {

      @monadic[F]
      override def isSubTraitInfo[F[+_] : Compilation](a: ArTrait[TContext], b: ArTrait[TContext]): F[Boolean] =
        a === b

      @monadic[F]
      override def isSubClassInfo[F[+_] : Compilation](a: ArClass[TContext], b: ArClass[TContext]): F[Boolean] =
        a === b

      @monadic[F]
      override def classImplementsTrait[F[+_] : Compilation](c: ArClass[TContext], t: ArTrait[TContext]): F[Boolean] =
        false

      @monadic[F]
      override def isSameDataConstructorInfo[F[+_] : Compilation](a: DataConstructor[TContext], b: DataConstructor[TContext]): F[Boolean] =
        a === b

      override def dataConstructorReturnType(ctor: DataConstructor[TContext]): Option[TypeBaseConcrete[ts.type]] = ???

      override def typeBaseConcreteToType(typeBase: TypeBaseConcrete[ts.type]): Option[TypeBaseConcrete[ts.type]] =
        Some(typeBase)

      @monadic[F]
      override def isSubType[F[+_] : Compilation](a: Option[TypeBaseConcrete[ts.type]], b: Option[TypeBaseConcrete[ts.type]]): F[Boolean] =
        (a, b) match {
          case (Some(a), Some(b)) => isSubTypeBaseConcrete(a, b).each
          case (None, None) => true
          case _ => false
        }

      @monadic[F]
      override def tupleElementIsSubType[F[+_] : Compilation](a: None.type, b: None.type): F[Boolean] = true

      @monadic[F]
      override def functionArgIsSubType[F[+_] : Compilation](a: None.type, b: None.type): F[Boolean] = true

      @monadic[F]
      override def functionResultIsSubType[F[+_] : Compilation](a: None.type, b: None.type): F[Boolean] = true
    }

}
