package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.core.Context

object ExpandTypeSystemConverter {

  trait Expander[F[_]] {
    def apply[A](a: A): F[A]
  }

  def apply[TContext <: Context with Singleton, F[_]]
  (ts1_outer: TypeSystem[TContext])
  (ts2_outer: TypeSystem[TContext] {
    type TTypeWrapper[A] = F[ts1_outer.TTypeWrapper[A]]
    type TUniverse = ts1_outer.TUniverse
    type TTypeUniverse = ts1_outer.TTypeUniverse
  })
  (f: Expander[F])
  : TypeSystemConverter[TContext, ts1_outer.type, ts2_outer.type] =
    new TypeSystemConverter[TContext, ts1_outer.type, ts2_outer.type] {


      override def convertType[A](ts1: ts1_outer.type)(ts2: ts2_outer.type)(fromSimpleType: ts2.SimpleType => A)(t: ts1.TTypeWrapper[A]): F[ts1_outer.TTypeWrapper[A]] =
        f(t)

      override def convertUniverse(ts1: ts1_outer.type)(ts2: ts2_outer.type)(universe: ts1.TUniverse): ts2.TUniverse =
        universe

      override def convertTypeUniverse(ts1: ts1_outer.type)(ts2: ts2_outer.type)(universe: ts1.TTypeUniverse): ts2.TTypeUniverse =
        universe

    }

}

