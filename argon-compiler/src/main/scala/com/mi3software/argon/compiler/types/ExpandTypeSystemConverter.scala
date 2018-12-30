package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.core.Context

object ExpandTypeSystemConverter {

  trait Expander[F[_]] {
    def apply[A](a: A): F[A]
  }

  def apply[TContext <: Context with Singleton, F[_]]
  (ts1_outer: TypeSystem[TContext])
  (ts2_outer: TypeSystem[TContext] { type TTypeWrapper[A] = F[ts1_outer.TTypeWrapper[A]] })
  (f: Expander[F])
  : TypeSystemConverter[TContext, ts1_outer.type, ts2_outer.type] =
    new TypeSystemConverter[TContext, ts1_outer.type, ts2_outer.type] {
      override def convertType(ts1: ts1_outer.type)(ts2: ts2_outer.type)(t: ts1.TTypeWrapper[ts2.SimpleType]): ts2.TTypeWrapper[ts2.SimpleType] =
        f(t)
    }

}

