package com.mi3software.argon.compiler.core

sealed trait AbsRef[TContext <: Context, T[_ <: Context, _[_, _]]] {
  type PayloadSpec[_, _]
  val value: T[TContext, PayloadSpec]
}

object AbsRef {

  def apply[TContext <: Context, TPayloadSpec[_, _], T[_ <: Context, _[_, _]]](instance: T[TContext, TPayloadSpec]): AbsRef[TContext, T] =
    new AbsRef[TContext, T] {
      override type PayloadSpec[A, B] = TPayloadSpec[A, B]
      override val value: T[TContext, PayloadSpec] = instance
    }

}
