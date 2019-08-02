package dev.argon.compiler.types

import dev.argon.compiler.Compilation
import dev.argon.compiler.core.Context

trait TypeSystemConverter[TContext <: Context with Singleton, TS1 <: TypeSystem[TContext], TS2 <: TypeSystem[TContext], F[_]] {
  def convertType[A](ts1: TS1)(ts2: TS2)(fromExpr: ts2.ArExpr => A)(t: ts1.TTypeWrapper[A]): F[ts2.TTypeWrapper[A]]
}

