package dev.argon.compiler.core

import dev.argon.compiler.types.{TypeSystem, TypeSystemConverter}
import cats.Monad

trait SignatureResultConverter[TResult[TContext <: Context with Singleton, _ <: TypeSystem[TContext] with Singleton]] {
  def convertTypeSystem[F[_]: Monad]
  (context: Context)
  (ts1: TypeSystem[context.type])
  (ts2: TypeSystem[context.type])
  (converter: TypeSystemConverter[context.type, ts1.type, ts2.type, F])
  (result: TResult[context.type, ts1.type])
  : F[TResult[context.type, ts2.type]]
}
