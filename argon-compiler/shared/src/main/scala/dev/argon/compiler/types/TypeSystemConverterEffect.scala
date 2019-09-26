package dev.argon.compiler.types

import cats.Monad
import dev.argon.compiler.core.Context

abstract class TypeSystemConverterEffect[F[_]: Monad] extends TypeSystemConverter[F] {
  def convertEffect[A](fa: ts.TSComp[A]): otherTS.TSComp[A]
  def conversionEffectToResultTS[A](fa: F[A]): otherTS.TSComp[A]
}

object TypeSystemConverterEffect {

  type Aux[TContext <: Context with Singleton, TS1 <: TypeSystem[TContext], TS2 <: TypeSystem[TContext], F[_]] =
    TypeSystemConverterEffect[F] {
      val context: TContext
      val ts: TS1
      val otherTS: TS2
    }

}
