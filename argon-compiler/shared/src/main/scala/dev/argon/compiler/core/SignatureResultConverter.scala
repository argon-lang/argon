package dev.argon.compiler.core

import dev.argon.compiler.types._
import cats.Monad
import dev.argon.compiler.Comp

trait SignatureResultConverter[TResult[TContext <: Context with Singleton, _ <: TypeSystem[TContext] with Singleton]] {
  def convertTypeSystem
  (context: Context)
  (ts1: TypeSystem[context.type])
  (ts2: TypeSystem[context.type])
  (converter: TypeSystemConverter.Aux[context.type, ts1.type, ts2.type])
  (result: TResult[context.type, ts1.type])
  : Comp[TResult[context.type, ts2.type]]

  def referencesParameter
  (signatureContext: SignatureContext)
  (refChecker: signatureContext.RefChecker)
  (result: TResult[signatureContext.context.type, signatureContext.typeSystem.type])
  : Comp[Boolean]

  def substitute
  (signatureContext: SignatureContext)
  (subst: signatureContext.Subst)
  (result: TResult[signatureContext.context.type, signatureContext.typeSystem.type])
  : TResult[signatureContext.context.type, signatureContext.typeSystem.type]
}
