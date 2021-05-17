package dev.argon.compiler.core

import dev.argon.compiler.types._
import dev.argon.compiler.Comp

trait SignatureResultConverter[TResult[TContext2 <: Context with Singleton, Wrap[+_]]] {
  def convertTypeSystem[Wrap1[+_], Wrap2[+_]]
  (context: Context)
  (converter: TypeSystemConverter.Aux[context.type, Wrap1, Wrap2])
  (result: TResult[context.type, Wrap1])
  : Comp[TResult[context.type, Wrap2]]

  def referencesParameter
  (signatureContext: SignatureContext)
  (refChecker: signatureContext.RefChecker)
  (result: TResult[signatureContext.context.type, signatureContext.TTypeWrapper])
  : Comp[Boolean]

  def substitute
  (signatureContext: SignatureContext)
  (subst: signatureContext.Subst)
  (result: TResult[signatureContext.context.type, signatureContext.TTypeWrapper])
  : TResult[signatureContext.context.type, signatureContext.TTypeWrapper]
}
