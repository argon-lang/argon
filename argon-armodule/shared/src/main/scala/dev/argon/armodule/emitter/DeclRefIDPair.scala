package dev.argon.armodule.emitter

import dev.argon.compiler.Comp
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}


private trait DeclRefIDPair[TContext <: Context with Singleton, TElem[_ <: Context with Singleton, _[_, _]]] {
  def getIdNum[TPayloadSpec[_, _]: PayloadSpecInfo](elem: TElem[TContext, TPayloadSpec]): Comp[Int]
  def getIdNumForAbsRef(elem: AbsRef[TContext, TElem]): Comp[Int] = {
    import elem.payloadSpecInfo
    getIdNum(elem.value)
  }

  def getByID(id: Int): Comp[AbsRef[TContext, TElem]]
  def getDeclaration(id: Int): Comp[TElem[TContext, DeclarationPayloadSpecifier]]
  def getReference(id: Int): Comp[TElem[TContext, ReferencePayloadSpecifier]]
}