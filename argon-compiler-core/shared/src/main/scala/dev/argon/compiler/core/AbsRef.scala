package dev.argon.compiler.core

import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}

sealed trait AbsRef[TContext <: Context with Singleton, T[_ <: Context with Singleton, _[_, _]]] {
  type PayloadSpec[_, _]
  val value: T[TContext, PayloadSpec]

  implicit val payloadSpecInfo: PayloadSpecInfo[PayloadSpec]
}

object AbsRef {

  final case class Declaration[TContext <: Context with Singleton, T[_ <: Context with Singleton, _[_, _]]](value: T[TContext, DeclarationPayloadSpecifier]) extends AbsRef[TContext, T] {
    override type PayloadSpec[A, B] = DeclarationPayloadSpecifier[A, B]
    override implicit val payloadSpecInfo: PayloadSpecInfo[DeclarationPayloadSpecifier] = PayloadSpecInfo.declarationPayloadSpecInfo
  }

  final case class Reference[TContext <: Context with Singleton, T[_ <: Context with Singleton, _[_, _]]](value: T[TContext, ReferencePayloadSpecifier]) extends AbsRef[TContext, T] {
    override type PayloadSpec[A, B] = ReferencePayloadSpecifier[A, B]
    override implicit val payloadSpecInfo: PayloadSpecInfo[ReferencePayloadSpecifier] = PayloadSpecInfo.referencePayloadSpecInfo
  }



  def apply[TContext <: Context with Singleton, TPayloadSpec[_, _]: PayloadSpecInfo, T[_ <: Context with Singleton, _[_, _]]](instance: T[TContext, TPayloadSpec]): AbsRef[TContext, T] =
    implicitly[PayloadSpecInfo[TPayloadSpec]].visit(instance)(new PayloadSpecVisitor[T[TContext, *[_, _]], AbsRef[TContext, T]] {
      override def visitDeclaration(container: T[TContext, DeclarationPayloadSpecifier]): AbsRef[TContext, T] =
        Declaration(container)

      override def visitReference(container: T[TContext, ReferencePayloadSpecifier]): AbsRef[TContext, T] =
        Reference(container)
    })

}
