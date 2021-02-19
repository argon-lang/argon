package dev.argon.compiler.core

import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}

trait PayloadSpecInfo[TPayloadSpec[_, _]] {
  def visit[Container[_[_, _]], TResult](container: Container[TPayloadSpec])(visitor: PayloadSpecVisitor[Container, TResult]): TResult
}

object PayloadSpecInfo {
  implicit val declarationPayloadSpecInfo: PayloadSpecInfo[DeclarationPayloadSpecifier] = new PayloadSpecInfo[DeclarationPayloadSpecifier] {
    override def visit[Container[_[_, _]], TResult](container: Container[DeclarationPayloadSpecifier])(visitor: PayloadSpecVisitor[Container, TResult]): TResult =
      visitor.visitDeclaration(container)
  }

  implicit val referencePayloadSpecInfo: PayloadSpecInfo[ReferencePayloadSpecifier] = new PayloadSpecInfo[ReferencePayloadSpecifier] {
    override def visit[Container[_[_, _]], TResult](container: Container[ReferencePayloadSpecifier])(visitor: PayloadSpecVisitor[Container, TResult]): TResult =
      visitor.visitReference(container)
  }
}
