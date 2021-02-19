package dev.argon.compiler.core

import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}

trait PayloadSpecVisitor[Container[_[_, _]], +TResult] {
  def visitDeclaration(container: Container[DeclarationPayloadSpecifier]): TResult
  def visitReference(container: Container[ReferencePayloadSpecifier]): TResult
}
