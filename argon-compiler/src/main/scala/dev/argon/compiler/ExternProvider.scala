package dev.argon.compiler

import dev.argon.compiler.UsingContext

trait ExternProvider extends UsingContext {
  def getTubeMetadata: Comp[context.implementations.TubeMetadata]
  def getExternFunction(name: String): Comp[Option[context.implementations.ExternFunction]]
  def getExternMethod(name: String): Comp[Option[context.implementations.ExternMethod]]
}
