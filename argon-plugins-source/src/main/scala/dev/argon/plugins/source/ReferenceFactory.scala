package dev.argon.plugins.source

import dev.argon.compiler.{ErasedSignature, UsingContext}
import zio.ZIO

trait ReferenceFactory extends UsingContext {
  def defineFunctionReference(sig: ErasedSignature): Comp[context.implementations.FunctionReference]
}
