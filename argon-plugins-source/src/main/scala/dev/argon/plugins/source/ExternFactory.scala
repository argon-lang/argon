package dev.argon.plugins.source

import dev.argon.compiler.{ErasedSignature, UsingContext}
import zio.ZIO

trait ExternFactory extends UsingContext {
  def getExternFunctionImplementation(name: String): Comp[Option[context.implementations.ExternFunctionImplementation]]
  
  def defineFunctionReference(sig: ErasedSignature): Comp[context.implementations.FunctionReference]
}
