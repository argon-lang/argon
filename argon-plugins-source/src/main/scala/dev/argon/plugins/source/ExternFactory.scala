package dev.argon.plugins.source

import dev.argon.compiler.{ErasedSignature, UsingContext, ImportSpecifier}
import zio.ZIO

trait ExternFactory extends UsingContext {
  def getImportSpecifier(sig: ErasedSignature): ImportSpecifier

  def getExternFunctionImplementation(name: String): Comp[Option[context.implementations.ExternFunctionImplementation]]
  
  def defineFunctionReference(sig: ErasedSignature): Comp[context.implementations.FunctionReference]
  def defineRecordReference(sig: ErasedSignature): Comp[context.implementations.RecordReference]
}
