package dev.argon.armodule.loader

import dev.argon.compiler.Comp
import dev.argon.compiler.core.Context

trait PayloadLoader[TContext <: Context, TPayloadSpec[_, _]] {

  def createClassPayload(context: TContext): TPayloadSpec[Unit, context.TClassMetadata]
  def createTraitPayload(context: TContext): TPayloadSpec[Unit, context.TTraitMetadata]
  def createDataConstructorPayload(context: TContext): TPayloadSpec[Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata]
  def createFunctionPayload(context: TContext): TPayloadSpec[Comp[context.TFunctionImplementation], context.TFunctionMetadata]
  def createMethodPayload(context: TContext): TPayloadSpec[Comp[context.TMethodImplementation], context.TMethodMetadata]
  def createClassConstructorPayload(context: TContext): TPayloadSpec[Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata]

}