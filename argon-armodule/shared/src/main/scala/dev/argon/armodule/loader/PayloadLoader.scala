package dev.argon.armodule.loader

import dev.argon.compiler.Comp
import dev.argon.compiler.core.Context

trait PayloadLoader[TContext <: Context, TPayloadSpec[_, _]] {

  def createDataConstructorPayload(context: TContext): TPayloadSpec[Comp[context.TDataConstructorImplementation], Unit]
  def createFunctionPayload(context: TContext): TPayloadSpec[Comp[context.TFunctionImplementation], Unit]
  def createMethodPayload(context: TContext): TPayloadSpec[Comp[context.TMethodImplementation], Unit]
  def createClassConstructorPayload(context: TContext): TPayloadSpec[Comp[context.TClassConstructorImplementation], Unit]

}