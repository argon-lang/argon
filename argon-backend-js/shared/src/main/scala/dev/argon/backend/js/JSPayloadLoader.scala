package dev.argon.backend.js

import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.armodule.loader.PayloadLoader

class JSPayloadLoader[TContext <: JSContext] extends PayloadLoader[TContext, ReferencePayloadSpecifier] {

  override def createClassPayload(context: TContext): ReferencePayloadSpecifier[Unit, context.TClassMetadata] = ()

  override def createTraitPayload(context: TContext): ReferencePayloadSpecifier[Unit, context.TTraitMetadata] = ()

  override def createDataConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata] = ()

  override def createFunctionPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TFunctionImplementation], context.TFunctionMetadata] = ()

  override def createMethodPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TMethodImplementation], context.TMethodMetadata] = ()

  override def createClassConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] = ()
}
