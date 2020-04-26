package dev.argon.backend.js

import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader

class JSPayloadLoader extends PayloadLoader[JSContext, ReferencePayloadSpecifier] {

  override def createClassPayload(context: JSContext): ReferencePayloadSpecifier[Unit, context.TClassMetadata] = ()

  override def createTraitPayload(context: JSContext): ReferencePayloadSpecifier[Unit, context.TTraitMetadata] = ()

  override def createDataConstructorPayload(context: JSContext): ReferencePayloadSpecifier[Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata] = ()

  override def createFunctionPayload(context: JSContext): ReferencePayloadSpecifier[Comp[context.TFunctionImplementation], context.TFunctionMetadata] = ()

  override def createMethodPayload(context: JSContext): ReferencePayloadSpecifier[Comp[context.TMethodImplementation], context.TMethodMetadata] = ()

  override def createClassConstructorPayload(context: JSContext): ReferencePayloadSpecifier[Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] = ()
}
