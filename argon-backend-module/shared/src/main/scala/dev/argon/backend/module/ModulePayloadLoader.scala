package dev.argon.backend.module

import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader

class ModulePayloadLoader extends PayloadLoader[ModuleContext, ReferencePayloadSpecifier] {

  override def createClassPayload(context: ModuleContext): ReferencePayloadSpecifier[Unit, context.TClassMetadata] = ()

  override def createTraitPayload(context: ModuleContext): ReferencePayloadSpecifier[Unit, context.TTraitMetadata] = ()

  override def createDataConstructorPayload(context: ModuleContext): ReferencePayloadSpecifier[Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata] = ()

  override def createFunctionPayload(context: ModuleContext): ReferencePayloadSpecifier[Comp[context.TFunctionImplementation], context.TFunctionMetadata] = ()

  override def createMethodPayload(context: ModuleContext): ReferencePayloadSpecifier[Comp[context.TMethodImplementation], context.TMethodMetadata] = ()

  override def createClassConstructorPayload(context: ModuleContext): ReferencePayloadSpecifier[Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] = ()

}
