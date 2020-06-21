package dev.argon.backend.generic

import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.armodule.loader.ArgonModuleLoader.PayloadLoader

private[generic] class GenericPayloadLoader[TContext <: ModuleContext] extends PayloadLoader[TContext, ReferencePayloadSpecifier] {

  override def createClassPayload(context: TContext): ReferencePayloadSpecifier[Unit, context.TClassMetadata] = ()

  override def createTraitPayload(context: TContext): ReferencePayloadSpecifier[Unit, context.TTraitMetadata] = ()

  override def createDataConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata] = ()

  override def createFunctionPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TFunctionImplementation], context.TFunctionMetadata] = ()

  override def createMethodPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TMethodImplementation], context.TMethodMetadata] = ()

  override def createClassConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] = ()

}
