package dev.argon.backend.generic

import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.armodule.loader.PayloadLoader

private[generic] class GenericPayloadLoader[TContext <: ModuleContext] extends PayloadLoader[TContext, ReferencePayloadSpecifier] {

  override def createDataConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TDataConstructorImplementation], Unit] = ()

  override def createFunctionPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TFunctionImplementation], Unit] = ()

  override def createMethodPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TMethodImplementation], Unit] = ()

  override def createClassConstructorPayload(context: TContext): ReferencePayloadSpecifier[Comp[context.TClassConstructorImplementation], Unit] = ()

}
