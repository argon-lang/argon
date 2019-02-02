package com.mi3software.argon.compiler.module

import com.mi3software.argon.compiler.Compilation
import com.mi3software.argon.compiler.core.ContextComp
import com.mi3software.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler.loaders.ModuleLoader
import com.mi3software.argon.compiler.loaders.armodule.ArgonModuleLoader

class ModuleContext[TComp[+_] : Compilation] extends ContextComp[TComp] {
  override type TFunctionMetadata = Unit
  override type TMethodMetadata = Unit
  override type TTraitMetadata = Unit
  override type TClassMetadata = Unit
  override type TDataConstructorMetadata = Unit
  override type TClassConstructorMetadata = Unit

  override type TFunctionImplementation = typeSystem.ArExpr
  override type TMethodImplementation = Option[typeSystem.ArExpr]
  override type TClassConstructorImplementation = typeSystem.ClassConstructorBody

  override def createExprFunctionImplementation(expr: typeSystem.ArExpr): typeSystem.ArExpr = expr
  override def createExprMethodImplementation(expr: typeSystem.ArExpr): Option[typeSystem.ArExpr] = Some(expr)
  override def abstractMethodImplementation: Option[typeSystem.ArExpr] = None
  override def createClassConstructorBodyImplementation(body: typeSystem.ClassConstructorBody): typeSystem.ClassConstructorBody = body

  override val compCompilationInstance: Compilation[TComp] = implicitly




  private val referencePayloadLoader: ArgonModuleLoader.PayloadLoader[this.type, ReferencePayloadSpecifier] =
    new ArgonModuleLoader.PayloadLoader[this.type, ReferencePayloadSpecifier] {

      override def createClassPayload(context: ModuleContext.this.type): ReferencePayloadSpecifier[Unit, context.TClassMetadata] = ()

      override def createTraitPayload(context: ModuleContext.this.type): ReferencePayloadSpecifier[Unit, context.TTraitMetadata] = ()

      override def createDataConstructorPayload(context: ModuleContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata] = ()

      override def createFunctionPayload(context: ModuleContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TFunctionImplementation], context.TFunctionMetadata] = ()

      override def createMethodPayload(context: ModuleContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TMethodImplementation], context.TMethodMetadata] = ()

      override def createClassConstructorPayload(context: ModuleContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] = ()
    }

  override val moduleLoaders: Vector[ModuleLoader[this.type]] = Vector(ArgonModuleLoader(this)(referencePayloadLoader))
}
