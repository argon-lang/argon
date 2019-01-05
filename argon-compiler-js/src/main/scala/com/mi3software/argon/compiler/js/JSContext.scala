package com.mi3software.argon.compiler.js

import com.mi3software.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler.core.{ContextComp, PayloadSpecifiers}
import com.mi3software.argon.compiler.loaders.ModuleLoader
import com.mi3software.argon.compiler.loaders.armodule.ArgonModuleLoader
import com.mi3software.argon.compiler.types.ArgonTypeSystem
import com.mi3software.argon.compiler.{Compilation, _}

final class JSContext[TComp[+_] : Compilation] extends ContextComp[TComp] {

  override type TTraitMetadata = Unit
  override type TClassMetadata = Unit
  override type TFunctionMetadata = Unit
  override type TMethodMetadata = Unit
  override type TDataConstructorMetadata = Unit
  override type TClassConstructorMetadata = Unit

  override type TFunctionImplementation = JSImpl.Function

  override def createExprFunctionImplementation(expr: typeSystem.ArExpr): JSImpl.Function =
    JSImpl.Function.ExpressionBody(expr)

  override type Comp[+T] = TComp[T]

  override val compCompilationInstance: Compilation[Comp] = implicitly



  private val referencePayloadLoader: ArgonModuleLoader.PayloadLoader[this.type, ReferencePayloadSpecifier] =
    new ArgonModuleLoader.PayloadLoader[this.type, ReferencePayloadSpecifier] {

      override def createClassPayload(context: JSContext.this.type): ReferencePayloadSpecifier[Unit, context.TClassMetadata] = ()

      override def createTraitPayload(context: JSContext.this.type): ReferencePayloadSpecifier[Unit, context.TTraitMetadata] = ()

      override def createDataConstructorPayload(context: JSContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TDataConstructorImplementation], context.TDataConstructorMetadata] = ()

      override def createFunctionPayload(context: JSContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TFunctionImplementation], context.TFunctionMetadata] = ()

      override def createMethodPayload(context: JSContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TMethodImplementation], context.TMethodMetadata] = ()

      override def createClassConstructorPayload(context: JSContext.this.type): ReferencePayloadSpecifier[context.Comp[context.TClassConstructorImplementation], context.TClassConstructorMetadata] = ()
    }

  override val moduleLoaders: Vector[ModuleLoader[this.type]] = Vector(ArgonModuleLoader(this)(referencePayloadLoader))


  object JSImpl {
    import typeSystem._

    sealed trait Function
    object Function {
      final case class ExpressionBody(expr: ArExpr) extends Function
    }
  }

}
