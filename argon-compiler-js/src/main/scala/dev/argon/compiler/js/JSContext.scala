package dev.argon.compiler.js

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.compiler.loaders.armodule.ArgonModuleLoader
import dev.argon.compiler.types.ArgonTypeSystem
import dev.argon.compiler._
import cats._
import cats.implicits._

final class JSContext[TCompE[+_, +_] : CompilationE, I](override protected val compilerInput: CompilerInput[I, JSBackendOptions[Id, I]]) extends ContextCompE[TCompE] {

  override type TTraitMetadata = Unit
  override type TClassMetadata = Unit
  override type TFunctionMetadata = Unit
  override type TMethodMetadata = Unit
  override type TDataConstructorMetadata = Unit
  override type TClassConstructorMetadata = Unit

  override type TFunctionImplementation = JSImpl.Function
  override type TMethodImplementation = JSImpl.Method
  override type TClassConstructorImplementation = JSImpl.ClassConstructor
  override type TDataConstructorImplementation = JSImpl.DataConstructor

  override type BackendOptions = JSBackendOptions[Id, I]

  override def createExprFunctionImplementation(expr: typeSystem.ArExpr): JSImpl.Function =
    JSImpl.Function.ExpressionBody(expr)

  override def createExprMethodImplementation(expr: typeSystem.ArExpr): JSImpl.Method =
    JSImpl.Method.ExpressionBody(expr)

  override def abstractMethodImplementation: JSImpl.Method = JSImpl.Method.Abstract

  override def createClassConstructorBodyImplementation(body: typeSystem.ClassConstructorBody): JSImpl.ClassConstructor =
    JSImpl.ClassConstructor.StatementBody(body)

  override def createDataConstructorImplementation(body: typeSystem.ArExpr): JSImpl.DataConstructor =
    JSImpl.DataConstructor.ExpressionBody(body)

  override def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[JSImpl.Function] =
    compilerInput.backendOptions.extern.get(specifier) match {
      case Some(impl) => JSImpl.Function.JSExpressionBody(JSExpressionRaw(impl)).pure[Comp]
      case None => Compilation[Comp].forErrors(CompilationError.UnknownExternImplementation(specifier, source))
    }

  override def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[JSImpl.Method] =
    compilerInput.backendOptions.extern.get(specifier) match {
      case Some(impl) => JSImpl.Method.JSExpressionBody(JSExpressionRaw(impl)).pure[Comp]
      case None => Compilation[Comp].forErrors(CompilationError.UnknownExternImplementation(specifier, source))
    }

  override val compECompilationInstance: CompilationE[CompE] = implicitly



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


  override type ResIndicator = I



  object JSImpl {
    import typeSystem._

    sealed trait Function
    object Function {
      final case class JSExpressionBody(expr: JSExpression) extends Function
      final case class ExpressionBody(expr: ArExpr) extends Function
    }

    sealed trait Method
    object Method {
      case object Abstract extends Method
      final case class JSExpressionBody(expr: JSExpression) extends Method
      final case class ExpressionBody(expr: ArExpr) extends Method
    }

    sealed trait ClassConstructor
    object ClassConstructor {
      final case class StatementBody(body: ClassConstructorBody) extends ClassConstructor
    }

    sealed trait DataConstructor
    object DataConstructor {
      final case class ExpressionBody(expr: ArExpr) extends DataConstructor
    }
  }

}
