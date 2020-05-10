package dev.argon.backend.js

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.{ModuleLoader, ResourceIndicator}
import dev.argon.compiler._
import dev.argon.compiler.expr._
import shapeless.Id
import zio._

sealed abstract class JSContext extends ContextWithModule {

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

  override type BackendOptions = JSBackendOptions[Id, ResIndicator]

  override def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): JSImpl.Function =
    JSImpl.Function.ExpressionBody(expr)

  override def createExprMethodImplementation(expr: typeSystem.SimpleExpr): JSImpl.Method =
    JSImpl.Method.ExpressionBody(expr)

  override def abstractMethodImplementation: JSImpl.Method = JSImpl.Method.Abstract

  override def createClassConstructorBodyImplementation(body: ClassConstructorBody[this.type]): JSImpl.ClassConstructor =
    JSImpl.ClassConstructor.StatementBody(body)

  override def createDataConstructorImplementation(body: typeSystem.SimpleExpr): JSImpl.DataConstructor =
    JSImpl.DataConstructor.ExpressionBody(body)

  override def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[JSImpl.Function] =
    compilerInput.backendOptions.extern.get(specifier) match {
      case Some(impl) => IO.succeed(JSImpl.Function.JSExpressionBody(JSExpressionRaw(impl)))
      case None => Compilation.forErrors(CompilationError.UnknownExternImplementation(specifier, source))
    }

  override def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[JSImpl.Method] =
    compilerInput.backendOptions.extern.get(specifier) match {
      case Some(impl) => IO.succeed(JSImpl.Method.JSExpressionBody(JSExpressionRaw(impl)))
      case None => Compilation.forErrors(CompilationError.UnknownExternImplementation(specifier, source))
    }

  object JSImpl {
    import typeSystem._

    sealed trait Function
    object Function {
      final case class JSExpressionBody(expr: JSExpression) extends Function
      final case class ExpressionBody(expr: SimpleExpr) extends Function
    }

    sealed trait Method
    object Method {
      case object Abstract extends Method
      final case class JSExpressionBody(expr: JSExpression) extends Method
      final case class ExpressionBody(expr: SimpleExpr) extends Method
    }

    sealed trait ClassConstructor
    object ClassConstructor {
      final case class StatementBody(body: ClassConstructorBody[JSContext.this.type]) extends ClassConstructor
    }

    sealed trait DataConstructor
    object DataConstructor {
      final case class ExpressionBody(expr: SimpleExpr) extends DataConstructor
    }
  }

}

object JSContext {
  def apply[I <: ResourceIndicator: Tagged](input: CompilerInput[I, JSBackendOptions[Id, I]]): JSContext with Context.WithRes[I] = new JSContext {
    type ResIndicator = I

    override protected val compilerInput: CompilerInput[I, JSBackendOptions[Id, I]] = input
    override val resIndicatorTag: Tagged[I] = implicitly
  }
}
