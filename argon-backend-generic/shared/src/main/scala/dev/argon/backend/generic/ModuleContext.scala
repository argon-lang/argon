package dev.argon.backend.generic

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.ResourceIndicator
import shapeless.Id
import cats.implicits._
import dev.argon.compiler.expr.ClassConstructorBody
import dev.argon.compiler.options.CompilerInput
import zio._

sealed abstract class ModuleContext extends ContextWithModule {
  override type TFunctionMetadata = Unit
  override type TMethodMetadata = Unit
  override type TTraitMetadata = Unit
  override type TClassMetadata = Unit
  override type TDataConstructorMetadata = Unit
  override type TClassConstructorMetadata = Unit

  override type TFunctionImplementation = typeSystem.SimpleExpr
  override type TMethodImplementation = Option[typeSystem.SimpleExpr]
  override type TClassConstructorImplementation = ClassConstructorBody[this.type]
  override type TDataConstructorImplementation = typeSystem.SimpleExpr

  override type BackendOptions = GenericBackendOptions[Id, ResIndicator]

  override def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): typeSystem.SimpleExpr = expr
  override def createExprMethodImplementation(expr: typeSystem.SimpleExpr): Option[typeSystem.SimpleExpr] = Some(expr)
  override def abstractMethodImplementation: Option[typeSystem.SimpleExpr] = None
  override def createClassConstructorBodyImplementation(body: ClassConstructorBody[this.type]): ClassConstructorBody[this.type] = body
  override def createDataConstructorImplementation(body: typeSystem.SimpleExpr): typeSystem.SimpleExpr = body


  override def createExternFunctionImplementation(specifier: String, source: DiagnosticSource): Comp[typeSystem.SimpleExpr] =
    Compilation.forErrors(DiagnosticError.UnknownExternImplementation(specifier, source))

  override def createExternMethodImplementation(specifier: String, source: DiagnosticSource): Comp[Option[typeSystem.SimpleExpr]] =
    Compilation.forErrors(DiagnosticError.UnknownExternImplementation(specifier, source))

}

object ModuleContext {
  def apply[I <: ResourceIndicator: Tag](input: CompilerInput[I, GenericBackendOptions[Id, I]]): ModuleContext with Context.WithRes[I] = new ModuleContext {
    override type ResIndicator = I
    override val resIndicatorTag: Tag[I] = implicitly
    override protected val compilerInput: CompilerInput[I, GenericBackendOptions[Id, I]] = input
  }
}

