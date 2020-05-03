package dev.argon.backend.module

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.ResourceIndicator
import shapeless.Id
import cats.implicits._
import zio._

sealed abstract class ModuleContext extends ContextWithModule {
  override type TFunctionMetadata = Unit
  override type TMethodMetadata = Unit
  override type TTraitMetadata = Unit
  override type TClassMetadata = Unit
  override type TDataConstructorMetadata = Unit
  override type TClassConstructorMetadata = Unit

  override type TFunctionImplementation = typeSystem.ArExpr
  override type TMethodImplementation = Option[typeSystem.ArExpr]
  override type TClassConstructorImplementation = typeSystem.ClassConstructorBody
  override type TDataConstructorImplementation = typeSystem.ArExpr

  override type BackendOptions = ModuleBackendOptions[Id, ResIndicator]

  override def createExprFunctionImplementation(expr: typeSystem.ArExpr): typeSystem.ArExpr = expr
  override def createExprMethodImplementation(expr: typeSystem.ArExpr): Option[typeSystem.ArExpr] = Some(expr)
  override def abstractMethodImplementation: Option[typeSystem.ArExpr] = None
  override def createClassConstructorBodyImplementation(body: typeSystem.ClassConstructorBody): typeSystem.ClassConstructorBody = body
  override def createDataConstructorImplementation(body: typeSystem.ArExpr): typeSystem.ArExpr = body


  override def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[typeSystem.ArExpr] =
    Compilation.forErrors(CompilationError.UnknownExternImplementation(specifier, source))

  override def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[Option[typeSystem.ArExpr]] =
    Compilation.forErrors(CompilationError.UnknownExternImplementation(specifier, source))

}

object ModuleContext {
  def apply[I <: ResourceIndicator: Tagged](input: CompilerInput[I, ModuleBackendOptions[Id, I]]): ModuleContext with Context.WithRes[I] = new ModuleContext {
    override type ResIndicator = I
    override val resIndicatorTag: Tagged[I] = implicitly
    override protected val compilerInput: CompilerInput[I, ModuleBackendOptions[Id, I]] = input
  }
}

