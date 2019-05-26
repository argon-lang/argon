package dev.argon.compiler.module

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.compiler.loaders.armodule.ArgonModuleLoader
import cats._
import cats.implicits._

class ModuleContext[TCompRE[-_, +_, +_], R, I]
(
  override protected val compilerInput: CompilerInput[I, ModuleBackendOptions[Id, I]]
)(implicit
  override implicit val compCompilationInstance: CompilationRE[TCompRE, R]
) extends ContextCompRE[TCompRE, R] {
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


  override type BackendOptions = ModuleBackendOptions[Id, I]

  override def createExprFunctionImplementation(expr: typeSystem.ArExpr): typeSystem.ArExpr = expr
  override def createExprMethodImplementation(expr: typeSystem.ArExpr): Option[typeSystem.ArExpr] = Some(expr)
  override def abstractMethodImplementation: Option[typeSystem.ArExpr] = None
  override def createClassConstructorBodyImplementation(body: typeSystem.ClassConstructorBody): typeSystem.ClassConstructorBody = body
  override def createDataConstructorImplementation(body: typeSystem.ArExpr): typeSystem.ArExpr = body


  override def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[typeSystem.ArExpr] =
    Compilation[Comp].forErrors(CompilationError.UnknownExternImplementation(specifier, source))

  override def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[Option[typeSystem.ArExpr]] =
    Compilation[Comp].forErrors(CompilationError.UnknownExternImplementation(specifier, source))



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
  override type ResIndicator = I
}
