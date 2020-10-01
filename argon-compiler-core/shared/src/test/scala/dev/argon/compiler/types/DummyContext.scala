package dev.argon.compiler.types

import dev.argon.compiler.{Comp, DiagnosticSource, CompilationError}
import dev.argon.compiler.core.{ArModule, Context}
import dev.argon.compiler.core.Context.WithRes
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.expr.ClassConstructorBody
import dev.argon.compiler.loaders.ModuleLoad
import dev.argon.compiler.options.{CompilerInput, CompilerOptions, FileList}
import shapeless.Id
import zio._
import zio.stream._

class DummyContext extends Context {
  override type TFunctionImplementation = Unit
  override type TMethodImplementation = Unit
  override type TDataConstructorImplementation = Unit
  override type TClassConstructorImplementation = Unit
  override type TFunctionMetadata = Unit
  override type TMethodMetadata = Unit
  override type TTraitMetadata = Unit
  override type TClassMetadata = Unit
  override type TDataConstructorMetadata = Unit
  override type TClassConstructorMetadata = Unit
  override type BackendOptions = Unit

  override def createExprFunctionImplementation(expr: typeSystem.SimpleExpr): Unit = ()

  override def createExprMethodImplementation(expr: typeSystem.SimpleExpr): Unit = ()

  override def abstractMethodImplementation: Unit = ()

  override def createClassConstructorBodyImplementation(body: ClassConstructorBody[DummyContext.this.type]): Unit = ()

  override def createDataConstructorImplementation(body: typeSystem.SimpleExpr): Unit = ()

  override def createExternFunctionImplementation(specifier: String, source: DiagnosticSource): Comp[Unit] = IO.unit

  override def createExternMethodImplementation(specifier: String, source: DiagnosticSource): Comp[Unit] = IO.unit


  override type ResIndicator = Nothing
  override val resIndicatorTag: Tag[ResIndicator] = implicitly


  override protected val compilerInput: CompilerInput[ResIndicator, Unit] = CompilerInput[ResIndicator, Unit](
    options = CompilerOptions[Id, ResIndicator](
      moduleName = "DummyModule",
      inputFiles = new FileList(List.empty),
      references = new FileList(List.empty),
    ),
    backendOptions = (),
  )

  override def module[TContext >: DummyContext.this.type <: WithRes[ResIndicator] : Tag]: ZManaged[ModuleLoad[ResIndicator, TContext], CompilationError, ArModule[DummyContext.this.type, DeclarationPayloadSpecifier]] =
    Managed.die(new UnsupportedOperationException)
}
