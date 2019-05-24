package dev.argon.compiler.core

import PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.compiler.loaders.source.SourceModuleCreator
import dev.argon.compiler.lookup._
import dev.argon.compiler.types.{ArgonTypeSystem, TypeSystem, TypeSystemConverter}
import cats._
import cats.data.NonEmptyList


sealed trait Context {

  type TFunctionImplementation
  type TMethodImplementation
  type TDataConstructorImplementation
  type TClassConstructorImplementation

  type TFunctionMetadata
  type TMethodMetadata
  type TTraitMetadata
  type TClassMetadata
  type TDataConstructorMetadata
  type TClassConstructorMetadata

  type BackendOptions

  def createExprFunctionImplementation(expr: typeSystem.ArExpr): TFunctionImplementation
  def createExprMethodImplementation(expr: typeSystem.ArExpr): TMethodImplementation
  def abstractMethodImplementation: TMethodImplementation
  def createClassConstructorBodyImplementation(body: typeSystem.ClassConstructorBody): TClassConstructorImplementation
  def createDataConstructorImplementation(body: typeSystem.ArExpr): TDataConstructorImplementation

  def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[TFunctionImplementation]
  def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[TMethodImplementation]

  type CompRE[-_, +_, +_]
  type CompE[+_, +_]
  type Comp[+_]
  implicit val compCompilationInstance: Compilation[Comp]

  object ContextTypeSystem extends ArgonTypeSystem[this.type] {
    override val context: Context.this.type = Context.this
  }

  final lazy val typeSystem: ContextTypeSystem.type = ContextTypeSystem
  final lazy val scopeContext: ScopeContext[this.type] { val typeSystem: Context.this.typeSystem.type } = new ScopeContext[this.type] {
    override val context: Context.this.type = Context.this
    override lazy val typeSystem: Context.this.typeSystem.type = Context.this.typeSystem
  }
  final lazy val signatureContext: SignatureContext[this.type] { val typeSystem: Context.this.typeSystem.type } = new SignatureContext[this.type] {
    override val context: Context.this.type = Context.this
    override lazy val typeSystem: Context.this.typeSystem.type = Context.this.typeSystem
  }

  val moduleLoaders: Vector[ModuleLoader[this.type]]

  type ResIndicator
  protected val compilerInput: CompilerInput[ResIndicator, BackendOptions]

  def createModule[A](f: ArModule[this.type, DeclarationPayloadSpecifier] => Comp[A])(implicit showRes: Show[ResIndicator], res: ResourceAccess[CompRE, ResIndicator]): Comp[A]


}

trait ContextComp[TComp[+_]] extends Context {
  override type Comp[+A] = TComp[A]

}

trait ContextCompE[TCompE[+_, +_]] extends ContextComp[TCompE[NonEmptyList[CompilationError], +?]] {
  override type CompE[+E, +A] = TCompE[E, A]
  override implicit val compCompilationInstance: CompilationE[CompE]
}

trait ContextCompRE[TCompRE[-_, +_, +_]] extends ContextCompE[TCompRE[Any, +?, +?]] {
  override type CompRE[-R, +E, +A] = TCompRE[R, E, A]
  override implicit val compCompilationInstance: CompilationRE[CompRE]

  override def createModule[A](f: ArModule[this.type, DeclarationPayloadSpecifier] => Comp[A])(implicit showRes: Show[ResIndicator], res: ResourceAccess[TCompRE, ResIndicator]): Comp[A] =
    SourceModuleCreator.createModule[CompRE, ResIndicator, A](this)(compilerInput)(f)(compCompilationInstance, showRes, res)
}
