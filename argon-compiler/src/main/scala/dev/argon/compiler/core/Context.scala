package dev.argon.compiler.core

import PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.compiler.loaders.source.SourceModuleCreator
import dev.argon.compiler.lookup._
import dev.argon.compiler.types.{ArgonTypeSystem, TypeSystem, TypeSystemConverter}
import cats._
import cats.data.NonEmptyList
import cats.evidence.{===, Is}


trait Context {

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

  type Environment
  type CompRE[-_, +_, +_]
  type CompE[+E, +A] = CompRE[Environment, E, A]
  type Comp[+A] = CompE[NonEmptyList[CompilationError], A]
  implicit val compCompilationInstance: CompilationRE[CompRE, Environment]

  object ContextTypeSystem extends ArgonTypeSystem[this.type] {
    override val context: Context.this.type = Context.this
    override val contextProof: Context.this.type === this.context.type = Is.refl
  }

  final lazy val typeSystem: ContextTypeSystem.type = ContextTypeSystem
  final lazy val scopeContext: ScopeContext[this.type] { val typeSystem: Context.this.typeSystem.type } = new ScopeContext[this.type] {
    override val context: Context.this.type = Context.this
    override lazy val typeSystem: Context.this.typeSystem.type = Context.this.typeSystem
  }
  final lazy val signatureContext: SignatureContext {
    val context: Context.this.type
    val typeSystem: Context.this.typeSystem.type
  } = new SignatureContext {
    override val context: Context.this.type = Context.this
    override lazy val typeSystem: Context.this.typeSystem.type = Context.this.typeSystem
  }

  val moduleLoaders: Vector[ModuleLoader[this.type]]

  type ResIndicator
  protected val compilerInput: CompilerInput[ResIndicator, BackendOptions]

  def createModule[A](f: ArModule[this.type, DeclarationPayloadSpecifier] => Comp[A])(implicit showRes: Show[ResIndicator], res: ResourceAccess[this.type]): Comp[A] =
    SourceModuleCreator.createModule[A](this)(compilerInput)(f)

}
