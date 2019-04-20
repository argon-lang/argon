package dev.argon.compiler.core

import PayloadSpecifiers._
import dev.argon.compiler._
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.compiler.loaders.source.SourceModuleCreator
import dev.argon.compiler.lookup._
import dev.argon.compiler.types.{ArgonTypeSystem, TypeSystem, TypeSystemConverter}
import scalaz.Show

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

  def createExternFunctionImplementation(specifier: String, source: CompilationMessageSource): Comp[TFunctionImplementation]
  def createExternMethodImplementation(specifier: String, source: CompilationMessageSource): Comp[TMethodImplementation]

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

  def createModule[A](f: ArModule[this.type, DeclarationPayloadSpecifier] => Comp[A])(implicit showRes: Show[ResIndicator], res: ResourceAccess[Comp, ResIndicator]): Comp[A]


}

trait ContextComp[TComp[+_]] extends Context {
  override type Comp[+A] = TComp[A]


  override def createModule[A](f: ArModule[ContextComp.this.type, DeclarationPayloadSpecifier] => TComp[A])(implicit showRes: Show[ResIndicator], res: ResourceAccess[TComp, ResIndicator]): TComp[A] =
    SourceModuleCreator.createModule[Comp, ResIndicator, A](this)(compilerInput)(f)(compCompilationInstance, implicitly, res)

}
