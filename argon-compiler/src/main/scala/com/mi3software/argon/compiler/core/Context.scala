package com.mi3software.argon.compiler.core

import PayloadSpecifiers._
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.loaders.ModuleLoader
import com.mi3software.argon.compiler.loaders.source.SourceModuleCreator
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.compiler.types.{ArgonTypeSystem, TypeSystem, TypeSystemConverter}
import scalaz.effect.IO

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

  def createExprFunctionImplementation(expr: typeSystem.ArExpr): TFunctionImplementation

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

  def createModule(input: CompilerInput): IO[Comp[ArModule[this.type, DeclarationPayloadSpecifier]]]


}

trait ContextComp[TComp[+_]] extends Context {
  override type Comp[+A] = TComp[A]

  override def createModule(input: CompilerInput): IO[TComp[ArModule[this.type, DeclarationPayloadSpecifier]]] =
    SourceModuleCreator.createModule[Comp](this)(input)(compCompilationInstance)
}
