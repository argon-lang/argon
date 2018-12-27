package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.PayloadSpecifiers._
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.loaders.ModuleLoader
import com.mi3software.argon.compiler.loaders.source.SourceModuleCreator
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.compiler.types.ArgonTypeSystem
import scalaz.effect.IO

sealed trait Context extends VariableContext with ArExprContext with SignatureContext with ScopeContext {

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

  val invalidTraitMetadata: TTraitMetadata
  val invalidClassMetadata: TClassMetadata

  type Comp[+_]
  implicit val compCompilationInstance: Compilation[Comp]

  object ContextTypeSystem extends ArgonTypeSystem {
    override val context: Context.this.type = Context.this

    override def fromArType(arType: Context.this.typeSystem.TType): TType = arType
  }

  final val typeSystem: ContextTypeSystem.type = ContextTypeSystem

  val moduleLoaders: Vector[ModuleLoader]

  def createModule(input: CompilerInput): IO[Comp[ArModule[this.type, DeclarationPayloadSpecifier]]]


}

trait ContextComp[TComp[+_]] extends Context {
  override type Comp[+A] = TComp[A]

  override def createModule(input: CompilerInput): IO[TComp[ArModule[this.type, DeclarationPayloadSpecifier]]] =
    SourceModuleCreator.createModule[Comp](this)(input)(compCompilationInstance)
}
