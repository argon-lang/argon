package com.mi3software.argon.compiler

import com.mi3software.argon.parser.SourceAST
import com.mi3software.argon.util.Compilation
import scalaz.Monad

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

  val invalidTraitMetadata: TTraitMetadata
  val invalidClassMetadata: TClassMetadata

  type Comp[+_]
  implicit val compMonadInstance: Monad[Comp]
  implicit val compCompilationInstance: Compilation[Comp]

  val typeSystem: ArgonTypeSystem[this.type]
  val moduleLoaders: Vector[ModuleLoader]

  def createModule(source: Vector[SourceAST]): ArModule[this.type]


  sealed trait ContextScopeTypes extends ScopeTypes {
    override type TTrait <: ArTrait[Context.this.type]
    override type TClass <: ArClass[Context.this.type]
    override type TDataConstructor <: DataConstructor[Context.this.type]
    override type TFunc <: ArFunc[Context.this.type]
    override type TVariable <: Variable[typeSystem.type, VariableLikeDescriptor]
  }

  sealed trait ScopeTypesWithPayload[TPayloadSpec[_, _]] extends ContextScopeTypes {
    override type TTrait = ArTraitWithPayload[Context.this.type, TPayloadSpec]
    override type TClass = ArClassWithPayload[Context.this.type, TPayloadSpec]
    override type TDataConstructor = DataConstructorWithPayload[Context.this.type, TPayloadSpec]
    override type TFunc = ArFuncWithPayload[Context.this.type, TPayloadSpec]
    override type TVariable = Nothing
  }

}
