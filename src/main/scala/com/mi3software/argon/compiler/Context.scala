package com.mi3software.argon.compiler

import com.mi3software.argon.util.Compilation
import scalaz.Monad

trait Context {

  type TFunctionImplementation
  type TMethodImplementation
  type TConstructorImplementation
  type TClassConstructorImplementation

  type TFunctionMetadata
  type TMethodMetadata
  type TTraitMetadata
  type TClassMetadata
  type TConstructorMetadata
  type TClassConstructorMetadata

  type Comp[+_]
  implicit val compMonadInstance: Monad[Comp]
  implicit val compCompilationInstance: Compilation[Comp]

  val typeSystem: TypeSystem

  sealed trait ContextScopeTypes extends ScopeTypes {
    override type TTrait <: ArTrait[Context.this.type]
    override type TClass <: ArClass[Context.this.type]
    override type TDataConstructor <: DataConstructor[Context.this.type]
    override type TFunc <: ArFunc[Context.this.type]
    override type TVariable <: Variable[typeSystem.type, VariableLikeDescriptor]
  }

  sealed trait ReferenceScopeTypes extends ContextScopeTypes {
    override type TTrait = ArTraitReference[Context.this.type]
    override type TClass = ArClassReference[Context.this.type]
    override type TDataConstructor = DataConstructorReference[Context.this.type]
    override type TFunc = ArFuncReference[Context.this.type]
    override type TVariable = Nothing
  }

  sealed trait DeclarationScopeTypes extends ContextScopeTypes {
    override type TTrait = ArTraitDeclaration[Context.this.type]
    override type TClass = ArClassDeclaration[Context.this.type]
    override type TDataConstructor = DataConstructorDeclaration[Context.this.type]
    override type TFunc = ArFuncDeclaration[Context.this.type]
    override type TVariable = Nothing
  }

}
