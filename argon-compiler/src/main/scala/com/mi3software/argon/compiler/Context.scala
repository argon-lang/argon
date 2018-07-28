package com.mi3software.argon.compiler

import com.mi3software.argon.Compilation
import scalaz.effect.IO

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
  implicit val compCompilationInstance: Compilation[Comp]

  val withCompType: this.type with ContextComp[Comp]

  val typeSystem: ArgonTypeSystem[this.type]
  val moduleLoaders: Vector[ModuleLoader]


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


  final def createModule(input: CompilerInput): IO[Comp[ArModule[this.type]]] =
    SourceModuleCreator.createModule[Comp](this.withCompType)(input)(compCompilationInstance)
      .map(identity)

}
