package dev.argon.compiler

import dev.argon.compiler.module.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.definitions.*

trait UsingContext {
  val context: Context
  protected type Comp[+A] = context.Comp[A]

  protected type ArTube = ArTubeC & HasContext[context.type]
  protected type ArModule = ArModuleC & HasContext[context.type]
  protected type ModuleElement[IsImplementation <: Boolean] = ModuleElementC[context.type, IsImplementation]
  protected type ArClass = ArClassC & HasContext[context.type]
  protected type ArTrait = ArTraitC & HasContext[context.type]
  protected type ArFunc = ArFuncC & HasContext[context.type]
  protected type ArMethod = ArMethodC & HasContext[context.type]
  protected type ClassConstructor = ClassConstructorC & HasContext[context.type]

  protected type MethodImplementation = MethodImplementationC & HasContext[context.type]
  protected type FunctionImplementation = FunctionImplementationC & HasContext[context.type]
  protected type ClassConstructorImplementation = ClassConstructorImplementationC & HasContext[context.type]

  protected type OwnedByModule[IsImplementation <: Boolean] = OwnedByModuleC[context.type, IsImplementation]
  protected type OwnedByClass[+ClassOwner, IsImplementation <: Boolean] = OwnedByClassC[context.type, ClassOwner, IsImplementation]
  protected type OwnedByClassStatic[+ClassOwner, IsImplementation <: Boolean] = OwnedByClassStaticC[context.type, ClassOwner, IsImplementation]
  protected type OwnedByTrait[+TraitOwner, IsImplementation <: Boolean] = OwnedByTraitC[context.type, TraitOwner, IsImplementation]
  protected type OwnedByTraitStatic[+TraitOwner, IsImplementation <: Boolean] = OwnedByTraitStaticC[context.type, TraitOwner, IsImplementation]

}
