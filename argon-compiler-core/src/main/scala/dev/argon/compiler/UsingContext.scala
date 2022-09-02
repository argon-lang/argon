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

  protected type OwnedByModule = OwnedByModuleC[context.type]
  protected type OwnedByClass[+ClassOwner] = OwnedByClassC[context.type, ClassOwner]
  protected type OwnedByClassStatic[+ClassOwner] = OwnedByClassStaticC[context.type, ClassOwner]
  protected type OwnedByTrait[+TraitOwner] = OwnedByTraitC[context.type, TraitOwner]
  protected type OwnedByTraitStatic[+TraitOwner] = OwnedByTraitStaticC[context.type, TraitOwner]

}
