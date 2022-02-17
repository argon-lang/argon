package dev.argon.compiler

import dev.argon.compiler.module.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.definitions.*

trait UsingContext {
  val context: Context
  protected type Comp[+A] = context.Comp[A]

  protected type ArTube = ArTubeC with HasContext[context.type]
  protected type ArModule = ArModuleC with HasContext[context.type]
  protected type ModuleElement = ModuleElementC[context.type]
  protected type ArClass = ArClassC with HasContext[context.type]
  protected type ArTrait = ArTraitC with HasContext[context.type]
  protected type ArFunc = ArFuncC with HasContext[context.type]
  protected type ArMethod = ArMethodC with HasContext[context.type]
  protected type ClassConstructor = ClassConstructorC with HasContext[context.type]


  protected type OwnedByModule = OwnedByModuleC[context.type]
  protected type OwnedByClass[+ClassOwner] = OwnedByClassC[context.type, ClassOwner]
  protected type OwnedByClassStatic[+ClassOwner] = OwnedByClassStaticC[context.type, ClassOwner]
  protected type OwnedByTrait[+TraitOwner] = OwnedByTraitC[context.type, TraitOwner]
  protected type OwnedByTraitStatic[+TraitOwner] = OwnedByTraitStaticC[context.type, TraitOwner]
  
}
