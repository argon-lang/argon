package dev.argon.compiler

import dev.argon.compiler.module._
import dev.argon.compiler.pkg._
import dev.argon.compiler.definitions._

trait UsingContext {
  val context: Context
  protected type Comp[+A] = context.Comp[A]

  protected type ArPackage = ArPackageC with HasContext[context.type]
  protected type ArModule = ArModuleC with HasContext[context.type]
  protected type ModuleElement = ModuleElementC[context.type]
  protected type ModuleEntry = ModuleEntryC[context.type]
  protected type ArClass = ArClassC with HasContext[context.type]
  protected type ArTrait = ArTraitC with HasContext[context.type]
  protected type DataConstructor = DataConstructorC with HasContext[context.type]
  protected type ArFunc = ArFuncC with HasContext[context.type]
  protected type ArMethod = ArMethodC with HasContext[context.type]
  protected type ClassConstructor = ClassConstructorC with HasContext[context.type]
}
