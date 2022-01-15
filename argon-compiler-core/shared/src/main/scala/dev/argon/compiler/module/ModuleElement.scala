package dev.argon.compiler.module

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*

enum ModuleElementC[TContext <: Context] {
  case ClassElement(arClass: ArClassC with HasContext[TContext])
  case TraitElement(arTrait: ArTraitC with HasContext[TContext])
  case FunctionElement(func: ArFuncC with HasContext[TContext])
}
