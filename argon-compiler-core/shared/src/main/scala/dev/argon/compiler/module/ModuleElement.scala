package dev.argon.compiler.module

import dev.argon.compiler._
import dev.argon.compiler.definitions._

enum ModuleElementC[TContext <: Context] {
  case ClassElement(arClass: ArClassC with HasContext[TContext])
}
