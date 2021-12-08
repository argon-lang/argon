package dev.argon.compiler.module

import dev.argon.compiler._
import dev.argon.parser.IdentifierExpr

trait ArModuleC extends UsingContext {
  val moduleName: ModuleName
  def imports(name: IdentifierExpr): Comp[Seq[ModuleEntry]]
}