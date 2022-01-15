package dev.argon.compiler.module

import dev.argon.compiler.*
import dev.argon.parser.IdentifierExpr

trait ArModuleC extends UsingContext {
  val moduleName: ModuleName
  def allExports(): Comp[Seq[ModuleEntry]]
  def exports(name: IdentifierExpr): Comp[Seq[ModuleEntry]]
}
