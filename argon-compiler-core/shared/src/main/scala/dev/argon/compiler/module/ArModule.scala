package dev.argon.compiler.module

import dev.argon.compiler.*
import dev.argon.parser.IdentifierExpr

abstract class ArModuleC extends UsingContext {
  val moduleName: ModuleName
  def allExports(): Comp[Seq[ModuleElement]]
  def exports(name: IdentifierExpr): Comp[Seq[ModuleElement]]

  final override def equals(obj: Any): Boolean =
    obj match {
      case other: ArModuleC => other.moduleName == moduleName
      case _ => false
    }

  final override def hashCode(): Int =
    moduleName.hashCode()
}
