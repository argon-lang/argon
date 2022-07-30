package dev.argon.compiler.module

import dev.argon.compiler.*
import dev.argon.compiler.definitions.DeclarationMode
import dev.argon.parser.IdentifierExpr

abstract class ArModuleC extends UsingContext with DeclarationMode {
  val tube: ArTube
  val moduleName: ModuleName
  def allExports(): Comp[Seq[ModuleElement[IsDeclaration]]]
  def exports(name: IdentifierExpr): Comp[Seq[ModuleElement[IsDeclaration]]]

  final override def equals(obj: Any): Boolean =
    obj match {
      case other: ArModuleC => other.moduleName == moduleName
      case _ => false
    }

  final override def hashCode(): Int = moduleName.hashCode()
}
