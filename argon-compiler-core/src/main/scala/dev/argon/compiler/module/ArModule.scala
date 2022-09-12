package dev.argon.compiler.module

import dev.argon.compiler.*
import dev.argon.compiler.definitions.{DeclarationMode, HasImplementation}
import dev.argon.parser.IdentifierExpr

abstract class ArModuleC extends UsingContext with DeclarationMode {
  val tube: ArTube & HasImplementation[IsImplementation]
  val moduleName: ModuleName
  def allExports(exportingModules: Set[ArModule]): Comp[Seq[ModuleElement[IsImplementation]]]
  def namedExports(exportingModules: Set[ArModule])(name: IdentifierExpr): Comp[Seq[ModuleElement[IsImplementation]]] =
    exports(exportingModules)(Some(name))

  def exports(exportingModules: Set[ArModule])(name: Option[IdentifierExpr]): Comp[Seq[ModuleElement[IsImplementation]]]


  final override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match {
      case other: ArModuleC => other.moduleName == moduleName
      case _ => false
    }

  final override def hashCode(): Int = moduleName.hashCode()

  override def toString: String = s"ArModule($moduleName)"
}
