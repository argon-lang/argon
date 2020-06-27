package dev.argon.compiler.core

import PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.Comp
import dev.argon.compiler.lookup.ModuleLookup
import dev.argon.util.NamespacePath

trait ArModule[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext

  val id: ModuleId
  val globalNamespace: Comp[Namespace[context.type, TPayloadSpec]]
  val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]

  def lookupNamespaceValues[T]
  (namespace: NamespacePath, name: GlobalName)
  (f: GlobalBinding[context.type, TPayloadSpec] => Comp[Option[T]])
  : Comp[Vector[T]] =
    globalNamespace.flatMap { globalNS =>
      ModuleLookup.lookupNamespaceValuesInGlobalNS(context)(globalNS)(namespace, name)(f)
    }

}

final case class ModuleElement[TContext <: Context with Singleton, TPayloadSpec[_, _]](namespacePath: NamespacePath, binding: GlobalBinding[TContext, TPayloadSpec])
