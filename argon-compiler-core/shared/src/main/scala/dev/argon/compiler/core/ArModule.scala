package dev.argon.compiler.core

import PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.Comp
import dev.argon.util.NamespacePath

trait ArModule[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext

  val descriptor: ModuleId
  val globalNamespace: Comp[Namespace[context.type, TPayloadSpec]]
  val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]
}

final case class ModuleElement[TContext <: Context with Singleton, TPayloadSpec[_, _]](namespacePath: NamespacePath, binding: GlobalBinding[TContext, TPayloadSpec])
