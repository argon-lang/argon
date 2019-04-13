package com.mi3software.argon.compiler.core

import PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.util.NamespacePath

trait ArModule[TContext <: Context with Singleton, TPayloadSpec[_, _]] {
  val context: TContext
  import context._

  val descriptor: ModuleDescriptor
  val globalNamespace: context.Comp[Namespace[context.type, TPayloadSpec]]
  val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]
}

final case class ModuleElement[TContext <: Context with Singleton, TPayloadSpec[_, _]](namespacePath: NamespacePath, binding: GlobalBinding[TContext, TPayloadSpec])
