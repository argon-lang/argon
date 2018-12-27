package com.mi3software.argon.compiler.core

import com.mi3software.argon.compiler.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.util.NamespacePath

trait ArModule[TContext <: Context, TPayloadSpec[_, _]] {
  val context: TContext
  import context._

  val descriptor: ModuleDescriptor
  val globalNamespace: Namespace[context.type, TPayloadSpec]
  val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]
}

final case class ModuleElement[TContext <: Context, TPayloadSpec[_, _]](namespacePath: NamespacePath, binding: GlobalBinding[TContext, TPayloadSpec])
