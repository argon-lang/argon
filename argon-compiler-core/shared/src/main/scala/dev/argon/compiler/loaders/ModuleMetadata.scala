package dev.argon.compiler.loaders

import dev.argon.compiler.Comp
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._

trait ModuleMetadata[-TContext <: Context] {
  val descriptor: ModuleId
  val referencedModules: Vector[ModuleId]
  def loadReference(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): Comp[ArModule[context.type, ReferencePayloadSpecifier]]
}
