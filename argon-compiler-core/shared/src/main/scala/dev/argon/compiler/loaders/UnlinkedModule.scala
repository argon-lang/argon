package dev.argon.compiler.loaders

import dev.argon.compiler.CompManaged
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._

trait UnlinkedModule[-TContext <: Context, PayloadSpecifier[_, _]] {
  val descriptor: ModuleId
  val referencedModules: Vector[ModuleId]
  def load(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): CompManaged[ArModule[context.type, PayloadSpecifier]]
}
