package dev.argon.compiler.loaders

import dev.argon.compiler.{Comp, CompManaged, CompilationError}
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import zio.Managed

trait ModuleMetadata[-TContext <: Context] {
  val descriptor: ModuleId
  val referencedModules: Vector[ModuleId]
  def loadReference(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): CompManaged[ArModule[context.type, ReferencePayloadSpecifier]]
}
