package dev.argon.compiler.loaders

import dev.argon.compiler.{Comp, CompManaged, CompilationError}
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.core._
import zio.Managed

trait UnlinkedModule[-TContext <: Context, PayloadSpecifier[_, _]] {
  val descriptor: ModuleId
  val referencedModules: Vector[ModuleId]
  def load(context: TContext)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): CompManaged[ArModule[context.type, PayloadSpecifier]]
}
