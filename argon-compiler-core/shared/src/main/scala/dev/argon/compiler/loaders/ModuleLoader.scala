package dev.argon.compiler.loaders

import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.core._
import zio._

trait ModuleLoader[-TContext <: Context] {
  def loadResource(fileName: String): CompManaged[Option[UnlinkedModule[TContext, ReferencePayloadSpecifier]]]
}
