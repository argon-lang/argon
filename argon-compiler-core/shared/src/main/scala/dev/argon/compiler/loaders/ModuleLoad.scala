package dev.argon.compiler.loaders

import dev.argon.compiler._
import dev.argon.compiler.core._
import zio._

object ModuleLoad {

  trait Service[I <: ResourceIndicator, -TContext <: Context] {
    def loadResource(id: I): Managed[CompilationError, Option[ModuleMetadata[TContext]]]
  }

}
