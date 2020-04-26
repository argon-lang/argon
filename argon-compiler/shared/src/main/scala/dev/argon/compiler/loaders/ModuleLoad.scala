package dev.argon.compiler.loaders

import dev.argon.compiler._
import dev.argon.compiler.core._
import zio._

object ModuleLoad {

  trait Service[-TContext <: Context] {
    def loadResource(id: ResourceIndicator): Managed[ErrorList, Option[ModuleMetadata[TContext]]]
  }

}
