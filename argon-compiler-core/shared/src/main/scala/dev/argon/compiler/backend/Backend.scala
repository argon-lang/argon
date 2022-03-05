package dev.argon.compiler.backend

import dev.argon.compiler.*
import dev.argon.compiler.module.*
import dev.argon.options.*
import dev.argon.util.*
import dev.argon.io.*
import zio.*
import Backend.*

trait BackendBase {
  type BackendOptionID <: OptionID { type Decoded[A] = A }
  val backendOptions: OptionsHandler[BackendOptionID, Id]

  type OutputOptionID <: OptionID { type ElementType <: BinaryResource; type Decoded[A] = A }
  val outputOptions: OptionsHandler[OutputOptionID, Id]

  trait BackendHandler {

    def moduleLoaders(options: Options[Id, BackendOptionID]): Seq[ModuleLoader]

    type TExternHandler <: ExternHandler
    def externHandler(options: Options[Id, BackendOptionID]): UIO[TExternHandler]

    def emitModule(options: Options[Id, BackendOptionID])(context: Context.WithBackend[this.type])
                  (module: ArModuleC with HasContext[context.type])
    : Options[Id, OutputOptionID]
  }

}

trait Backend extends BackendBase {
  val id: String
  val name: String
}

object Backend {
  type AsFile[_] = SingleFile
  type AsFileOption[_] = Option[SingleFile]
  type AsUnit[_] = Unit
}
