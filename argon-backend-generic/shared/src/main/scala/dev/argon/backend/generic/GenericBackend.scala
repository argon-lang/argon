package dev.argon.backend.generic

import dev.argon.backend.Backend
import dev.argon.backend.Backend.AsFile
import dev.argon.compiler.core.Context.Aux
import dev.argon.compiler.core.ArModule
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.options.{Options, OptionsHandler, TypedOptionID}
import dev.argon.util.Id
import zio._

object GenericBackend extends Backend {
  override val id: String = "generic"
  override val name: String = "Generic"

  override type BackendOptionID = Nothing
  override val backendOptions: OptionsHandler[BackendOptionID, Id] = new OptionsHandler.Empty[Id]

  override type OutputOptionID = Nothing


  override val outputOptions: OptionsHandler[OutputOptionID, AsFile] = new OptionsHandler.Empty[AsFile]

  override def moduleLoaders(options: Options[Id, BackendOptionID]): Seq[ModuleLoader] = Seq()

  override type TExternHandler = GenericExternHandler
  override def externHandler(options: Options[Id, BackendOptionID]): UIO[GenericExternHandler] =
    IO.succeed(new GenericExternHandler)

  override def emitModule(options: Options[Id, BackendOptionID])(context: Aux[GenericBackend.this.type])(module: ArModule[context.type, DeclarationPayloadSpecifier]): Options[Id, OutputOptionID] =
    Options.fromFunction[Id, Nothing](new Options.OptionValueFunction[Id, OutputOptionID] {
      override def apply[E](id: Nothing with TypedOptionID[E]): Id[E] = id
    })
}

