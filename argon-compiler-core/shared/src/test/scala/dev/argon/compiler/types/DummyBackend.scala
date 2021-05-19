package dev.argon.compiler.types

import dev.argon.backend.{Backend, ExternHandler}
import dev.argon.compiler.core.ArModule
import dev.argon.compiler.core.Context.Aux
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.{Comp, CompStream, DiagnosticError, DiagnosticSource}
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.options.{Options, OptionsHandler, SingleFile, TypedOptionID}
import dev.argon.util.Id
import zio._

class DummyBackend extends Backend {
  override val id: String = "dummy"
  override val name: String = "Dummy"

  override type BackendOptionID = Nothing
  override val backendOptions: OptionsHandler[Nothing, Id] = new OptionsHandler.Empty[Id]
  override type OutputOptionID = Nothing
  override val outputOptions: OptionsHandler[Nothing, Lambda[X => SingleFile]] = new OptionsHandler.Empty[Lambda[X => SingleFile]]

  override def moduleLoaders(options: Options[Id, Nothing]): Seq[ModuleLoader] = Seq.empty

  override type TExternHandler = ExternHandler
  override def externHandler(options: Options[Id, Nothing]): UIO[ExternHandler] =
    IO.succeed(new ExternHandler {
      override type ExternFunction = Nothing
      override type ExternMethod = Nothing

      override def loadExternFunction(source: DiagnosticSource, name: String): Comp[ExternFunction] =
        IO.fail(DiagnosticError.UnknownExternImplementation(name, source))

      override def loadExternMethod(source: DiagnosticSource, name: String): Comp[ExternMethod] =
        IO.fail(DiagnosticError.UnknownExternImplementation(name, source))

      override def encodeExternalFunction(source: DiagnosticSource, extern: ExternFunction)(platform: String): CompStream[Byte] =
        extern

      override def encodeExternalMethod(source: DiagnosticSource, extern: ExternFunction)(platform: String): CompStream[Byte] =
        extern
    })

  override def emitModule(options: Options[Id, Nothing])(context: Aux[DummyBackend.this.type])(module: ArModule[context.type, DeclarationPayloadSpecifier]): Options[Id, Nothing] =
    Options.fromFunction[Id, Nothing](new Options.OptionValueFunction[Id, Nothing] {
      override def apply[E](id: Nothing with TypedOptionID[E]): Id[E] = id
    })
}
