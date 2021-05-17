package dev.argon.backend.generic

import cats.Applicative
import dev.argon.backend.Backend
import dev.argon.compiler.core.Context.Aux
import dev.argon.compiler.core.ArModule
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.options.{OptionDecoder, OptionInfo, Options, OptionsHandler, SingleFile}
import shapeless._
import zio._

object GenericBackend extends Backend {
  override val id: String = "generic"
  override val name: String = "Generic"

  override type BackendOptionID = Nothing
  override val backendOptions: OptionsHandler[BackendOptionID, Id] = new OptionsHandler[BackendOptionID, Id] {
    override type OptRepr[A[_]] = HNil

    override def ids: HNil = HNil

    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: HNil, listb: HNil)(f: OptionsHandler.CombineFunction[BackendOptionID, A, B, C, F]): F[HNil] =
      Applicative[F].pure(HNil)

    override def reprToOptions[A[_]](list: HNil): Options[A, Nothing] =
      Options.fromFunction[A, Nothing](new Options.OptionValueFunction[A, Nothing] {
        override def apply[E](id: Nothing { type ElementType = E }): A[E] = id
      })

    override def info: Options[OptionInfo, Nothing] = reprToOptions(HNil)

    override def decoder: Options[OptionDecoder, Nothing] = reprToOptions(HNil)
  }

  override type OutputOptionID = Nothing


  override val outputOptions: OptionsHandler[OutputOptionID, Lambda[X => SingleFile]] = new OptionsHandler.Empty[Lambda[X => SingleFile]]

  override def moduleLoaders(options: Options[Id, BackendOptionID]): Seq[ModuleLoader] = Seq()

  override type TExternHandler = GenericExternHandler
  override def externHandler(options: Options[Id, BackendOptionID]): UIO[GenericExternHandler] =
    IO.succeed(new GenericExternHandler)

  override def emitModule(options: Options[Id, BackendOptionID])(context: Aux[GenericBackend.this.type])(module: ArModule[context.type, DeclarationPayloadSpecifier]): Options[Id, OutputOptionID] =
    Options.fromFunction[Id, Nothing](new Options.OptionValueFunction[Id, OutputOptionID] {
      override def apply[E](id: Nothing { type ElementType = E }): Id[E] = id
    })
}

