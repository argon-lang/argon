package dev.argon.backend

import cats.Id
import dev.argon.compiler._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core.{ArModule, Context}
import dev.argon.compiler.loaders.ModuleLoader
import dev.argon.compiler.output.BuildArtifact
import dev.argon.options.{OptionID, Options, OptionsHandler, SingleFile}
import zio._

trait Backend {
  val id: String
  val name: String

  type BackendOptionID <: OptionID
  val backendOptions: OptionsHandler[BackendOptionID, Id]

  type OutputOptionID <: OptionID { type ElementType <: BuildArtifact }
  val outputOptions: OptionsHandler[OutputOptionID, Lambda[X => SingleFile]]

  def moduleLoaders(options: Options[Id, BackendOptionID]): Seq[ModuleLoader]

  type TExternHandler <: ExternHandler
  def externHandler(options: Options[Id, BackendOptionID]): UIO[TExternHandler]

  def emitModule(options: Options[Id, BackendOptionID])(context: Context.Aux[this.type])(module: ArModule[context.type, DeclarationPayloadSpecifier]): Options[Id, OutputOptionID]

}

