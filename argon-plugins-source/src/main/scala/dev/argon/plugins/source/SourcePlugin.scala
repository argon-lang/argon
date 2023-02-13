package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.ResourceFactory
import dev.argon.options.{OptionCodec, OutputHandler}
import dev.argon.parser.SyntaxError
import dev.argon.plugin.*
import java.io.IOException
import zio.*

import java.nio.charset.CharacterCodingException

type SourceError = CharacterCodingException | SyntaxError | CompError | IOException

object SourcePlugin extends Plugin[SourceError] {
  override type Options[-R, +E] = SourceOptions[R, E]
  override type Output[-R, +E] = SourceOutput[R, E]

  override def optionCodec[R, E >: SourceError]: OptionCodec[R, E, Options[R, E]] =
    summon[OptionCodec[R, E, Options[R, E]]]

  override def outputHandler[R, E >: SourceError]: OutputHandler[R, E, Output[R, E]] =
    summon[OutputHandler[R, E, Output[R, E]]]


  override type ExternMethodImplementation = Unit
  override type ExternFunctionImplementation = Unit
  override type ExternClassConstructorImplementation = Unit

  override def emitTube
  (context: Context { type Error >: SourceError })
  (adapter: PluginContextAdapter.Aux[context.type, this.type])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : context.Comp[Output[context.Env, context.Error]] =
    ZIO.succeed(SourceOutput())


  override def loadExternMethod[R, E >: SourceError](options: SourceOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternFunction[R, E >: SourceError](options: SourceOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternClassConstructor[R, E >: SourceError](options: SourceOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def tubeLoaders: Map[String, TubeLoader[SourceError]] = Map("buildspec" -> SourceTubeLoader)
}
