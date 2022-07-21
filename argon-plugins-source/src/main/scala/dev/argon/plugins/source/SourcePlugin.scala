package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.ResourceFactory
import dev.argon.options.{OptionDecoder, OutputHandler}
import dev.argon.parser.SyntaxError
import dev.argon.plugin.*
import zio.*

import java.nio.charset.CharacterCodingException

type SourceError = CharacterCodingException | SyntaxError | CompError

class SourcePlugin extends Plugin[Any, SourceError] {
  override type Options[R, E] = SourceOptions[R, E]
  override type Output[R, E] = SourceOutput[R, E]

  override def optionDecoder[R, E >: SourceError](using ResourceFactory[R, E]): OptionDecoder[R, E, Options[R, E]] =
    summon[OptionDecoder[R, E, Options[R, E]]]

  override def outputHandler[R, E >: SourceError]: OutputHandler[R, E, Output[R, E]] =
    summon[OutputHandler[R, E, Output[R, E]]]

  override def backend: Backend[Options, Output, Any, SourceError] = new Backend[Options, Output, Any, SourceError] {
    override def emitTube
    (context: Context { type Env; type Error >: SourceError })
    (options: Options[context.Env, context.Error])
    (tube: ArTubeC with HasContext[context.type])
    : context.Comp[Output[context.Env, context.Error]] =
      ZIO.succeed(SourceOutput())
  }

  override def tubeLoaders: Seq[TubeLoader[SourceOptions, Any, SourceError]] = Seq(SourceTubeLoader)

  override def buildOutputExecutor: Option[BuildOutputExecutor[SourceOutput]] = None
}
