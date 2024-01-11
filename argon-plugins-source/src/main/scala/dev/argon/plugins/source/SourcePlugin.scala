package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.ResourceFactory
import dev.argon.options.{OptionDecoder, OutputHandler}
import dev.argon.parser.SyntaxError
import dev.argon.plugin.*
import dev.argon.plugin.executor.TestExecutor

import java.io.IOException
import zio.*

import java.nio.charset.CharacterCodingException

type SourceEnv = CompEnv
type SourceError = CharacterCodingException | SyntaxError | CompError | IOException

final class SourcePlugin[R <: SourceEnv, E >: SourceError, PlatformPlugins <: PlatformPluginSet[R, E]](val platformPlugins: PlatformPlugins) extends CompositePlugin[R, E, PlatformPlugins] {
  override val pluginId: "source" = "source"

  override type OutputOptions = Nothing
  override type Output = Nothing

  override def outputOptionsDecoder: OptionDecoder[R, E, Nothing] =
    summon[OptionDecoder[R, E, OutputOptions]]

  override def outputHandler: OutputHandler[R, E, Nothing] =
    summon[OutputHandler[R, E, Output]]

  override def emitTube[CtxPlugin <: PluginForPlatforms]
  (context: PluginContext[R, E, CtxPlugin])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  (options: OutputOptions)
  : context.Comp[Output] =
    options

  override def testExecutor: Option[TestExecutor[R, E, Options, Output]] = None

  override def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, this.type]] =
    Map("buildspec" -> SourceTubeLoader(this))
}
