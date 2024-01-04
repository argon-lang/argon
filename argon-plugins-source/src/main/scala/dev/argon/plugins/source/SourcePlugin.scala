package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.ResourceFactory
import dev.argon.options.{OptionCodec, OutputHandler}
import dev.argon.parser.SyntaxError
import dev.argon.plugin.*
import dev.argon.plugin.executor.TestExecutor

import java.io.IOException
import zio.*

import java.nio.charset.CharacterCodingException

type SourceError = CharacterCodingException | SyntaxError | CompError | IOException

final class SourcePlugin[R, E >: SourceError] extends Plugin[R, E] {
  override type Options = SourceOptions[R, E]
  override type Output = SourceOutput[R, E]

  override def optionCodec: OptionCodec[R, E, Options] =
    summon[OptionCodec[R, E, Options]]

  override def outputHandler: OutputHandler[R, E, Output] =
    summon[OutputHandler[R, E, Output]]


  override type ExternMethodImplementation = Unit
  override type ExternFunctionImplementation = Unit
  override type ExternClassConstructorImplementation = Unit

  override def emitTube
  (context: Context { type Env = R; type Error = E })
  (adapter: PluginContextAdapter.Aux[context.type, this.type])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : context.Comp[Output] =
    ZIO.succeed(SourceOutput())


  override def loadExternMethod(options: SourceOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternFunction(options: SourceOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternClassConstructor(options: SourceOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, ContextOptions]] =
    Map("buildspec" -> SourceTubeLoader())

  override def testExecutor: Option[TestExecutor[R, E, SourceOptions[R, E], SourceOutput[R, E]]] = None
}
