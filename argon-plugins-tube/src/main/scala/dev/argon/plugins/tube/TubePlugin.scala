package dev.argon.plugins.tube

import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.parser.SyntaxError
import dev.argon.plugin.*
import dev.argon.options.*
import zio.ZIO

import java.io.IOException
import java.nio.charset.CharacterCodingException

type TubeError = CharacterCodingException | SyntaxError | CompError | IOException

object TubePlugin extends Plugin[Any, TubeError] {
  override type Options[-R, +E] = TubeOptions[R, E]
  override type Output[-R, +E] = TubeOutput[R, E]


  override def optionCodec[R, E >: TubeError]: OptionCodec[R, E, Options[R, E]] =
    summon[OptionCodec[R, E, Options[R, E]]]

  override def outputHandler[R, E >: TubeError]: OutputHandler[R, E, Output[R, E]] =
    summon[OutputHandler[R, E, Output[R, E]]]

  override type ExternMethodImplementation = Unit
  override type ExternFunctionImplementation = Unit
  override type ExternClassConstructorImplementation = Unit

  override def emitTube
  (context: Context { type Env; type Error >: TubeError })
  (adapter: PluginContextAdapter.Aux[context.type, TubePlugin.this.type])
  (options: Options[context.Env, context.Error])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : context.Comp[TubeOutput[context.Env, context.Error]] = ???

  override def loadExternMethod[R <: Any, E >: TubeError](options: TubeOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternFunction[R <: Any, E >: TubeError](options: TubeOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternClassConstructor[R <: Any, E >: TubeError](options: TubeOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def tubeLoaders: Map[String, TubeLoader[Any, TubeError]] = ???
}
