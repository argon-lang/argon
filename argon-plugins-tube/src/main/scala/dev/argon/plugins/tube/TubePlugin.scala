package dev.argon.plugins.tube

import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.*
import dev.argon.compiler.definitions.HasImplementation
import dev.argon.parser.SyntaxError
import dev.argon.plugin.*
import dev.argon.options.*
import dev.argon.plugin.tube.{InvalidTube, SerializedTube, TubeSerializer}
import dev.argon.io.Resource
import zio.*

import java.io.IOException
import java.nio.charset.CharacterCodingException

type TubeError = CharacterCodingException | SyntaxError | CompError | IOException | InvalidTube

final class TubePlugin[R, E >: TubeError] extends Plugin[R, E] {
  override type Options = TubeOptions[R, E]
  override type Output = TubeOutput[R, E]


  override def optionCodec: OptionCodec[R, E, Options] =
    summon[OptionCodec[R, E, Options]]

  override def outputHandler: OutputHandler[R, E, Output] =
    summon[OutputHandler[R, E, Output]]

  override type ExternMethodImplementation = Unit
  override type ExternFunctionImplementation = Unit
  override type ExternClassConstructorImplementation = Unit

  override def emitTube
  (context: Context { type Env = R; type Error = E })
  (adapter: PluginContextAdapter.Aux[context.type, TubePlugin.this.type])
  (tube: ArTubeC & HasContext[context.type] & HasImplementation[true])
  : context.Comp[TubeOutput[context.Env, context.Error]] =
    ZIO.succeed(
      TubeOutput(
        implementationModule = new SerializedTubeResource[R, E] with Resource.WithoutFileName {
          override def asSerializedTube: ZIO[R & Scope, E, SerializedTube[R, E]] =
            TubeSerializer.ofImplementation(context)(tube)
        },
        interfaceModule = new SerializedTubeResource[R, E] with Resource.WithoutFileName {
          override def asSerializedTube: ZIO[R & Scope, E, SerializedTube[R, E]] =
            TubeSerializer.ofInterface(context)(tube)
        },
      )
    )

  override def loadExternMethod(options: TubeOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternFunction(options: TubeOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def loadExternClassConstructor(options: TubeOptions[R, E])(id: String): ZIO[R, E, Option[Unit]] =
    ZIO.unit.asSome

  override def tubeLoaders[ContextOptions]: Map[String, TubeLoader[R, E, ContextOptions]] =
    Map("tube" -> TubeZipTubeLoader())
}
