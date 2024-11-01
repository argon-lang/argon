package dev.argon.plugin

import cats.data.OptionT
import dev.argon.compiler.*
import dev.argon.options.OptionDecoder
import zio.{ZEnvironment, ZIO}

sealed trait PluginSet[E >: PluginError] extends ExternContext[E] {

  type PlatformOptions
  given optionDecoder: OptionDecoder[PlatformOptions]

  val externFunction: Extern
  val externRecord: ExternRef

  type CompatibleContext = Context {
    type Env <: PluginEnv
    type Error = E
    val implementations: {
      type ExternFunctionImplementation = externFunction.Implementation
      type FunctionReference = externFunction.Reference
      type RecordReference = externRecord.Reference
    }
  }

  def emitter[Ctx <: CompatibleContext]: CompoundTubeEmitter[E, Ctx]
  def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]]

}

object PluginSet {
  private[plugin] def apply[E >: PluginError](platforms: PlatformPluginSet[E], formats: FormatPluginSet[E, platforms.type]): PluginSet[E] =
    new PluginSet[E] {
      override type PlatformOptions = platforms.PlatformOptions
      override def optionDecoder: OptionDecoder[PlatformOptions] =
        platforms.optionDecoder.toDecoder

      override val externFunction: Extern {
        type Implementation = ZEnvironment[platforms.externFunction.Implementation]
        type Reference = ZEnvironment[platforms.externFunction.Reference]
      } = platforms.externFunction.asExtern(this)

      override val externRecord: ExternRef {
        type Reference = ZEnvironment[platforms.externRecord.Reference]
      } = platforms.externRecord.asExtern(this)

      override def emitter[Ctx <: CompatibleContext]: CompoundTubeEmitter[E, Ctx] =
        TubeEmitterSet.Union(
          platforms.emitter[Ctx],
          formats.emitter[Ctx],
        ).toEmitter

      override def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]] =
        platforms.tubeLoaders[Ctx] ++ formats.tubeLoaders[Ctx]
    }
}
