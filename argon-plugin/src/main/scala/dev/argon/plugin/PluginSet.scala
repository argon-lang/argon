package dev.argon.plugin

import cats.data.OptionT
import dev.argon.compiler.*
import dev.argon.options.OptionDecoder
import zio.{ZEnvironment, ZIO}

sealed trait PluginSet extends ExternContext {

  type PlatformOptions[E >: PluginError]
  given optionDecoder[E >: PluginError]: OptionDecoder[PlatformOptions[E]]

  val externFunction: Extern
  val externRecord: ExternRef

  type CompatibleContext = Context {
    type Env <: PluginEnv
    type Error >: PluginError
    val implementations: {
      type ExternFunctionImplementation = externFunction.Implementation
      type FunctionReference = externFunction.Reference
      type RecordReference = externRecord.Reference
    }
  }

  def emitter[Ctx <: CompatibleContext]: CompoundTubeEmitter[Ctx]
  def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]]

}

object PluginSet {
  private[plugin] def apply(platforms: PlatformPluginSet, formats: FormatPluginSet[platforms.type]): PluginSet =
    new PluginSet {
      override type PlatformOptions[E >: PluginError] = platforms.PlatformOptions[E]
      override def optionDecoder[E >: PluginError]: OptionDecoder[PlatformOptions[E]] =
        platforms.optionDecoder[E].toDecoder

      override val externFunction: Extern {
        type Implementation = ZEnvironment[platforms.externFunction.Implementation]
        type Reference = ZEnvironment[platforms.externFunction.Reference]
      } = platforms.externFunction.asExtern(this)

      override val externRecord: ExternRef {
        type Reference = ZEnvironment[platforms.externRecord.Reference]
      } = platforms.externRecord.asExtern(this)

      override def emitter[Ctx <: CompatibleContext]: CompoundTubeEmitter[Ctx] =
        TubeEmitterSet.Union(
          platforms.emitter[Ctx],
          formats.emitter[Ctx],
        ).toEmitter

      override def tubeLoaders[Ctx <: CompatibleContext]: Map[TubeLoaderName, TubeLoader[Ctx]] =
        platforms.tubeLoaders[Ctx] ++ formats.tubeLoaders[Ctx]
    }
}
