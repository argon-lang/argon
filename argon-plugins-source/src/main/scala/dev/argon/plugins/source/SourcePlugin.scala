package dev.argon.plugins.source

import cats.data.NonEmptySeq
import dev.argon.compiler.{ArTubeC, HasContext, TubeImporter, TubeName}
import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError
import dev.argon.plugin.{FormatPlugin, PlatformPluginSet, PluginError, TubeEmitter, TubeLoader}
import zio.{Scope, ZIO}

import java.nio.charset.CharacterCodingException
import dev.argon.plugin.PluginSetUtil.PartialOptionDecoder.given

final class SourcePlugin[Platforms <: PlatformPluginSet](override val platforms: Platforms) extends FormatPlugin[Platforms] {
  override val pluginId: String = "source"

  override def emitter[Ctx <: CompatibleContext]: Option[TubeEmitter[Ctx]] = None

  override def tubeLoaders[Ctx <: CompatibleContext]: Map[String, TubeLoader[Ctx]] =
    Map("argon-sources" -> new TubeLoader[Ctx] {
      override type LibOptions[E >: PluginError] = SourceCodeTubeOptions[E, platforms.PlatformOptions[E]]
      override def libOptionDecoder[E >: PluginError]: OptionDecoder[E, LibOptions[E]] =
        SourceCodeTubeOptions.optionDecoder

      override def load
      (context: Ctx)
      (tubeImporter: TubeImporter & HasContext[context.type])
      (libOptions: LibOptions[context.Error])
      : ZIO[context.Env & Scope, context.Error, ArTubeC & HasContext[context.type]] =
        SourceTube.make(platforms)(context)(libOptions)(tubeImporter)
    })
}
