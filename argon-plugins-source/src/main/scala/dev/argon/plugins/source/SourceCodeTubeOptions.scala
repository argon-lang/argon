package dev.argon.plugins.source

import dev.argon.esexpr.{constructor, keyword}
import dev.argon.io.DirectoryResource
import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException

@constructor("source-options")
final case class SourceCodeTubeOptions[E, PlatformOptions](
  @keyword name: dev.argon.plugin.tube.TubeName,
  @keyword sources: Seq[DirectoryResource[E, ArgonSourceCodeResource]],
  @keyword platforms: PlatformOptions,
)

object SourceCodeTubeOptions {
  given optionDecoder[E >: CharacterCodingException | ArgonSyntaxError, PlatformOptions](using OptionDecoder[E, PlatformOptions]): OptionDecoder[E, SourceCodeTubeOptions[E, PlatformOptions]] =
    OptionDecoder.derive
}

