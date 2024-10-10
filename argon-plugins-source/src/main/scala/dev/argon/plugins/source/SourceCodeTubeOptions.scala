package dev.argon.plugins.source

import esexpr.{constructor, keyword}
import dev.argon.io.{DirectoryResource, BinaryResource, ResourceReader}
import dev.argon.options.OptionDecoder
import dev.argon.parser.SyntaxError

import java.nio.charset.CharacterCodingException
import esexpr.ESExprCodec
import esexpr.ESExpr
import esexpr.ESExprCodec.{DecodeError, ErrorPath}
import zio.*
import dev.argon.tube.ResourceId
import java.io.IOException

final case class SourceCodeTubeOptions[E, PlatformOptions](
  name: dev.argon.tube.TubeName,
  sources: Seq[DirectoryResource[E, ArgonSourceCodeResource]],
  platforms: PlatformOptions,
)

@constructor("source-options")
final case class SourceCodeTubeOptionsRepr[E](
  @keyword name: dev.argon.tube.TubeName,
  @keyword sources: Seq[String],
  @keyword platforms: ESExpr,
)

object SourceCodeTubeOptions {
  given optionDecoder[E >: CharacterCodingException | ArgonSyntaxError | IOException, PlatformOptions](using platformOptionsDecoder: OptionDecoder[PlatformOptions]): OptionDecoder[SourceCodeTubeOptions[E, PlatformOptions]] =
    new OptionDecoder[SourceCodeTubeOptions[E, PlatformOptions]] {

      override def decode(expr: ESExpr): ZIO[ResourceReader, DecodeError, SourceCodeTubeOptions[E, PlatformOptions]] =
        for
          repr <- ZIO.fromEither(ESExprCodec.derived[SourceCodeTubeOptionsRepr[E]].decode(expr))
          sources <- ZIO.foreach(repr.sources)(id => ZIO.serviceWith[ResourceReader](resReader => DirectoryResource.decode[E, BinaryResource, ArgonSourceCodeResource](resReader.directoryResource(id))))
          platformOptions <- platformOptionsDecoder.decode(repr.platforms)
            .mapError(e => DecodeError(e.message, ErrorPath.Keyword("source-options", "platforms", e.path)))
        yield SourceCodeTubeOptions(
          name = repr.name,
          sources = sources,
          platforms = platformOptions,
        )

      end decode
      
    }
}

