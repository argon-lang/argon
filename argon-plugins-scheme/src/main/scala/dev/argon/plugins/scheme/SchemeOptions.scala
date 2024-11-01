package dev.argon.plugins.scheme

import esexpr.*
import dev.argon.plugin.scalaApi.options.OptionDecoder
import dev.argon.plugin.scalaApi.DecodeError
import nobleidl.core.Esexpr
import zio.*
import dev.argon.plugin.scalaApi.options.ResourceReader

private[scheme] final case class SchemeOptions(
  @keyword
  externs: Dictionary[SchemeImport],
) derives ESExprCodec

private[scheme] final case class SchemeImport(
  library: Seq[String],
  name: String,
) derives ESExprCodec

private[scheme] object SchemeOptions {

  def optionDecoder[E <: Throwable]: OptionDecoder[E, SchemeOptions] =
    new OptionDecoder[E, SchemeOptions] {
      override def decode(reader: ResourceReader[E], expr: Esexpr): IO[DecodeError, SchemeOptions] =
        ZIO.fromEither(summon[ESExprCodec[SchemeOptions]].decode(expr))
          .mapError(DecodeError.fromCodecError)
    }

}

private[scheme] final case class SchemeOutputOptions(

) derives ESExprCodec

private[scheme] object SchemeOutputOptions {

  def optionDecoder[E <: Throwable]: OptionDecoder[E, SchemeOutputOptions] =
    new OptionDecoder[E, SchemeOutputOptions] {
      override def decode(reader: ResourceReader[E], expr: Esexpr): IO[DecodeError, SchemeOutputOptions] =
        ZIO.fromEither(summon[ESExprCodec[SchemeOutputOptions]].decode(expr))
          .mapError(DecodeError.fromCodecError)
    }

}

private[scheme] final case class SchemeOutput(

)

private[scheme] object SchemeOutput {


}

