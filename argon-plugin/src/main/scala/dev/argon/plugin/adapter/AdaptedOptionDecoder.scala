package dev.argon.plugin.adapter

import dev.argon.options.OptionDecoder
import dev.argon.plugin.scalaApi
import java.io.IOException
import esexpr.ESExprCodec.DecodeError
import esexpr.ESExpr
import dev.argon.io.{ResourceReader, BinaryResourceErrorWrapped, DirectoryResourceErrorWrapped}
import zio.ZIO
import dev.argon.util.{*, given}
import zio.UIO
import dev.argon.plugin.PluginError
import dev.argon.util.async.ErrorWrapper

final class AdaptedOptionDecoder[E >: PluginError, EX <: Throwable, A](
  decoder: UIO[scalaApi.options.OptionDecoder[EX, A]]
)(using errorWrapper: ErrorWrapper.Aux[E, EX]) extends OptionDecoder[A] {

  override def decode(expr: ESExpr): ZIO[ResourceReader, DecodeError, A] =
    ZIO.serviceWithZIO[ResourceReader] { resReader =>
      val resReader2 = new scalaApi.options.ResourceReader[EX] {
        override def getBinaryResource(id: String): UIO[scalaApi.options.BinaryResource[EX]] =
          ZIO.succeed(BinaryResourceErrorWrapped[E, EX](resReader.binaryResource(id)))

        override def getDirectoryResource(id: String): UIO[scalaApi.options.DirectoryResource[EX]] =
          ZIO.succeed(DirectoryResourceErrorWrapped[E, EX](resReader.directoryResource(id)))
      }

      decoder.flatMap(
        _.decode(resReader2, expr)
          .mapError(scalaApi.DecodeError.toCodecError)
      )
    }
    

  
}
