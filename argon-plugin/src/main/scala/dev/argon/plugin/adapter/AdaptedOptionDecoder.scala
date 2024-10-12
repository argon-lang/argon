package dev.argon.plugin.adapter

import dev.argon.options.OptionDecoder
import dev.argon.plugin.scalaApi
import java.io.IOException
import esexpr.ESExprCodec.DecodeError
import esexpr.ESExpr
import dev.argon.io.ResourceReader
import zio.ZIO
import dev.argon.util.{*, given}
import zio.UIO
import dev.argon.plugin.PluginError

final class AdaptedOptionDecoder[E >: PluginError, A](decoder: UIO[scalaApi.options.OptionDecoder[E, A]]) extends OptionDecoder[A] {

  override def decode(expr: ESExpr): ZIO[ResourceReader, DecodeError, A] =
    ZIO.serviceWithZIO[ResourceReader] { resReader =>
      val resReader2 = new scalaApi.options.ResourceReader[E] {
        override def getBinaryResource(id: String): UIO[scalaApi.options.BinaryResource[E]] =
          ZIO.succeed(resReader.binaryResource(id))

        override def getDirectoryResource(id: String): UIO[scalaApi.options.DirectoryResource[E]] =
          ZIO.succeed(resReader.directoryResource(id))
      }

      decoder.flatMap(
        _.decode(resReader2, expr)
          .mapError { error =>
            val msg = error.getMessage()
            DecodeError(if msg == null then "" else msg, error.information)
          }
      )
    }
    

  
}
