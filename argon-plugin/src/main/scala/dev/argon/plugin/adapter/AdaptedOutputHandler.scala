package dev.argon.plugin.adapter

import dev.argon.options.{OutputHandler, OutputInfo}
import dev.argon.plugin.scalaApi
import java.io.IOException
import esexpr.ESExprCodec.DecodeError
import esexpr.ESExpr
import dev.argon.io.{ResourceReader, BinaryResourceErrorUnwrapped, DirectoryResourceErrorUnwrapped}
import zio.ZIO
import dev.argon.util.{*, given}
import zio.UIO
import dev.argon.plugin.PluginError
import dev.argon.util.async.ErrorWrapper
import dev.argon.io.BinaryResource
import dev.argon.io.FileSystemResource

final class AdaptedOutputHandler[E >: PluginError, EX <: Throwable, A](
  decoder: UIO[scalaApi.options.OutputHandler[EX, A]]
)(using errorWrapper: ErrorWrapper.Aux[E, EX]) extends OutputHandler[E, A] {

  override def outputs: UIO[Map[Seq[String], OutputInfo[E, A]]] =
    for
      decoder <- decoder
      outputs <- decoder.outputs()
      outputMapping <- ZIO.foreach(outputs) { output =>
          for
            name <- output.entryName()
          yield name -> new OutputInfo[E, A] {

            override def getValue(options: A): UIO[FileSystemResource[E, BinaryResource]] =
              output.getValue(options).map {
                case scalaApi.options.Resource.BinaryResource(res) => FileSystemResource.Of(BinaryResourceErrorUnwrapped(res))
                case scalaApi.options.Resource.DirectoryResource(res) => DirectoryResourceErrorUnwrapped(res)
              }


          }
        }
    yield outputMapping.toMap
  
}
