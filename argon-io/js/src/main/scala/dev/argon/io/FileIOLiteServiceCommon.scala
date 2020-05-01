package dev.argon.io

import java.io.IOException

import dev.argon.io.fileio.FileIOLite
import dev.argon.stream.builder.{Source, SourceIO, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream.ZStream

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[io] trait FileIOLiteServiceCommon extends FileIOLite.Service with FileIOCommon {

  def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, *]], Unit]): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
    ZStreamSource(
      ZStream.flatten(
        ZStream.fromEffect(
          IO.effectTotal { new JSZip() }
            .flatMap { zip =>
              entries.foreach { entry =>
                dataStreamToUint8Array(entry.dataStream).flatMap { buffer =>
                  IO.effect { zip.file(entry.path, buffer) }
                    .orDie
                    .unit
                }
              }
                .flatMap { _ =>
                  promiseToIO(errorHandler)(zip.generateAsync(JSZip.JSZipGeneratorOptions("uint8array")))
                    .map { data => ZStream(Chunk.fromArray(data.toArray.map { _.toByte })) }
                }
            }
        )
      )
    )

  def deserializeProtocolBuffer[R, E, A <: GeneratedMessage](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, A] =
    dataStreamToArray(SourceIO.fromSource(data))
      .flatMap { data =>
        IO.effect { companion.parseFrom(data) }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
      }

  def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
    ZStreamSource(
      ZStream.fromEffect(
        IO.effect {
          message.toByteArray
        }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
          .map { data => Chunk.fromArray(data) }
      )
    )


}
