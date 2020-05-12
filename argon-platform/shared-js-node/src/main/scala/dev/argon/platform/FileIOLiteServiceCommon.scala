package dev.argon.platform

import java.io.IOException

import dev.argon.io.ZipEntryInfo
import dev.argon.io.fileio.FileIOLite
import dev.argon.stream.builder.{Source, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream.ZStream

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] trait FileIOLiteServiceCommon extends FileIOLite.Service with FileIOCommon {

  def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[R, E, ZipEntryInfo[R, E], Unit]): Source[R, E, Chunk[Byte], Unit] =
    new ZStreamSource(
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

  def deserializeProtocolBuffer[R, E, A <: GeneratedMessage](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: Source[R, E, Chunk[Byte], Unit]): ZIO[R, E, A] =
    dataStreamToArray(data)
      .flatMap { data =>
        IO.effect { companion.parseFrom(data) }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
      }

  def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): Source[R, E, Chunk[Byte], Unit] =
    new ZStreamSource(
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
