package dev.argon.platform

import java.io.IOException

import dev.argon.io.ZipEntryInfo
import dev.argon.io.fileio.FileIOLite
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream.ZStream

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] trait FileIOLiteServiceCommon extends FileIOLite.Service with FileIOCommon {

  def zipEntries[R, E](errorHandler: IOException => E)(entries: ZStream[R, E, ZipEntryInfo[R, E]]): ZStream[R, E, Byte] =
    ZStream.unwrap(
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
                .map { data =>
                  val arr = new Array[Byte](data.length)
                  for(i <- arr.indices) {
                    arr(i) = data(i).toByte
                  }

                  ZStream.fromChunk(Chunk.fromArray(arr))
                }
            }
        }
    )

  def deserializeProtocolBuffer[R, E, A <: GeneratedMessage](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: ZStream[R, E, Byte]): ZIO[R, E, A] =
    dataStreamToArray(data)
      .flatMap { data =>
        IO.effect { companion.parseFrom(data) }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
      }

  def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): ZStream[R, E, Byte] =
    ZStream.unwrap(
      IO.effect {
        message.toByteArray
      }
        .refineOrDie {
          case ex: IOException => errorHandler(ex)
        }
        .map { data => ZStream.fromChunk(Chunk.fromArray(data)) }
    )


}
