package dev.argon.platform

import java.io.IOException

import dev.argon.io.ZipEntryInfo
import dev.argon.io.fileio.FileIOLite
import dev.argon.stream.builder.{Source, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.blocking.Blocking
import zio.stream.ZStream
import zio.{Chunk, Has, ZIO, ZLayer}

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] object FileIOLitePlatform {
  def live: ZLayer[Blocking, Nothing, FileIOLite] = ZLayer.fromFunction { env =>
    val blocking = env.get[Blocking.Service]

    new FileIOLite.Service {
      override def zipEntries[R, E](errorHandler: IOException => E)(entries: ZStream[R, E, ZipEntryInfo[R, E]]): ZStream[R, E, Byte] =
        ZipEntryStreamTransformation(errorHandler, blocking)(entries)

      override def deserializeProtocolBuffer[R, E, A <: GeneratedMessage](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: ZStream[R, E, Byte]): ZIO[R, E, A] =
        InputStreamReaderTransformation(data) { stream =>
          blocking.effectBlocking {
            companion.parseFrom(stream)
          }
            .refineOrDie {
              case ex: IOException => errorHandler(ex)
            }
        }

      override def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): ZStream[R, E, Byte] =
        OutputStreamWriterStream { stream =>
          blocking.effectBlocking {
            message.writeTo(stream)
          }
            .refineOrDie {
              case ex: IOException => errorHandler(ex)
            }
        }
    }
  }
}
