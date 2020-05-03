package dev.argon.platform

import java.io.IOException

import dev.argon.io.ZipEntryInfo
import dev.argon.io.fileio.FileIOLite
import dev.argon.stream.builder.{Source, SourceIO, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.blocking.Blocking
import zio.{Chunk, ZIO, ZLayer}

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] object FileIOLitePlatform {
  def live: ZLayer[Blocking, Nothing, FileIOLite] = ZLayer.fromFunction { env =>
    val blocking = env.get[Blocking.Service]

    new FileIOLite.Service {
      override def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, *]], Unit]): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
        ZStreamSource(
          ZipEntryStreamTransformation[R, E](errorHandler, env)(entries)
        )

      override def deserializeProtocolBuffer[R, E, A <: GeneratedMessage](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, A] =
        InputStreamReaderTransformation(SourceIO.fromSource(data).toZStream) { stream =>
          blocking.effectBlocking {
            companion.parseFrom(stream)
          }
            .refineOrDie {
              case ex: IOException => errorHandler(ex)
            }
        }

      override def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
        ZStreamSource(
          OutputStreamWriterStream { stream =>
            blocking.effectBlocking {
              message.writeTo(stream)
            }
              .refineOrDie {
                case ex: IOException => errorHandler(ex)
              }
          }
        )
    }
  }
}
