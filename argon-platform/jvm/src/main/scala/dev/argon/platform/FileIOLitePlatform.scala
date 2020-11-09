package dev.argon.platform

import java.io.IOException

import com.google.protobuf.CodedInputStream
import dev.argon.io.{StreamableMessage, ZipEntryInfo}
import dev.argon.io.fileio.FileIOLite
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.blocking.Blocking
import zio.stream._
import zio._

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Null", "dev.argon.warts.ZioEffect"))
private[platform] object FileIOLitePlatform {
  def live: ZLayer[Blocking, Nothing, FileIOLite] = ZLayer.fromFunction { env =>
    val blocking = env.get[Blocking.Service]

    new FileIOLite.Service {
      override def zipEntries[R, E <: Throwable](errorHandler: Throwable => Cause[E])(entries: ZStream[R, E, ZipEntryInfo[R, E]]): ZStream[R, E, Byte] =
        ZipEntryStreamTransformation(errorHandler, blocking)(entries)

      override def deserializeProtocolBuffer[R, E <: Throwable, A <: GeneratedMessage](errorHandler: Throwable => Cause[E])(companion: GeneratedMessageCompanion[A])(data: ZStream[R, E, Byte]): ZIO[R, E, A] =
        data.toInputStream.use { stream =>
          blocking.effectBlocking {
            companion.parseFrom(stream)
          }
            .catchAll { ex => IO.halt(errorHandler(ex)) }
        }

      override def serializeProtocolBuffer[E <: Throwable](errorHandler: Throwable => Cause[E])(message: GeneratedMessage): Stream[E, Byte] =
        ZStream.fromOutputStreamWriter { stream =>
          message.writeTo(stream)
        }
          .catchAll { ex => ZStream.halt(errorHandler(ex)) }
          .provide(env)

      override def deserializeProtocolBufferStream[R, E <: Throwable, A >: Null <: AnyRef](errorHandler: Throwable => Cause[E])(companion: StreamableMessage[A])(data: ZStream[R, E, Byte]): ZStream[R, E, A] =
        ZStream(
          data.toInputStream
            .map { stream =>
              CodedInputStream.newInstance(stream)
            }
            .orDie
            .map { input =>
              blocking.effectBlocking {
                companion.readElement(input)
              }
                .catchAll { ex => IO.halt(errorHandler(ex)) }
                .asSomeError
                .flatMap { a =>
                  if(a eq null) IO.fail(None)
                  else IO.succeed(Chunk(a))
                }
            }
        )
    }
  }
}
