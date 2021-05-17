package dev.argon.util

import com.google.protobuf.CodedInputStream
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.blocking.Blocking
import zio.stream._

@SuppressWarnings(Array("scalafix:DisableSyntax.null", "scalafix:Disable.eq", "dev.argon.warts.ZioEffect"))
object ProtoBufCodecs {

  def deserializeProtocolBuffer[R <: Blocking, A <: GeneratedMessage]
  (companion: GeneratedMessageCompanion[A])
  (data: ZStream[R, Throwable, Byte])
  : ZIO[R, Throwable, A] =
    data.toInputStream.use { stream =>
      blocking.effectBlockingInterrupt {
        companion.parseFrom(stream)
      }
    }

  def serializeProtocolBuffer(message: GeneratedMessage): ZStream[Blocking, Throwable, Byte] =
    ZStream.fromChunk(Chunk.fromArray(message.toByteArray))


  def deserializeProtocolBufferStream[R <: Blocking, A >: Null <: AnyRef]
  (companion: StreamableMessage[A])
  (data: ZStream[R, Throwable, Byte])
  : ZStream[R, Throwable, A] =
    ZStream(
      data.toInputStream
        .map { stream =>
          CodedInputStream.newInstance(stream)
        }
        .orDie
        .map { input =>
          blocking.effectBlockingInterrupt {
            companion.readElement(input)
          }
            .asSomeError
            .flatMap { a =>
              if(a eq null) IO.fail(None)
              else IO.succeed(Chunk(a))
            }
        }
    )

}
