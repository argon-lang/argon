package dev.argon.util

import com.google.protobuf.CodedInputStream
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio._
import zio.stream._

import scala.collection.mutable

@SuppressWarnings(Array("scalafix:Disable.eq", "scalafix:DisableSyntax.null", "scalafix:Disable.mutable", "dev.argon.warts.ZioEffect"))
object ProtoBufCodecs {

  def deserializeProtocolBuffer[R, A <: GeneratedMessage]
  (companion: GeneratedMessageCompanion[A])
  (data: ZStream[R, Throwable, Byte])
  : ZIO[R, Throwable, A] =
    data.runCollect.flatMap { data =>
      IO.effect { companion.parseFrom(data.toArray) }
    }

  def serializeProtocolBuffer(message: GeneratedMessage): Stream[Throwable, Byte] =
    ZStream.unwrap(
      IO.effect {
        message.toByteArray
      }
        .map { data => ZStream.fromChunk(Chunk.fromArray(data)) }
    )

  def deserializeProtocolBufferStream[R, A >: Null <: AnyRef]
  (companion: StreamableMessage[A])
  (data: ZStream[R, Throwable, Byte])
  : ZStream[R, Throwable, A] =
    ZStream.unwrap(
      data.runCollect
        .flatMap { data =>
          IO.effect {
            val input = CodedInputStream.newInstance(data.toArray)
            val buffer = new mutable.ArrayBuffer[A]()

            def iter(): Unit = {
              val elem = companion.readElement(input)
              if(elem eq null) {
                ()
              }
              else {
                val _ = buffer += elem
                iter()
              }
            }

            iter()

            ZStream.fromIterable(buffer.toSeq)
          }
        }
    )

}
