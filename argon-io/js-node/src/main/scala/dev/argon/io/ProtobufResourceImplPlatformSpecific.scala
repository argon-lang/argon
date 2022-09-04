package dev.argon.io

import scalapb.GeneratedMessage
import zio.*
import zio.stream.*


trait ProtobufResourceImplPlatformSpecific[-R, +E, +A <: GeneratedMessage] extends ProtobufResource[R, E, A] {
  override def asBytes: ZStream[R, E, Byte] =
    ZStream.unwrapScoped(
      for
        message <- asMessage
      yield ZStream.fromChunk(Chunk.fromArray(message.toByteArray))
    )
}
