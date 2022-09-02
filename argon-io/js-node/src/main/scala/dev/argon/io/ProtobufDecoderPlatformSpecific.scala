package dev.argon.io

import dev.argon.util.*
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.ZIO
import zio.stream.ZStream

import java.io.IOException

final class ProtobufDecoderPlatformSpecific[A <: GeneratedMessage: GeneratedMessageCompanion] extends BinaryResourceDecoder[[R0, E0] =>> ProtobufResource[R0, E0, A], Any, IOException] {
  override def decode[R <: Any, E >: IOException](resource: BinaryResource[R, E]): ProtobufResource[R, E, A] =
    new ProtobufResource[R, E, A] {
      override def fileName: Option[String] = resource.fileName

      override def asBytes: ZStream[R, E, Byte] = resource.asBytes

      override def asMessage: ZIO[R, E, A] =
        asBytes.runCollect.map { bytes =>
          summon[GeneratedMessageCompanion[A]].parseFrom(bytes.toArray)
        }

    }
}
