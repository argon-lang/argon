package dev.argon.io

import scalapb.GeneratedMessage
import zio.*
import zio.stream.*
import java.io.IOException


trait ProtobufResourceImplPlatformSpecific[-R, +E >: IOException, A <: GeneratedMessage] extends ProtobufResource[R, E, A] {
  override def asBytes: ZStream[R, E, Byte] =
    ZStream.unwrapScoped(
      asMessage.map { message =>
        ZStream.fromOutputStreamWriter { outputStream =>
          message.writeTo(outputStream)
        }.refineToOrDie[IOException]
      }
    )
}
