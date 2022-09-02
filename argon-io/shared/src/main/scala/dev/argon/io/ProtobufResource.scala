package dev.argon.io

import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import zio.*

import java.io.IOException

trait ProtobufResource[-R, +E, A <: GeneratedMessage] extends BinaryResource[R, E] {
  def asMessage: ZIO[R, E, A]
}

object ProtobufResource:
  trait Impl[-R, +E >: IOException, A <: GeneratedMessage] extends ProtobufResource[R, E, A] with ProtobufResourceImplPlatformSpecific[R, E, A]
  
  given [A <: GeneratedMessage: GeneratedMessageCompanion]: BinaryResourceDecoder[[R0, E0] =>> ProtobufResource[R0, E0, A], Any, IOException] =
    ProtobufDecoderPlatformSpecific()
  
end ProtobufResource



