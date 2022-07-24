package dev.argon.io

import zio.stream.ZStream

abstract class BinaryResource[-R, +E] extends Resource[R, E] with BinaryResourcePlatformSpecific[R, E] {
  def asBytes: ZStream[R, E, Byte]
}

object BinaryResource:
  given BinaryResourceDecoder[BinaryResource, Any, Nothing] with
    def decode[R <: Any, E >: Nothing](resource: BinaryResource[R, E]): BinaryResource[R, E] =
      resource
  end given
end BinaryResource