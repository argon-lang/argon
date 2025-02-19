package dev.argon.io

import dev.argon.util.async.ErrorWrapper
import zio.*

import java.io.InputStream
import java.nio.channels.SeekableByteChannel

trait BinaryResourcePlatformSpecific[+E] {
  self: BinaryResource[E] =>

  def asInputStream[E1 >: E](using ErrorWrapper[E1]): ZIO[Scope, E1, InputStream] =
    ErrorWrapper.unwrapEffect(ErrorWrapper.wrapStream[Any, E1, Byte](asBytes).toInputStream)
}
