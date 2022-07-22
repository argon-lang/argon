package dev.argon.io

import zio.*
import java.io.InputStream
import java.nio.channels.SeekableByteChannel

trait BinaryResourcePlatformSpecific[-R, +E] {
  def asInputStream: ZIO[R & Scope, E, Option[InputStream]] = ZIO.none
  def asSeekableByteChannel: ZIO[R & Scope, E, Option[SeekableByteChannel]] = ZIO.none
}
