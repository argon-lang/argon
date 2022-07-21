package dev.argon.io

import zio.*
import java.io.InputStream
import java.nio.channels.SeekableByteChannel

trait BinaryResourcePlatformSpecific[-R, +E] {
  def asInputStream: ZIO[R, E, Option[InputStream]] = ZIO.none
  def asSeekableByteChannel: ZIO[R, E, Option[SeekableByteChannel]] = ZIO.none
}
