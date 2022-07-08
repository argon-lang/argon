package dev.argon.io

import zio.*
import java.io.InputStream
import java.nio.channels.SeekableByteChannel

trait BinaryResourcePlatformSpecific[+E] {
  def asInputStream: IO[E, Option[InputStream]] = ZIO.none
  def asSeekableByteChannel: IO[E, Option[SeekableByteChannel]] = ZIO.none
}
