package dev.argon.io

import zio.*
import java.io.InputStream
import java.nio.channels.SeekableByteChannel

trait BinaryResourcePlatformSpecific[+E] {
  def asInputStream: Option[ZIO[Scope, E, InputStream]] = None
  def asSeekableByteChannel: Option[ZIO[Scope, E, SeekableByteChannel]] = None
}
