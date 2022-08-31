package dev.argon.io

import zio.*
import java.io.InputStream
import java.nio.channels.SeekableByteChannel

trait BinaryResourcePlatformSpecific[-R, +E] {
  def asInputStream: Option[ZIO[R & Scope, E, InputStream]] = None
  def asSeekableByteChannel: Option[ZIO[R & Scope, E, SeekableByteChannel]] = None
}
