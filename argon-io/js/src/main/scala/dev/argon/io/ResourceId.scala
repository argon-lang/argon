package dev.argon.io

import zio.stream.*
import java.nio.file.Path
import zio.ZManaged

trait ResourceId {
  def asStream: ZStream[Any, Nothing, Byte]
}
