package dev.argon.io

import dev.argon.stream._
import zio._

trait ZipFileReader[E] {
  def getEntryStream(name: String): ArStream[ZIO, Any, E, Byte]
}
