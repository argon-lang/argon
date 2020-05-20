package dev.argon.io

import dev.argon.stream.builder.Source
import zio._
import zio.stream.ZStream

trait ZipFileReader[-R, +E] {
  def getEntryStream(name: String): ZStream[R, E, Byte]
}
