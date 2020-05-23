package dev.argon.io

import zio.stream.ZStream

trait ZipFileReader[-R, +E] {
  def getEntryStream(name: String): ZStream[R, E, Byte]
}
