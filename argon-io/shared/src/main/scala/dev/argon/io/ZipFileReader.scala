package dev.argon.io

import zio.{IO, ZIO, ZManaged}
import zio.stream.ZStream

trait ZipFileReader[-R, +E] {
  def getEntryStream(name: String): ZIO[R, E, Option[ZStream[R, E, Byte]]]
}
