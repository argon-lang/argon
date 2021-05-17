package dev.argon.io

import zio.blocking.Blocking
import zio.stream.ZStream

object ZipCreator {
  def zipEntries[R <: Blocking](entries: ZStream[R, Throwable, ZipEntryInfo[R, Throwable]]): ZStream[R, Throwable, Byte] =
    ZipEntryStreamTransformation(entries)
}
