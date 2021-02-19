package dev.argon.io

import zio.{Cause, Chunk, IO, ZIO}
import zio.stream.ZStream

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.|

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
object FileIOUtil {
  def dataStreamToUint8Array[R, E](dataStream: ZStream[R, E, Byte]): ZIO[R, E, Uint8Array] =
    dataStream.runCollect.map { data =>
      val u8arr = new Uint8Array(data.length)
      for(i <- data.indices) {
        u8arr(i) = (data.byte(i) & 0xFF).toShort
      }
      u8arr
    }
}
