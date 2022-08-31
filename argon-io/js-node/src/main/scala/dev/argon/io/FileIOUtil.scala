package dev.argon.io

import zio.*
import zio.stream.ZStream

import scala.scalajs.js.typedarray.Uint8Array


private[io] def dataStreamToUint8Array[R, E](dataStream: ZStream[R, E, Byte]): ZIO[R, E, Uint8Array] =
  dataStream.runCollect.map { data =>
    val u8arr = new Uint8Array(data.length)
    for i <- data.indices do
      u8arr(i) = (data.byte(i) & 0xFF).toShort
    end for
    u8arr
  }

private[io] def uint8ArrayToChunk(array: Uint8Array): UIO[Chunk[Byte]] =
  ZIO.succeed {
    val cb = ChunkBuilder.make[Byte](array.length)
    for b <- array do
      cb += b.toByte
    cb.result()
  }
