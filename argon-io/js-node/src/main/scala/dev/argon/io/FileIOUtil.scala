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

def uint8ArrayToChunk(array: Uint8Array): Chunk[Byte] =
  val cb = ChunkBuilder.make[Byte](array.length)
  for b <- array do
    cb += b.toByte
  cb.result()
end uint8ArrayToChunk

def chunkToUint8Array(chunk: Chunk[Byte]): Uint8Array =
  val arr = new Uint8Array(chunk.size)
  for i <- chunk.indices do
    arr(i) = chunk.byte(i)
  arr
end chunkToUint8Array
