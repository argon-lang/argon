package dev.argon.util

import zio.Chunk

import scala.scalajs.js.typedarray.{Int8Array, Uint8Array, byteArray2Int8Array, int8Array2ByteArray}

object TypedArrayUtil {
  def fromByteArray(a: Array[Byte]): Uint8Array = new Uint8Array(byteArray2Int8Array(a).buffer)
  def fromByteChunk(chunk: Chunk[Byte]): Uint8Array = fromByteArray(chunk.toArray)
  
  def toByteArray(a: Uint8Array): Array[Byte] = int8Array2ByteArray(new Int8Array(a.buffer))
  def toByteChunk(a: Uint8Array): Chunk[Byte] = Chunk.fromArray(toByteArray(a))
}
