package dev.argon.util

import zio.Chunk
import zio.NonEmptyChunk

object ChunkUnCons {
  def unapply[A](arg: Chunk[A]): Option[NonEmptyChunk[A]] =
    NonEmptyChunk.fromChunk(arg)
}
