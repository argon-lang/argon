package dev.argon.parser

import zio.{Chunk, ZIO}

private[parser] trait TextReader[R, E] {
  def read: ZIO[R, E, Chunk[String]]
}
