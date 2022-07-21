package dev.argon.plugin

import dev.argon.io.BinaryResource

import zio.*

trait ExternCodec[R, E, T] {
  def decode(resource: BinaryResource[R, E]): ZIO[R, E, T]
  def encode(value: T): BinaryResource[R, E]
}
