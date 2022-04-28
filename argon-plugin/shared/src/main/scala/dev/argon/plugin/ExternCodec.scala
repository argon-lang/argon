package dev.argon.plugin

import dev.argon.io.BinaryResource

import zio.*

trait ExternCodec[E, T] {
  def decode(resource: BinaryResource[E]): IO[E, T]
  def encode(value: T): BinaryResource[E]
}
