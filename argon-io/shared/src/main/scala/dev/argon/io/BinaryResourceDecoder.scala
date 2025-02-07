package dev.argon.io

trait BinaryResourceDecoder[Res[_ >: E], E] {
  def decode(resource: BinaryResource[E]): Res[E]
}
