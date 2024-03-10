package dev.argon.io

trait BinaryResourceDecoder[Res[_ >: E0], +E0] {
  def decode[E >: E0](resource: BinaryResource[E]): Res[E]
}
