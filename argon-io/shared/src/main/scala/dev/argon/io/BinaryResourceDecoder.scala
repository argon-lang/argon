package dev.argon.io

trait BinaryResourceDecoder[Res[_ <: R0, _ >: E0], -R0, +E0] {
  def decode[R <: R0, E >: E0](resource: BinaryResource[R, E]): Res[R, E]
}
