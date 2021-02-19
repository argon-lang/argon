package dev.argon.options

trait OptionDecoder[A] {
  def decodeValue(value: String): OptionDecodeResult.Single[A]
  def decodeAdditionalValue(prev: A, value: String): OptionDecodeResult[A]
}
