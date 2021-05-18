package dev.argon.options

trait OptionID {
  type ElementType
  type Decoded[_]
  val decoder: OptionDecoder[Decoded[ElementType]]
}

abstract class TypedOptionID[Dec[_], E](implicit val decoder: OptionDecoder[Dec[E]]) extends OptionID {
  override type ElementType = E
  override type Decoded[A] = Dec[A]
}

