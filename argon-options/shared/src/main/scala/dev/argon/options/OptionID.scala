package dev.argon.options

trait OptionID {
  type ElementType
  type Decoded[_]
  val decoder: OptionDecoder[Decoded[ElementType]]
  val info: OptionInfo[ElementType]

  def asTypedOption: this.type with TypedOptionID[ElementType]
}

trait TypedOptionID[E] extends OptionID {
  override type ElementType = E

  override def asTypedOption: this.type = this
}

abstract class OptionIDBase[Dec[_], E](implicit val decoder: OptionDecoder[Dec[E]]) extends TypedOptionID[E] {
  override type Decoded[A] = Dec[A]
}

