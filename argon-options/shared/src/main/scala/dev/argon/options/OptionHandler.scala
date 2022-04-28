package dev.argon.options

trait OptionHandler[E, Options] {
  type Builder
  def options: Set[OptionInfo[E, ?, Options, Builder]]

  def emptyBuilder: Builder
  def build(builder: Builder): Either[OptionsError.MissingOption, Options]
}
