package dev.argon.options

import dev.argon.util.*

trait OptionHandler[Options[_[_], _]] {
  def options: Set[OptionInfoAny[Options]]

  def empty[E]: Options[Option, E]
  def build[E](options: Options[Option, E]): Either[OptionsError.MissingOption, Options[Id, E]]
}

