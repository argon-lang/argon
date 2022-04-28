package dev.argon.options

sealed trait OptionsError

object OptionsError {
  final case class MissingOption(missingOptionNames: Seq[String]) extends OptionsError

  sealed trait ParseError extends OptionsError

  final case class CouldNotDecode(name: String) extends ParseError
  final case class MultipleValuesNotSupported(name: String) extends ParseError
}

