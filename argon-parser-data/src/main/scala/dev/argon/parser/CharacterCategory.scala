package dev.argon.parser

sealed trait CharacterCategory
object CharacterCategory {
  final case object SingleQuoteStringChar extends CharacterCategory
  final case object CR extends CharacterCategory
  final case object LF extends CharacterCategory
  final case object Whitespace extends CharacterCategory
  final case object SingleQuote extends CharacterCategory
  final case object NumberDigit extends CharacterCategory
  final case object NonZeroDigit extends CharacterCategory
  final case object Zero extends CharacterCategory
  final case object Digit extends CharacterCategory
  final case object BaseSpecifier extends CharacterCategory
  final case object Letter extends CharacterCategory
  final case object Underscore extends CharacterCategory

  final case object QMark extends CharacterCategory
  final case object Exclaim extends CharacterCategory

  final case object And extends CharacterCategory
  final case object Or extends CharacterCategory
  final case object LessThan extends CharacterCategory
  final case object GreaterThan extends CharacterCategory
  final case object Equals extends CharacterCategory
  final case object Colon extends CharacterCategory
  final case object Plus extends CharacterCategory
  final case object Minus extends CharacterCategory

  final case object NotEquals extends CharacterCategory
  final case object LessThanEq extends CharacterCategory
  final case object GreaterThanEq extends CharacterCategory

  final case object Dot extends CharacterCategory
  final case object Comma extends CharacterCategory
  final case object Semicolon extends CharacterCategory

  final case object OpenParen extends CharacterCategory
  final case object CloseParen extends CharacterCategory
  final case object OpenSquare extends CharacterCategory
  final case object CloseSquare extends CharacterCategory
  final case object OpenCurly extends CharacterCategory
  final case object CloseCurly extends CharacterCategory

  final case object Star extends CharacterCategory
  final case object Times extends CharacterCategory
  final case object Slash extends CharacterCategory
  final case object Divide extends CharacterCategory

  final case object Caret extends CharacterCategory
  final case object Tilde extends CharacterCategory

}
