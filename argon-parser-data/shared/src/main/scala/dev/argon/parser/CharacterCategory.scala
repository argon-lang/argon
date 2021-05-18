package dev.argon.parser

sealed trait CharacterCategory
object CharacterCategory {
  case object StringChar extends CharacterCategory
  case object CR extends CharacterCategory
  case object LF extends CharacterCategory
  case object Whitespace extends CharacterCategory
  case object Quote extends CharacterCategory
  case object StringEscape extends CharacterCategory
  case object NumberDigit extends CharacterCategory
  case object NonZeroDigit extends CharacterCategory
  case object Zero extends CharacterCategory
  case object Digit extends CharacterCategory
  case object BaseSpecifier extends CharacterCategory
  case object Letter extends CharacterCategory
  case object Underscore extends CharacterCategory
  case object Hash extends CharacterCategory

  case object QMark extends CharacterCategory
  case object Exclaim extends CharacterCategory

  case object And extends CharacterCategory
  case object Or extends CharacterCategory
  case object LessThan extends CharacterCategory
  case object GreaterThan extends CharacterCategory
  case object Equals extends CharacterCategory
  case object Colon extends CharacterCategory
  case object Plus extends CharacterCategory
  case object Minus extends CharacterCategory

  case object NotEquals extends CharacterCategory
  case object LessThanEq extends CharacterCategory
  case object GreaterThanEq extends CharacterCategory

  case object Dot extends CharacterCategory
  case object Comma extends CharacterCategory
  case object Semicolon extends CharacterCategory

  case object OpenParen extends CharacterCategory
  case object CloseParen extends CharacterCategory
  case object OpenSquare extends CharacterCategory
  case object CloseSquare extends CharacterCategory
  case object OpenCurly extends CharacterCategory
  case object CloseCurly extends CharacterCategory

  case object Star extends CharacterCategory
  case object Times extends CharacterCategory
  case object Slash extends CharacterCategory
  case object Divide extends CharacterCategory

  case object Caret extends CharacterCategory
  case object Tilde extends CharacterCategory

}
