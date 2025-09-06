package dev.argon.parser

private[parser] enum StringTokenType derives CanEqual {
  case LiteralText
  case EscapedBackspace
  case EscapedFormFeed
  case EscapedNewLine
  case EscapedCarriageReturn
  case EscapedTab
  case EscapedVerticalTab
  case EscapedBackslash
  case EscapedSingleQuote
  case EscapedDoubleQuote
  case EscapedHash
  case EscapedOpenBracket
  case EscapedCloseBracket
  case EscapedOpenCurly
  case EscapedCloseCurly
  case EscapedUnicodeCodePoint
  case InterpolationStart
  case EndOfString
}
