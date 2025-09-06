package dev.argon.parser

private[parser] enum LexerMode derives CanEqual {
  case Normal
  case SkipNewLines
  case StringText
}
