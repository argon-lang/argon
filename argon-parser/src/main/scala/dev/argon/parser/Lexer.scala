package dev.argon.parser

import dev.argon.util.{FilePosition, WithSource}
import zio.ZIO

private[parser] trait Lexer[R, E] {
  def nextToken(mode: LexerMode): ZIO[R, E, WithSource[Token] | FilePosition]
}
