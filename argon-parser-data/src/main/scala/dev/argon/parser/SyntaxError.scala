package dev.argon.parser

import dev.argon.grammar.GrammarError
import dev.argon.util.{FileSpec, SourceLocation}

final case class SyntaxErrorData(fileSpec: FileSpec, syntaxError: SyntaxError)

sealed trait SyntaxError {
  def fileName: Option[String]
  def location: SourceLocation
}

object SyntaxError {
  final case class InvalidSurrogatePairs(ch: Char, override val fileName: Option[String], override val location: SourceLocation) extends SyntaxError
  final case class UnexpectedCombingCharacter(cp: Int, override val fileName: Option[String], override val location: SourceLocation) extends SyntaxError

  final case class LexerError(override val fileName: Option[String], error: GrammarError[String, CharacterCategory]) extends SyntaxError {
    override def location: SourceLocation = error.location
  }

  final case class ParserError(override val fileName: Option[String], error: GrammarError[Token, TokenCategory]) extends SyntaxError {
    override def location: SourceLocation = error.location
  }

}
