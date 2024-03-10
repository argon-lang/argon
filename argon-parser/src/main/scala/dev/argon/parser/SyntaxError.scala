package dev.argon.parser

import dev.argon.grammar.GrammarError
import dev.argon.util.{FilePosition, FileSpec, SourceLocation}

final case class SyntaxErrorData(fileSpec: FileSpec, syntaxError: SyntaxError)

sealed abstract class SyntaxError {
  def fileName: Option[String]
  def location: SourceLocation

  protected def message: String

  override def toString: String = s"$location: $message"
}

object SyntaxError {
  final case class InvalidSurrogatePairs(ch: Char, override val fileName: Option[String], override val location: SourceLocation) extends SyntaxError {
    override protected def message: String = "Invalid surrogate pair"
  }
  final case class UnexpectedCombingCharacter(cp: Int, override val fileName: Option[String], override val location: SourceLocation) extends SyntaxError {
    override protected def message: String = "Unexpected combing character"
  }

  final case class LexerError(override val fileName: Option[String], error: GrammarError[String, CharacterCategory, FilePosition]) extends SyntaxError {
    override def location: SourceLocation = error.location

    override protected def message: String = s"Lexer error: $error"
  }

  final case class ParserError(override val fileName: Option[String], error: GrammarError[Token, TokenCategory, FilePosition]) extends SyntaxError {
    override def location: SourceLocation = error.location

    override protected def message: String = s"Parse error: $error"
  }

}
