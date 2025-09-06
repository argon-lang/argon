package dev.argon.parser

import dev.argon.grammar.GrammarError
import dev.argon.util.{FilePosition, FileSpec, SourceLocation, WithSource}

final case class SyntaxErrorData(fileSpec: FileSpec, syntaxError: SyntaxError)

sealed abstract class SyntaxError {
  def location: SourceLocation

  protected def message: String

  override def toString: String = s"$location: $message"
}

object SyntaxError {
  private[parser] final case class SyntaxErrorException(error: SyntaxError) extends Exception {
    override def getMessage: String = error.message
  }

  final case class InvalidSurrogatePairs(ch: Char, override val location: SourceLocation) extends SyntaxError {
    override protected def message: String = "Invalid surrogate pair"
  }
  final case class UnexpectedCombingCharacter(cp: Int, override val location: SourceLocation) extends SyntaxError {
    override protected def message: String = "Unexpected combing character"
  }

  final case class LexerError(override val location: SourceLocation, invalidChar: Option[Int], partialToken: String) extends SyntaxError {
    override protected def message: String = s"Lexer error: Invalid character ${invalidChar.fold("<EOF>")(Character.toString)} ${if partialToken.isEmpty then "at start of token" else "following partial token '$partialToken'"}"
  }

  final case class InvalidIntegerToken(override val location: SourceLocation, invalidIntToken: String) extends SyntaxError {
    override protected def message: String = s"Lexer error: Invalid integer token: $invalidIntToken"
  }

  final case class ParserError(ruleName: String, nextToken: WithSource[Token], expectedCategories: Set[TokenCategory]) extends SyntaxError {
    override def location: SourceLocation = nextToken.location

    override protected def message: String = s"Parse error: While parsing $ruleName, found token ${nextToken.value}, expected ${expectedCategories.mkString(", ")}"
  }

}
