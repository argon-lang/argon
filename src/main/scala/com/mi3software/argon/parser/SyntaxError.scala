package com.mi3software.argon.parser

import com.mi3software.argon.parser.impl.{Grammar, Token}
import com.mi3software.argon.util.{FileSpec, SourceLocation, WithSource}

import scala.collection.immutable._

final case class SyntaxErrorData(fileSpec: FileSpec, syntaxError: SyntaxError)

sealed trait SyntaxError {
  def location: SourceLocation
}
object SyntaxError {
  final case class InvalidSurrogatePairs(ch: Char, override val location: SourceLocation) extends SyntaxError
  final case class UnexpectedCombingCharacter(cp: Int, override val location: SourceLocation) extends SyntaxError

  final case class LexerError(error: Grammar.GrammarError[String, CharacterCategory]) extends SyntaxError {
    override def location: SourceLocation = error.location
  }

  final case class CouldNotMatchInputTokens(tokens: WithSource[Token], override val location: SourceLocation) extends SyntaxError
  final case class UnterminatedString(override val location: SourceLocation) extends SyntaxError
  final case class InvalidIntegerDigit(ch: WithSource[String], base: BigInt) extends SyntaxError {
    override def location: SourceLocation = ch.location
  }
  final case class InvalidMemberAccess(expr: WithSource[Expr]) extends SyntaxError {
    override def location: SourceLocation = expr.location
  }
  final case class UnexpectedToken(expected: String, actual: WithSource[Token]) extends SyntaxError {
    override def location: SourceLocation = actual.location
  }
  final case class OneOf(errors: Seq[SyntaxError], override val location: SourceLocation) extends SyntaxError
  final case class UnexpectedEndOfFile(expected: String, override val location: SourceLocation) extends SyntaxError
  final case class InvalidMethodModifier(token: WithSource[Modifier]) extends SyntaxError {
    override def location: SourceLocation = token.location
  }
  final case class InvalidTraitModifier(token: WithSource[Modifier]) extends SyntaxError {
    override def location: SourceLocation = token.location
  }
  final case class InvalidConstructorModifier(token: WithSource[Modifier]) extends SyntaxError {
    override def location: SourceLocation = token.location
  }
  final case class InvalidClassModifier(token: WithSource[Modifier]) extends SyntaxError {
    override def location: SourceLocation = token.location
  }
}
