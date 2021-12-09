package dev.argon.grammar

import dev.argon.util.{FilePosition, SourceLocation, WithSource}

sealed trait GrammarError[+TToken, +TTokenCategory] {
  def location: SourceLocation
}

object GrammarError {

  final case class ExpectedEndOfFile[TToken](token: WithSource[TToken]) extends GrammarError[TToken, Nothing] {
    override def location: SourceLocation = token.location
  }

  final case class UnexpectedToken[TToken, TTokenCategory](expectedCategory: TTokenCategory, token: WithSource[TToken])
      extends GrammarError[TToken, TTokenCategory] {
    override def location: SourceLocation = token.location
  }

  final case class UnexpectedEndOfFile[TTokenCategory](expectedCategory: TTokenCategory, position: FilePosition)
      extends GrammarError[Nothing, TTokenCategory] {
    override def location: SourceLocation = SourceLocation(position, FilePosition(position.line, position.position + 1))
  }

  final case class InfiniteRecursion(position: FilePosition) extends GrammarError[Nothing, Nothing] {
    override def location: SourceLocation = SourceLocation(position, FilePosition(position.line, position.position + 1))
  }

}
