package dev.argon.grammar

import dev.argon.util.{Location, WithLocation}

sealed trait GrammarError[+TToken, +TTokenCategory, +TPos] {
  def location: Location[TPos]
}

object GrammarError {

  final case class ExpectedEndOfFile[TToken, TPos](token: WithLocation[TToken, TPos]) extends GrammarError[TToken, Nothing, TPos] {
    override def location: Location[TPos] = token.location
  }

  final case class UnexpectedToken[TToken, TTokenCategory, TPos](expectedCategory: TTokenCategory, token: WithLocation[TToken, TPos])
      extends GrammarError[TToken, TTokenCategory, TPos] {
    override def location: Location[TPos] = token.location
  }

  final case class UnexpectedEndOfFile[TTokenCategory, TPos](expectedCategory: TTokenCategory, fileName: Option[String], position: TPos)
      extends GrammarError[Nothing, TTokenCategory, TPos] {
    override def location: Location[TPos] = Location(fileName, position, position)
  }

  final case class InfiniteRecursion[TPos](fileName: Option[String], position: TPos) extends GrammarError[Nothing, Nothing, TPos] {
    override def location: Location[TPos] = Location(fileName, position, position)
  }

}
