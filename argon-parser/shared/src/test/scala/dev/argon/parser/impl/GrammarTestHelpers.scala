package dev.argon.parser.impl

import dev.argon.util.{FilePosition, SourceLocation, WithSource}
import cats.data.NonEmptyVector
import dev.argon.grammar.Grammar
import Grammar.{GrammarResult, GrammarResultComplete, GrammarResultSuspend, ParseOptions}

trait GrammarTestHelpers {

  type TToken
  type TSyntaxError
  type TLabel <: Grammar.RuleLabel

  type TGrammar[T] = Grammar[TToken, TSyntaxError, TLabel, T]

  protected val grammarFactory: Grammar.GrammarFactory[TToken, TSyntaxError, TLabel]

  def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

  protected def parse[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: TToken*): Either[NonEmptyVector[TSyntaxError], (Vector[TToken], T)] =
    parseTokens(grammar)(
      tokens.zipWithIndex.map { case (value, i) => WithSource(value, SourceLocation(FilePosition(1, i + 1), FilePosition(1, i + 2))) }.toVector
    )
      .toEither
      .map { case (remTokens, res) => (remTokens.map { _.value }, res.value) }


}

trait GrammarTestHelpersEntireSequence extends GrammarTestHelpers {
  override def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
    tokens match {
      case head +: tail =>
        grammar
          .parseTokens(
            NonEmptyVector(head, tail),
            ParseOptions(Set.empty, None, grammarFactory)
          )
          .completeResult(FilePosition(1, tokens.size + 1))

      case Vector() =>
        grammar
          .parseEnd(
            FilePosition(1, 1),
            ParseOptions(Set.empty, None, grammarFactory)
          )
    }
}

trait GrammarTestHelpersSingleTokens extends GrammarTestHelpers {
  override def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = {
    val endPos = FilePosition(1, tokens.size + 1)

    def handleResult(result: GrammarResult[TToken, TSyntaxError, TLabel, T], tail: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      result match {
        case result: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =>
          result.map { case (unusedTokens, t) => (unusedTokens ++ tail, t) }
        case suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T] =>
          impl(suspend)(tail)
      }

    def impl(suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      tokens match {
        case head +: tail =>
          handleResult(suspend.continue(NonEmptyVector.of(head)), tail)

        case Vector() =>
          suspend.completeResult(endPos)
      }

    tokens match {
      case head +: tail =>
        handleResult(grammar.parseTokens(NonEmptyVector.of(head), ParseOptions(Set.empty, None, grammarFactory)), tail)

      case Vector() =>
        grammar.parseEnd(endPos, ParseOptions(Set.empty, None, grammarFactory))
    }
  }
}
