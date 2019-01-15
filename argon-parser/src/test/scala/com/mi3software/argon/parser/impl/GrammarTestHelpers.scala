package com.mi3software.argon.parser.impl

import com.mi3software.argon.util.{FilePosition, NonEmptyVector, SourceLocation, WithSource}
import scalaz._
import com.mi3software.argon.grammar.Grammar
import Grammar.{GrammarResultComplete, GrammarResultTransform, ParseOptions}

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

    def impl[A](trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, A, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      tokens match {
        case head +: tail =>
          trans.grammar.parseTokens(NonEmptyVector.of(head), trans.parseOptions).transformComplete(trans.f) match {
            case result: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =>
              result.map { case (unusedTokens, t) => (unusedTokens ++ tail, t) }
            case trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, a, T] =>
              impl(trans)(tail)
          }

        case _ =>
          trans.grammar
            .parseEnd(
              endPos,
              trans.parseOptions
            )
            .transformComplete(trans.f)
            .completeResult(endPos)
      }

    impl(GrammarResultTransform(grammar)(ParseOptions(Set.empty, None, grammarFactory))(identity))(tokens)
  }
}
