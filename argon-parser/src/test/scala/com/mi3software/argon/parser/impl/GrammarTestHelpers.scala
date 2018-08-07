package com.mi3software.argon.parser.impl

import com.mi3software.argon.util.{FilePosition, SourceLocation, WithSource}
import scalaz._
import com.mi3software.argon.parser.impl.Grammar.{GrammarResultComplete, GrammarResultTransform, ParseOptions, ParseState}

trait GrammarTestHelpers {

  type TToken
  type TSyntaxError
  type TLabel

  type TGrammar[T] = Grammar[TToken, TSyntaxError, TLabel, T]

  def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

  protected def parse[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: TToken*): Either[NonEmptyList[TSyntaxError], (Vector[TToken], T)] =
    parseTokens(grammar)(
      tokens.zipWithIndex.map { case (value, i) => WithSource(value, SourceLocation(FilePosition(1, i + 1), FilePosition(1, i + 2))) }.toVector
    )
      .toEither
      .map { case (ParseState(remTokens, _), res) => (remTokens.map { _.value }, res.value) }


}

trait GrammarTestHelpersEntireSequence extends GrammarTestHelpers {
  override def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
    if(tokens.isEmpty)
      grammar
        .parseEnd(
          FilePosition(1, 1),
          ParseOptions(Set.empty, None)
        )
    else
      grammar
        .parseTokens(
          ParseState(
            tokens,
            FilePosition(1, 1)
          ),
          ParseOptions(Set.empty, None)
        )
        .completeResult
}

trait GrammarTestHelpersSingleTokens extends GrammarTestHelpers {
  override def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = {
    def impl[A](trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, A, T])(tokens: Vector[WithSource[TToken]]): GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      tokens match {
        case head +: tail =>
          trans.grammar.parseTokens(ParseState(Vector(head), trans.pos), trans.parseOptions).transformComplete(trans.f) match {
            case result: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =>
              result.map { case (ParseState(unusedTokens, pos), t) => (ParseState(unusedTokens ++ tail, pos), t) }
            case trans: GrammarResultTransform[TToken, TSyntaxError, TLabel, a, T] =>
              impl(trans)(tail)
          }

        case _ =>
          trans.grammar
            .parseEnd(
              trans.pos,
              trans.parseOptions
            )
            .transformComplete(trans.f)
            .completeResult
      }

    impl(GrammarResultTransform(grammar)(ParseOptions(Set.empty, None))(FilePosition(1, 1))(identity))(tokens)
  }
}
