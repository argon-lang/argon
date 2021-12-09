package dev.argon.parser.impl

import dev.argon.util.*
import dev.argon.grammar.Grammar
import Grammar.{GrammarResult, GrammarResultComplete, GrammarResultSuspend, ParseOptions}
import zio.*

trait GrammarTestHelpers {

  type TToken
  type TSyntaxError
  type TLabel[_]

  type TGrammar[T] = Grammar[TToken, TSyntaxError, TLabel, T]

  protected val grammarFactory: Grammar.GrammarFactory[TToken, TSyntaxError, TLabel]

  def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Chunk[WithSource[TToken]])
    : GrammarResultComplete[TToken, TSyntaxError, TLabel, T]

  protected def parse[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: TToken*)
    : Either[NonEmptyChunk[TSyntaxError], (Chunk[TToken], T)] =
    parseTokens(grammar)(
      Chunk.fromIterable(tokens.zipWithIndex.map { case (value, i) =>
        WithSource(value, SourceLocation(FilePosition(1, i + 1), FilePosition(1, i + 2)))
      })
    )
      .toEither
      .map { case (remTokens, res) => (remTokens.map { _.value }, res.value) }

}

trait GrammarTestHelpersEntireSequence extends GrammarTestHelpers {

  override def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Chunk[WithSource[TToken]])
    : GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
    tokens match {
      case ChunkUnCons(tokens) =>
        grammar
          .parseTokens(
            tokens,
            ParseOptions(Set.empty, None, grammarFactory),
          )
          .completeResult(FilePosition(1, tokens.size + 1))

      case _ =>
        grammar
          .parseEnd(
            FilePosition(1, 1),
            ParseOptions(Set.empty, None, grammarFactory),
          )
    }

}

trait GrammarTestHelpersSingleTokens extends GrammarTestHelpers {

  override def parseTokens[T](grammar: Grammar[TToken, TSyntaxError, TLabel, T])(tokens: Chunk[WithSource[TToken]])
    : GrammarResultComplete[TToken, TSyntaxError, TLabel, T] = {
    val endPos = FilePosition(1, tokens.size + 1)

    def handleResult(result: GrammarResult[TToken, TSyntaxError, TLabel, T], tail: Chunk[WithSource[TToken]])
      : GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      result match {
        case result: GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =>
          result.map { case (unusedTokens, t) => (unusedTokens ++ tail, t) }
        case suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T] =>
          impl(suspend)(tail)
      }

    def impl(suspend: GrammarResultSuspend[TToken, TSyntaxError, TLabel, T])(tokens: Chunk[WithSource[TToken]])
      : GrammarResultComplete[TToken, TSyntaxError, TLabel, T] =
      tokens match {
        case ChunkUnCons(tokens) =>
          handleResult(suspend.continue(NonEmptyChunk(tokens.head)), tokens.tail)

        case _ =>
          suspend.completeResult(endPos)
      }

    tokens match {
      case ChunkUnCons(tokens) =>
        handleResult(
          grammar.parseTokens(NonEmptyChunk(tokens.head), ParseOptions(Set.empty, None, grammarFactory)),
          tokens.tail,
        )

      case _ =>
        grammar.parseEnd(endPos, ParseOptions(Set.empty, None, grammarFactory))
    }
  }

}
