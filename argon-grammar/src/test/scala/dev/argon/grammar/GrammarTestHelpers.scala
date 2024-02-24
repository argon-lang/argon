package dev.argon.grammar

import dev.argon.grammar.Grammar
import dev.argon.grammar.Grammar.{GrammarResult, GrammarResultComplete, GrammarResultSuccess, GrammarResultSuspend, ParseOptions}
import dev.argon.util.*
import zio.*

trait GrammarTestHelpers {

  type TToken
  type TLabel[_]

  type TGrammar[T] = Grammar[TToken, String, FilePosition, TLabel, T]

  protected val grammarFactory: Grammar.GrammarFactory[TToken, String, FilePosition, TLabel]

  def parseTokens[T](grammar: TGrammar[T])(tokens: Chunk[WithSource[TToken]])
    : GrammarResultComplete[TToken, String, FilePosition, TLabel, T]

  protected def parse[T](grammar: TGrammar[T])(tokens: TToken*)
    : Either[NonEmptyChunk[GrammarError[TToken, String, FilePosition]], (Chunk[TToken], T)] =
    parseTokens(grammar)(
      Chunk.fromIterable(tokens.zipWithIndex.map { case (value, i) =>
        WithLocation(value, Location(None, FilePosition(1, i + 1), FilePosition(1, i + 2)))
      })
    )
      .toEither
      .map { case (remTokens, res) => (remTokens.map { _.value }, res.value) }

}

trait GrammarTestHelpersEntireSequence extends GrammarTestHelpers {

  override def parseTokens[T](grammar: TGrammar[T])(tokens: Chunk[WithSource[TToken]])
    : GrammarResultComplete[TToken, String, FilePosition, TLabel, T] =
    tokens match {
      case ChunkUnCons(tokens) =>
        grammar
          .parseTokens(
            tokens,
            ParseOptions(Set.empty, grammarFactory),
          )
          .completeResult(FilePosition(1, tokens.size + 1))

      case _ =>
        grammar
          .parseEnd(
            FilePosition(1, 1),
            ParseOptions(Set.empty, grammarFactory),
          )
    }

}

trait GrammarTestHelpersSingleTokens extends GrammarTestHelpers {

  override def parseTokens[T](grammar: TGrammar[T])(tokens: Chunk[WithSource[TToken]])
    : GrammarResultComplete[TToken, String, FilePosition, TLabel, T] = {
    val endPos = FilePosition(1, tokens.size + 1)

    def handleResult(result: GrammarResult[TToken, String, FilePosition, TLabel, T], tail: Chunk[WithSource[TToken]])
      : GrammarResultComplete[TToken, String, FilePosition, TLabel, T] =
      result match {
        case result: GrammarResultComplete[TToken, String, FilePosition, TLabel, T] =>
          result.map { case GrammarResultSuccess(unusedTokens, t) => GrammarResultSuccess(unusedTokens ++ tail, t) }
        case suspend: GrammarResultSuspend[TToken, String, FilePosition, TLabel, T] =>
          impl(suspend)(tail)
      }

    def impl(suspend: GrammarResultSuspend[TToken, String, FilePosition, TLabel, T])(tokens: Chunk[WithSource[TToken]])
      : GrammarResultComplete[TToken, String, FilePosition, TLabel, T] =
      tokens match {
        case ChunkUnCons(tokens) =>
          handleResult(suspend.continue(NonEmptyChunk(tokens.head)), tokens.tail)

        case _ =>
          suspend.completeResult(endPos)
      }

    tokens match {
      case ChunkUnCons(tokens) =>
        handleResult(
          grammar.parseTokens(NonEmptyChunk(tokens.head), ParseOptions(Set.empty, grammarFactory)),
          tokens.tail,
        )

      case _ =>
        grammar.parseEnd(endPos, ParseOptions(Set.empty, grammarFactory))
    }
  }

}
