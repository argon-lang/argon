package dev.argon.esexpr.parser

import dev.argon.esexpr.ESExpr
import dev.argon.grammar.Characterizer
import dev.argon.util.{WithSource, ZChannelUtil}
import zio.Chunk
import zio.stream.ZChannel

object ESExprTextReader {
  def read[E](fileName: Option[String]): ZChannel[Any, E, Chunk[String], Any, E | ESExprLexer.LexerError | ESExprParser.ParseError, Chunk[WithSource[ESExpr]], Any] =
    ZChannel.identity[E, Chunk[String], Any].mapOut(c => c.flatMap(s => Chunk.fromArray(s.toCharArray.nn))) >>>
      Characterizer.characterize(fileName) >>>
      ESExprLexer.lex[E](fileName) >>>
      ESExprParser.parse[E | ESExprLexer.LexerError](fileName)

}
