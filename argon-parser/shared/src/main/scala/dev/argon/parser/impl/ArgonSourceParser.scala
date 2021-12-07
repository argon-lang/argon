package dev.argon.parser.impl

import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util._
import zio.Chunk
import zio.stream._
import dev.argon.parser.impl.TopLevelStatement.NSAndImports

object ArgonSourceParser {

  def parse[E](fileSpec: FileSpec): ZChannel[Any, E, Chunk[Char], Any, E | SyntaxError, Chunk[SourceAST], Any] =
    Characterizer.characterize
      .pipeTo(Lexer.lex)
      .pipeTo(ArgonParser.parse)
      .pipeTo(buildSourceAST(fileSpec))

  private def buildSourceAST[E](fileSpec: FileSpec): ZChannel[Any, E, Chunk[TopLevelStatement], Any, E, Chunk[SourceAST], Any] =
    ZChannelUtil.mapAccumOption(TopLevelStatement.defaultNSAndImports)(TopLevelStatement.accumulate(fileSpec))
  
}
