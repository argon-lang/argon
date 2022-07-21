package dev.argon.parser.impl

import dev.argon.parser.{Stmt, SyntaxError, SyntaxErrorData}
import dev.argon.parser.tubespec.ModulePatternMapping
import dev.argon.util.*
import zio.Chunk
import zio.stream.*

object ArgonSourceParser {

  def parse[E >: SyntaxError]: ZChannel[Any, E, Chunk[Char], Any, E, Chunk[Stmt], Any] =
    Characterizer.characterize
      .pipeTo(Lexer.lex)
      .pipeTo(ArgonParser.parse)

  def parseTubeSpec[E >: SyntaxError]: ZChannel[Any, E, Chunk[Char], Any, E, Chunk[ModulePatternMapping], Any] =
    Characterizer.characterize
      .pipeTo(Lexer.lex)
      .pipeTo(ArgonParser.parseTubeSpec)

}
