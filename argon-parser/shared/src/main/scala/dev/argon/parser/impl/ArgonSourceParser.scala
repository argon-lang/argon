package dev.argon.parser.impl

import dev.argon.parser.{Stmt, SyntaxError, SyntaxErrorData}
import dev.argon.util.*
import zio.Chunk
import zio.stream.*

object ArgonSourceParser {

  def parse[E](fileSpec: FileSpec): ZChannel[Any, E, Chunk[Char], Any, E | SyntaxError, Chunk[Stmt], Any] =
    Characterizer.characterize
      .pipeTo(Lexer.lex)
      .pipeTo(ArgonParser.parse)

}
