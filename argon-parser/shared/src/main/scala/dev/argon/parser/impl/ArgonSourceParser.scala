package dev.argon.parser.impl

import dev.argon.parser.{Stmt, SyntaxError, SyntaxErrorData}
import dev.argon.util._
import zio.Chunk
import zio.stream._

object ArgonSourceParser {

  def parse[E](fileSpec: FileSpec): ZChannel[Any, E, Chunk[Char], Any, E | SyntaxError, Chunk[Stmt], Any] =
    Characterizer.characterize
      .pipeTo(Lexer.lex)
      .pipeTo(ArgonParser.parse)
  
}
