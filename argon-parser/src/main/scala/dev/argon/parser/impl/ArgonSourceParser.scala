package dev.argon.parser.impl

import dev.argon.grammar.Characterizer
import dev.argon.parser.{Stmt, SyntaxError, SyntaxErrorData}
import dev.argon.parser.tubespec.ModulePatternMapping
import dev.argon.util.*
import zio.Chunk
import zio.stream.*

object ArgonSourceParser {

  def parse(fileName: Option[String]): ZPipeline[Any, SyntaxError, Char, Stmt] =
    ZPipeline.fromChannel(
      Characterizer.characterize(fileName)
        .pipeToOrFail(Lexer.lex(fileName))
        .pipeToOrFail(ArgonParser.parse(fileName))
    )

  def parseTubeSpec(fileName: Option[String]): ZPipeline[Any, SyntaxError, Char, ModulePatternMapping] =
    ZPipeline.fromChannel(
      Characterizer.characterize(fileName)
        .pipeToOrFail(Lexer.lex(fileName))
        .pipeToOrFail(ArgonParser.parseTubeSpec(fileName))
    )

}
