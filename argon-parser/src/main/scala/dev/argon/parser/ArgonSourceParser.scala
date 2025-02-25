package dev.argon.parser

import dev.argon.grammar.Characterizer
import dev.argon.ast.{Stmt, ModulePatternMapping}
import dev.argon.util.*
import zio.Chunk
import zio.stream.*

object ArgonSourceParser {

  def parse(fileName: Option[String]): ZPipeline[Any, SyntaxError, Char, WithSource[Stmt]] =
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
