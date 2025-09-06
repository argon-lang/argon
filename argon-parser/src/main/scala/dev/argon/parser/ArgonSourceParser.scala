package dev.argon.parser

import dev.argon.ast.ModuleDeclaration
import zio.stream.*
import zio.{Scope, ZIO}

object ArgonSourceParser {  
  def parse[R, E >: SyntaxError](fileName: Option[String], stream: ZStream[R, E, String]): ZIO[R, E, ModuleDeclaration] =
    ZIO.scoped(
      for
        textReader <- StreamTextReader.make(stream)
        lexer <- ArgonLexer.make(fileName)(textReader)
        parser <- ArgonParser.make(fileName)(lexer)
        module <- parser.parse
      yield module
    )
}
