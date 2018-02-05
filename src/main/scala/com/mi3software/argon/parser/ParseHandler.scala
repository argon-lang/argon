package com.mi3software.argon.parser

import com.mi3software.argon.parser.impl._
import com.mi3software.argon.util._

import scalaz._

final class ParseHandler {

  private val lexer = new Lexer()
  private val parser = new Parser()

  def parse(fileSpec: FileSpec)(content: String): NonEmptyList[SyntaxErrorData] \/ Vector[WithSource[SourceAST]] = {

    def convertError(syntaxError: SyntaxError): SyntaxErrorData =
      SyntaxErrorData(fileSpec, syntaxError)

    val chars = Characterizer.characterize(content)

    \/.fromEither(
      lexer
        .lex(chars)
        .flatMap(parser.parse)
        .map(TopLevelStatement.toSourceAST(fileSpec))
    )
      .leftMap { _.map(convertError) }
  }



}
