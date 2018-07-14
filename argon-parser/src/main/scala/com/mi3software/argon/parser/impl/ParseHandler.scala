package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import com.mi3software.argon.util._
import scalaz._

object ParseHandler {

  private val lexer = new Lexer()
  private val parser = new Parser()

  def parse(fileSpec: FileSpec)(content: String): NonEmptyList[SyntaxErrorData] \/ Vector[SourceAST] = {

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
