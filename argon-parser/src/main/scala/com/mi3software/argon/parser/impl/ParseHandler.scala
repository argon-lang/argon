package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import com.mi3software.argon.util._
import scalaz._
import fs2._

object ParseHandler {

  private val lexer = new Lexer()
  private val parser = new Parser()

  def parse(fileSpec: FileSpec)(content: String): NonEmptyList[SyntaxErrorData] \/ Vector[SourceAST] = {

    def convertError(syntaxError: SyntaxError): SyntaxErrorData =
      SyntaxErrorData(fileSpec, syntaxError)

    val charsWithLen = Characterizer.characterize(Stream(content: _*)).toVector

    val (chars, _) = charsWithLen.foldLeft((Vector[WithSource[String]](), FilePosition(1, 1))) {
      case ((acc, pos @ FilePosition(startLine, startPos)), WithLength(value, length, lengthAfterNewLine)) =>
        val newPos = if(lengthAfterNewLine) FilePosition(startLine + 1, length) else FilePosition(startLine, startPos + length)
        (acc :+ WithSource(value, SourceLocation(pos, newPos)), newPos)
    }

    \/.fromEither(
      lexer
        .lex(chars)
        .flatMap(parser.parse)
        .map(TopLevelStatement.toSourceAST(fileSpec))
    )
      .leftMap { _.map(convertError) }
  }



}
