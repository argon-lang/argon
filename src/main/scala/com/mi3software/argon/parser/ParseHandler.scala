package com.mi3software.argon.parser

import com.mi3software.argon.parser.impl._
import com.mi3software.argon.util._

import scalaz._
import Grammar.Operators._

final class ParseHandler {

  private val lexer = new Lexer()
  private val parser = new Parser()

  import lexer.errorFactory

  def parse(fileSpec: FileSpec): SequenceHandler[Char, Any, NonEmptyList[SyntaxErrorData] \/ Vector[WithSource[SourceAST]]] = {

    def convertError(syntaxError: SyntaxError): SyntaxErrorData =
      SyntaxErrorData(fileSpec, syntaxError)

    val grammar = lexer.token.streamInto(parser.ruleTopLevelStatementList) {
      case Some(token) => token
    }

    Characterizer.characterize(grammar.sequenceHandler).map {
      case -\/(error) => -\/(NonEmptyList(convertError(error)))
      case \/-(-\/(errors)) => -\/(errors.map(convertError))
      case \/-(\/-(NonEmptyList(WithSource(result, _), INil()))) =>
        \/-(TopLevelStatement.toSourceAST(fileSpec)(result))

      case \/-(\/-(NonEmptyList(WithSource(_, location), ICons(_, _)))) =>
        -\/(NonEmptyList(convertError(SyntaxError.AmbiguousParse(location))))

    }
  }




}
