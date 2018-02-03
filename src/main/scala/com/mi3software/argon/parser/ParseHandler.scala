package com.mi3software.argon.parser

import com.mi3software.argon.parser.impl._
import com.mi3software.argon.util._

import scalaz._
import Grammar.Operators._

final class ParseHandler {

  private val lexer = new Lexer()
  private val parser = new Parser()

  import lexer.errorFactory

  def parse(fileSpec: FileSpec): SequenceHandler[Char, Any, NonEmptyList[SyntaxError] \/ Vector[WithSource[SourceAST]]] = {

    val grammar = lexer.token.streamInto(parser.ruleTopLevelStatementList) {
      case Some(token) => token
    }

    Characterizer.characterize(grammar.sequenceHandler).map {
      case -\/(error) => -\/(NonEmptyList(error))
      case \/-(-\/(errors)) => -\/(errors)
      case \/-(\/-(NonEmptyList(WithSource(result, _), INil()))) =>
        \/-(TopLevelStatement.toSourceAST(fileSpec)(result))

      case \/-(\/-(NonEmptyList(WithSource(_, location), ICons(_, _)))) =>
        -\/(NonEmptyList(SyntaxError.AmbiguousParse(location)))

    }
  }




}
