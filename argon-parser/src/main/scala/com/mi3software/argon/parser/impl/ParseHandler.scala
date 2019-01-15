package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import com.mi3software.argon.util._
import scalaz._
import Scalaz._
import com.mi3software.argon.grammar.SyntaxErrorReporter
import com.mi3software.argon.parser.impl.TopLevelStatement.NSAndImports
import com.mi3software.argon.util.stream.{ArStream, StreamTransformation}

object ParseHandler {

  type ErrorReporter[F[_]] = SyntaxErrorReporter[F, SyntaxError]

  def parse[F[_]: Monad : ErrorReporter](fileSpec: FileSpec)(text: ArStream[F, Char, Unit]): ArStream[F, SourceAST, Unit] =
    Characterizer.characterize(text)
      .transformWith(Lexer.lex)
      .transformWith(ArgonParser.parse)
      .transformWith(buildSourceAST(fileSpec))

  private def buildSourceAST[F[_]](fileSpec: FileSpec): StreamTransformation.Single[TopLevelStatement, Unit, SourceAST, Unit] =
    new StreamTransformation.Single[TopLevelStatement, Unit, SourceAST, Unit] {
      override protected type S = NSAndImports
      override protected val initialState: NSAndImports = TopLevelStatement.defaultNSAndImports

      override protected def processItem(state: NSAndImports, item: TopLevelStatement): (NSAndImports, Vector[SourceAST]) = {
        val (newState, opt) = TopLevelStatement.accumulate(fileSpec)(state, item)
        (newState, opt.toVector)
      }


      override protected def processResult(state: NSAndImports, result: Unit): (Unit, Vector[SourceAST]) =
        ((), Vector.empty)
    }

}
