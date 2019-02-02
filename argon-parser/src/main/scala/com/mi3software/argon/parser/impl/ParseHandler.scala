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
      .chunkBuffer(1024 * 8)
      .transformWith(Lexer.lex)
      .chunkBuffer(1024 * 2)
      .transformWith(ArgonParser.parse)
      .transformWith(buildSourceAST(fileSpec))

  private def buildSourceAST[F[_]](fileSpec: FileSpec): StreamTransformation.Single[F, TopLevelStatement, Unit, SourceAST, Unit] =
    new StreamTransformation.Single[F, TopLevelStatement, Unit, SourceAST, Unit] {
      override type S = NSAndImports
      override val initialState: NSAndImports = TopLevelStatement.defaultNSAndImports

      override protected def processItem[S2](state: NSAndImports, state2: S2, item: TopLevelStatement)(f: (S2, NonEmptyVector[SourceAST]) => F[S2])(implicit monadInstance: Monad[F]): F[(NSAndImports, S2)] = {
        val (newState, opt) = TopLevelStatement.accumulate(fileSpec)(state, item)

        opt
          .traverse { tls => f(state2, NonEmptyVector.of(tls)) }
          .map { newState2 => (newState, newState2.getOrElse(state2)) }
      }

      override def processResult[S2](state: NSAndImports, state2: S2, result: Unit)(f: (S2, NonEmptyVector[SourceAST]) => F[S2])(implicit monadInstance: Monad[F]): F[(Unit, S2)] =
        ((), state2).point[F]

    }

}
