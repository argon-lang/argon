package dev.argon.parser.impl

import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util._
import cats._
import cats.data._
import cats.implicits._
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.impl.TopLevelStatement.NSAndImports
import dev.argon.stream._
import dev.argon.stream.builder.{GenEffect, Sink, Source}

object ParseHandler {

  def parse[F[_]: Monad](fileSpec: FileSpec)(chars: Source[F, Char, Unit])(implicit errorHandler: ParseErrorHandler[F, NonEmptyVector[SyntaxError]]): Source[F, SourceAST, Unit] =
    Characterizer.characterize[F](chars)
      .bufferVector(1024 * 4)
      .into(Lexer.lex(_))
      .bufferVector(1024)
      .into(ArgonParser.parse(_))
      .into(buildSourceAST(fileSpec)(_))

  private def buildSourceAST[F[_]: Monad](fileSpec: FileSpec)(stmts: Source[F, TopLevelStatement, Unit]): Source[F, SourceAST, Unit] =
    new Source[F, SourceAST, Unit] {

      override protected val monadF: Monad[F] = implicitly

      override protected def generateImpl[G[_] : Monad](sink: Sink[G, SourceAST])(implicit genEffect: GenEffect[F, G]): G[Unit] =
        stmts.foldLeftG(TopLevelStatement.defaultNSAndImports) { (state, stmt) =>
          val (newState, opt) = TopLevelStatement.accumulate(fileSpec)(state, stmt)
          opt.fold(().pure[G])(sink.consume).map { _ => newState }
        }.map { _ => }
    }

}
