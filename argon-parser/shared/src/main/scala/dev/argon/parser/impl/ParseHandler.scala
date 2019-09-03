package dev.argon.parser.impl

import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util._
import cats._
import cats.data._
import cats.implicits._
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.impl.TopLevelStatement.NSAndImports
import dev.argon.stream._
import dev.argon.stream.builder.{Builder, GenEffect, Generator, Iter}

object ParseHandler {

  def parse[F[_]: Monad, L[_, _]](fileSpec: FileSpec)(chars: L[Char, Unit])(implicit errorHandler: ParseErrorHandler[F, NonEmptyVector[SyntaxError]], iter: Iter[F, L, Unit]): Generator[F, SourceAST, Unit] =
    Characterizer.characterize[F, L](chars)
      .buffer(1024 * 4)
      .into(Lexer.lex(_))
      .buffer(1024)
      .into(ArgonParser.parse(_))
      .into(buildSourceAST(fileSpec)(_))

  private def buildSourceAST[F[_], L[_, _]](fileSpec: FileSpec)(stmts: L[TopLevelStatement, Unit])(implicit iter: Iter[F, L, Unit]): Generator[F, SourceAST, Unit] =
    new Generator[F, SourceAST, Unit] {
      override def create[G[_]](implicit genEffect: GenEffect[F, G], builder: Builder[G, SourceAST]): G[Unit] =
        genEffect.liftFuncState(iter.foldLeftMHandlerFunc(stmts))(TopLevelStatement.defaultNSAndImports) { (state, stmt) =>
          val (newState, opt) = TopLevelStatement.accumulate(fileSpec)(state, stmt)
          opt.fold(builder.pure(()))(builder.append).map { _ => newState }
        }.map { _ => }
    }

}
