package dev.argon.parser.impl

import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util._
import cats._
import cats.data._
import cats.implicits._
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.impl.TopLevelStatement.NSAndImports
import dev.argon.stream._
import dev.argon.stream.builder.{Source, ZStreamSource}
import zio.ZIO
import zio.stream.ZStream

object ParseHandler {

  def parse[R, E](fileSpec: FileSpec)(chars: ZStream[R, E, Char])(implicit errorHandler: ParseErrorHandler[ZIO[R, E, *], NonEmptyVector[SyntaxError]]): ZStream[R, E, SourceAST] =
    Characterizer.characterize(new ZStreamSource(chars))
      .bufferVector(1024 * 4)
      .into(Lexer.lex[R, E])
      .bufferVector(1024)
      .into(ArgonParser.parse[R, E])
      .into(buildSourceAST[R, E](fileSpec)(_))
      .toZStream

  private def buildSourceAST[R, E](fileSpec: FileSpec)(stmts: Source[R, E, TopLevelStatement, Unit]): Source[R, E, SourceAST, Unit] =
    new Source[R, E, SourceAST, Unit] {
      override def foreach[R1 <: R, E1 >: E](f: SourceAST => ZIO[R1, E1, Unit]): ZIO[R1, E1, Unit] =
        stmts.foldLeftM[R1, E1, NSAndImports](TopLevelStatement.defaultNSAndImports) { (state, stmt) =>
          val (newState, opt) = TopLevelStatement.accumulate(fileSpec)(state, stmt)
          ZIO.foreach(opt)(f).as(newState)
        }.unit
    }

}
