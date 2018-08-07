package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import com.mi3software.argon.util._
import scalaz._
import Scalaz._
import fs2._
import shims._

object ParseHandler {

  private val lexer = new Lexer()
  private val parser = new Parser()

  def decodeText[F[_]]: Pipe[F, Byte, Char] =
    _
      .through(fs2.text.utf8Decode)
      .mapSegments { _.flatMap { str => Segment(str: _*) }.mapResult { _ => () } }


  def parse[F[_]: Monad](fileSpec: FileSpec): Pipe[EitherT[F, NonEmptyList[SyntaxError], ?], Char, SourceAST] =
    _
      .through(Characterizer.characterize)
      .through(lexer.lex)
      .through(parser.parse)
      .through(buildSourceAST(fileSpec))

  private def buildSourceAST[F[_]](fileSpec: FileSpec): Pipe[F, TopLevelStatement, SourceAST] =
    _
      .pull
      .scanSegments(TopLevelStatement.defaultNSAndImports) { (ns, seg) =>
        seg
          .mapAccumulate(ns)(TopLevelStatement.accumulate(fileSpec))
          .mapResult { case (_, ns) => ns }
      }
      .stream
      .unNone


  def addSyntaxErrorEffect[F[_] : Functor]: cats.~>[F, EitherT[F, NonEmptyList[SyntaxError], ?]] = new cats.~>[F, EitherT[F, NonEmptyList[SyntaxError], ?]] {
    override def apply[A](fa: F[A]): EitherT[F, NonEmptyList[SyntaxError], A] =
      EitherT(fa.map(\/.right))
  }


  def convertSyntaxErrorToCompilationError[F[_]: Functor](fileSpec: FileSpec): cats.~>[EitherT[F, NonEmptyList[SyntaxError], ?], EitherT[F, NonEmptyList[SyntaxErrorData], ?]] =
    new cats.~>[EitherT[F, NonEmptyList[SyntaxError], ?], EitherT[F, NonEmptyList[SyntaxErrorData], ?]] {
      override def apply[A](fa: EitherT[F, NonEmptyList[SyntaxError], A]): EitherT[F, NonEmptyList[SyntaxErrorData], A] =
        fa.leftMap(_.map(error => SyntaxErrorData(fileSpec, error)))
    }

}
