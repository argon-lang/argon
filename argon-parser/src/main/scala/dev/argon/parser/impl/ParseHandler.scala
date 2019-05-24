package dev.argon.parser.impl

import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util._
import cats._
import cats.data._
import cats.implicits._
import dev.argon.parser.impl.TopLevelStatement.NSAndImports
import dev.argon.util.stream._

object ParseHandler {

  def parse(fileSpec: FileSpec): StreamTransformation[PureEffect, Any, NonEmptyVector[SyntaxError], Char, Unit, SourceAST, Unit] =
    (Characterizer.characterize : StreamTransformation[PureEffect, Any, NonEmptyVector[SyntaxError], Char, Unit, WithSource[String], FilePosition])
      .buffer(1024 * 8)
      .into(Lexer.lex)
      .buffer(1024 * 2)
      .into(ArgonParser.parse)
      .into(buildSourceAST(fileSpec))

  private def buildSourceAST(fileSpec: FileSpec): StreamTransformation[PureEffect, Any, NonEmptyVector[SyntaxError], TopLevelStatement, Unit, SourceAST, Unit] =
    new StreamTransformation.PureSingle[NonEmptyVector[SyntaxError], TopLevelStatement, Unit, SourceAST, Unit] {
      override type State = NSAndImports


      override def initialPure: NSAndImports = TopLevelStatement.defaultNSAndImports

      override def stepSinglePure(state: NSAndImports, item: TopLevelStatement): StepPure[NSAndImports, Nothing, TopLevelStatement, SourceAST, Unit] = {
        val (newState, opt) = TopLevelStatement.accumulate(fileSpec)(state, item)

        opt match {
          case Some(value) => Step.Produce(newState, value, Vector.empty)
          case None => Step.Continue(newState)
        }
      }

      override def endPure(s: NSAndImports, result: Unit): (Vector[SourceAST], Either[Nothing, Unit]) =
        (Vector.empty, Right(()))

    }

}
