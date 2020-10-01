package dev.argon.parser.impl

import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util._
import cats._
import cats.data._
import cats.implicits._
import dev.argon.parser.impl.TopLevelStatement.NSAndImports
import dev.argon.stream._
import zio.{Chunk, IO, NonEmptyChunk, ZIO}
import zio.stream.ZStream

private[impl] object ParseHandler {

  def parse(fileSpec: FileSpec): StreamTransformation[Any, SyntaxError, Char, Unit, SourceAST, Unit] =
    Characterizer.characterize
      .andThen(Lexer.lex)
      .andThen(ArgonParser.parse)
      .andThen(buildSourceAST(fileSpec))

  private def buildSourceAST(fileSpec: FileSpec): StreamTransformation[Any, SyntaxError, TopLevelStatement, Unit, SourceAST, Unit] =
    new StreamTransformation[Any, SyntaxError, TopLevelStatement, Unit, SourceAST, Unit] {
      override type TransformState = NSAndImports

      override def start: IO[SyntaxError, NSAndImports] = IO.succeed(TopLevelStatement.defaultNSAndImports)

      override def consume(state: NSAndImports, values: NonEmptyChunk[TopLevelStatement]): IO[SyntaxError, (NSAndImports, Chunk[SourceAST])] = {
        val (state2, chunkOpt) = values.toChunk.mapAccum(state)(TopLevelStatement.accumulate(fileSpec))
        IO.succeed(state2, chunkOpt.collect { case Some(x) => x })
      }

      override def finish(state: NSAndImports, value: Unit): IO[SyntaxError, (Chunk[SourceAST], Unit)] =
        IO.succeed((Chunk.empty, ()))
    }

}
