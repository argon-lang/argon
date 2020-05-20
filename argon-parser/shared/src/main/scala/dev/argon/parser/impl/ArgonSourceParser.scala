package dev.argon.parser.impl

import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.compiler.{Compilation, CompilationError, ErrorList}
import dev.argon.compiler.loaders.SourceParser
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util.FileSpec
import zio.{IO, ULayer, ZIO, ZLayer, stream}

object ArgonSourceParser {

  def live: ULayer[SourceParser] = ZLayer.succeed(
    new SourceParser.Service {


      override def parse(fileSpec: FileSpec)(sourceCode: stream.Stream[ErrorList, Char]): stream.Stream[ErrorList, SourceAST] = {

        def toCompileError(error: SyntaxError): CompilationError =
          CompilationError.SyntaxCompilerError(SyntaxErrorData(fileSpec, error))

        implicit val errorHandler: ParseErrorHandler[IO[ErrorList, *], NonEmptyVector[SyntaxError]] =
          new ParseErrorHandler[IO[ErrorList, *], NonEmptyVector[SyntaxError]] {
            override def raiseError[A](errors: NonEmptyVector[SyntaxError]): IO[ErrorList, A] =
              Compilation.forErrors(NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList))
          }

        ParseHandler.parse(fileSpec)(sourceCode)
      }
    }
  )

}
