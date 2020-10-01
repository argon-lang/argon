package dev.argon.parser.impl

import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.compiler._
import dev.argon.compiler.loaders.SourceParser
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util.FileSpec
import zio.{IO, ULayer, ZIO, ZLayer}
import zio.stream._
import dev.argon.stream.StreamExtensions._

object ArgonSourceParser {

  def live: ULayer[SourceParser] = ZLayer.succeed(
    new SourceParser.Service {


      override def parse(fileSpec: FileSpec)(sourceCode: Stream[CompilationError, Char]): Stream[CompilationError, SourceAST] = {

        def toCompileError(error: SyntaxError): CompilationError =
          DiagnosticError.SyntaxCompilerError(SyntaxErrorData(fileSpec, error))

        sourceCode.transformWith(
          ParseHandler.parse(fileSpec)
            .mapError(toCompileError)
        )
      }
    }
  )

}
