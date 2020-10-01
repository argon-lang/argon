package dev.argon.parser.impl

import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.compiler.{Compilation, CompilationError, CompError}
import dev.argon.compiler.loaders.SourceParser
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util.FileSpec
import zio.{IO, ULayer, ZIO, ZLayer}
import zio.stream._
import dev.argon.stream.StreamExtensions._

object ArgonSourceParser {

  def live: ULayer[SourceParser] = ZLayer.succeed(
    new SourceParser.Service {


      override def parse(fileSpec: FileSpec)(sourceCode: Stream[CompError, Char]): Stream[CompError, SourceAST] = {

        def toCompileError(error: SyntaxError): CompilationError =
          CompilationError.SyntaxCompilerError(SyntaxErrorData(fileSpec, error))

        sourceCode.transformWith(
          ParseHandler.parse(fileSpec)
            .mapError(toCompileError)
        )
      }
    }
  )

}
