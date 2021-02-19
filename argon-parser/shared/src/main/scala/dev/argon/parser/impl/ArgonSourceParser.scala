package dev.argon.parser.impl

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import dev.argon.util.FileSpec
import zio.stream._
import dev.argon.stream.StreamExtensions._

object ArgonSourceParser {

  def parse[R](fileSpec: FileSpec)(sourceCode: ZStream[R, CompilationError, Char]): ZStream[R, CompilationError, SourceAST] = {

    def toCompileError(error: SyntaxError): CompilationError =
      DiagnosticError.SyntaxCompilerError(SyntaxErrorData(fileSpec, error))

    sourceCode.transformWith(
      ParseHandler.parse(fileSpec)
        .mapError(toCompileError)
    )
  }

}
