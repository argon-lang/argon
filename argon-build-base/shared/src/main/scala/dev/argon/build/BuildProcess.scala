package dev.argon.build

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.arrow.FunctionK
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.backend.{Backend, ResourceReader}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.impl.ParseHandler
import dev.argon.util.FileSpec
import dev.argon.stream._
import dev.argon.stream.builder.Source
import dev.argon.util.AnyExtensions._
import zio._
import zio.interop.catz._

object BuildProcess {

  def parseInput[R]
  (inputFiles: Source[R, ErrorList, InputFileInfo[R, ErrorList], Unit])
  : Source[R, ErrorList, SourceAST, Unit] =
    inputFiles.flatMap { fileInfo =>
      def toCompileError(error: SyntaxError): CompilationError =
        CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

      implicit val errorHandler: ParseErrorHandler[ZIO[R, ErrorList, *], NonEmptyVector[SyntaxError]] =
        new ParseErrorHandler[ZIO[R, ErrorList, *], NonEmptyVector[SyntaxError]] {
          override def raiseError[A](errors: NonEmptyVector[SyntaxError]): ZIO[R, ErrorList, A] =
            Compilation.forErrors(NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList))
        }

      ParseHandler.parse[R, ErrorList](fileInfo.fileSpec)(fileInfo.dataStream)
    }

  def compile[I <: ResourceIndicator: Tagged]
  (
    backend: Backend
  )(
    sourceASTs: Vector[SourceAST],
    references: Vector[I],
    compilerOptions: CompilerOptions[Id],
    backendOptions: backend.BackendOptions[Id, I]
  )
  : ZManaged[ResourceReader[I], ErrorList, backend.TCompilationOutput] = {
    val input = CompilerInput(
      source = sourceASTs,
      references = references,
      options = compilerOptions,
      backendOptions = backendOptions,
    )

    backend.compile(input)
  }


}
