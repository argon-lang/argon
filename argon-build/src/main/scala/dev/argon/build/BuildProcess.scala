package dev.argon.build

import java.io.File

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.compiler.backend.Backend
import dev.argon.parser.impl.ParseHandler
import dev.argon.stream.{ArStream, MapError, StreamTransformation}
import dev.argon.util.FileSpec
import dev.argon.stream._
import dev.argon.util.AnyExtensions._

object BuildProcess {

  def parseInput[F[-_, +_, +_], R]
  (inputFiles: ArStream[F, R, NonEmptyList[CompilationError], InputFileInfo[F, R]])
  (implicit
    monadErrorNothing: MonadError[F[Any, Nothing, ?], Nothing],
    monadError: MonadError[F[Any, NonEmptyVector[SyntaxError], ?], NonEmptyVector[SyntaxError]],
    mapError: MapError[F],
    compInstance: CompilationRE[F, R]
  )
  : ArStream[F, R, NonEmptyList[CompilationError], SourceAST] =
    inputFiles.flatMap { fileInfo =>
      def toCompileError(error: SyntaxError): CompilationError =
        CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

      def convertErrors(errors: NonEmptyVector[SyntaxError]): NonEmptyList[CompilationError] =
        NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList)

      fileInfo.dataStream.transformWith(
        ParseHandler.parse(fileInfo.fileSpec)
          .upcast[StreamTransformation[F, R, NonEmptyVector[SyntaxError], Char, Unit, SourceAST, Unit]]
          .mapError(convertErrors)
      )
    }

  def compile[F[-_, +_, +_], R, I: Show, A]
  (
    backend: Backend
  )(
    sourceASTs: Vector[SourceAST],
    references: Vector[I],
    compilerOptions: CompilerOptions[Id],
    backendOptions: backend.BackendOptions[Id, I]
  )(
    f: backend.TCompilationOutput[F, R, I] => F[R, NonEmptyList[CompilationError], A]
  )
  (implicit compInstance: CompilationRE[F, R], res: ResourceAccess[F, R, I])
  : F[R, NonEmptyList[CompilationError], A] = {
    val input = CompilerInput(
      source = sourceASTs,
      references = references,
      options = compilerOptions,
      backendOptions = backendOptions,
    )

    backend.compile[F, R, I, A](input)(f)
  }


}
