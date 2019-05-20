package dev.argon.build

import java.io.File

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.implicits._
import cats.data.NonEmptyVector
import dev.argon.parser.impl.ParseHandler
import dev.argon.util.FileSpec
import dev.argon.util.stream._

object BuildProcess {

  def parseInput[F[+_, +_]: CompilationE](inputFiles: ArStream[F, scalaz.NonEmptyList[CompilationError], InputFileInfo[F]]): ArStream[F, scalaz.NonEmptyList[CompilationError], SourceAST] = {

    implicit val catsMonad = shims.monadToCats[F[scalaz.NonEmptyList[CompilationError], ?]]

    inputFiles.flatMap { fileInfo =>
      fileInfo.dataStream.transformWith(
        ParseHandler.parse(fileInfo.fileSpec).translate(new (Either[NonEmptyVector[SyntaxError], ?] ~> F[scalaz.NonEmptyList[CompilationError], ?]) {

          private def toCompileError(error: SyntaxError): CompilationError =
            CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

          override def apply[A](fa: Either[NonEmptyVector[SyntaxError], A]): F[scalaz.NonEmptyList[CompilationError], A] = fa match {
            case Left(error) => CompilationE[F].forErrors(toCompileError(error.head), error.tail.map(toCompileError): _*)
            case Right(value) => value.pure[F[scalaz.NonEmptyList[CompilationError], ?]]
          }
        })
      )
    }
  }

  def compile[F[+_, +_]: CompilationE, I: Show, A]
  (
    backend: Backend
  )(
    sourceASTs: Vector[SourceAST],
    references: Vector[I],
    compilerOptions: CompilerOptions[Id],
    backendOptions: backend.BackendOptions[Id, I]
  )(
    f: backend.TCompilationOutput[F, I] => F[scalaz.NonEmptyList[CompilationError], A]
  )
  (implicit res: ResourceAccess[F[scalaz.NonEmptyList[CompilationError], ?], I])
  : F[scalaz.NonEmptyList[CompilationError], A] = {
    val input = CompilerInput(
      source = sourceASTs,
      references = references,
      options = compilerOptions,
      backendOptions = backendOptions,
    )

    backend.compile[F, I, A](input)(f)
  }


}
