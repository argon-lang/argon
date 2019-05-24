package dev.argon.build

import java.io.File

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.parser.impl.ParseHandler
import dev.argon.util.FileSpec
import dev.argon.util.stream._

object BuildProcess {

  def parseInput[F[-_, +_, +_]: CompilationRE](inputFiles: ArStream[F, Any, NonEmptyList[CompilationError], InputFileInfo[F]]): ArStream[F, Any, NonEmptyList[CompilationError], SourceAST] =
    inputFiles.flatMap { fileInfo =>
      fileInfo.dataStream.transformWith(
        ParseHandler.parse(fileInfo.fileSpec).translate(new (PureEffect[Any, NonEmptyVector[SyntaxError], ?] ~> F[Any, NonEmptyList[CompilationError], ?]) {

          private def toCompileError(error: SyntaxError): CompilationError =
            CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

          override def apply[A](fa: PureEffect[Any, NonEmptyVector[SyntaxError], A]): F[Any, NonEmptyList[CompilationError], A] = fa.run(()).value match {
            case Left(error) => CompilationRE[F].forErrors(toCompileError(error.head), error.tail.map(toCompileError): _*)
            case Right(value) => value.pure[F[Any, NonEmptyList[CompilationError], ?]]
          }
        })
      )
    }

  def compile[F[-_, +_, +_]: CompilationRE, I: Show, A]
  (
    backend: Backend
  )(
    sourceASTs: Vector[SourceAST],
    references: Vector[I],
    compilerOptions: CompilerOptions[Id],
    backendOptions: backend.BackendOptions[Id, I]
  )(
    f: backend.TCompilationOutput[F, I] => F[Any, NonEmptyList[CompilationError], A]
  )
  (implicit res: ResourceAccess[F, I])
  : F[Any, NonEmptyList[CompilationError], A] = {
    val input = CompilerInput(
      source = sourceASTs,
      references = references,
      options = compilerOptions,
      backendOptions = backendOptions,
    )

    backend.compile[F, I, A](input)(f)
  }


}
