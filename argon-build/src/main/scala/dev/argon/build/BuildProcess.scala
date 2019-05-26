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

  def parseInput[F[-_, +_, +_], R](inputFiles: ArStream[F, R, NonEmptyList[CompilationError], InputFileInfo[F, R]])(implicit compInstance: CompilationRE[F, R]): ArStream[F, R, NonEmptyList[CompilationError], SourceAST] =
    inputFiles.flatMap { fileInfo =>
      fileInfo.dataStream.transformWith(
        ParseHandler.parse(fileInfo.fileSpec).translate(new (PureEffect[Any, NonEmptyVector[SyntaxError], ?] ~> F[R, NonEmptyList[CompilationError], ?]) {

          private def toCompileError(error: SyntaxError): CompilationError =
            CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

          override def apply[A](fa: PureEffect[Any, NonEmptyVector[SyntaxError], A]): F[R, NonEmptyList[CompilationError], A] =
            compInstance.fromPureEffect(fa.mapLeft { errors =>
              NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList)
            })
        })
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
