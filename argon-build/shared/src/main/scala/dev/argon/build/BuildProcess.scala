package dev.argon.build

import java.io.File

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.arrow.FunctionK
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.compiler.backend.Backend
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.impl.ParseHandler
import dev.argon.stream.{ArStream, MapError, StreamTransformation}
import dev.argon.util.FileSpec
import dev.argon.stream._
import dev.argon.stream.builder.{Builder, Generator, Iter}
import dev.argon.util.AnyExtensions._

object BuildProcess {

  def parseInput[F[_], L[_, _]]
  (inputFiles: L[InputFileInfo[L], Unit])
  (implicit
    monadError: MonadError[F, NonEmptyList[CompilationError]],
    iterInstance: Iter[F, L, Unit],
  )
  : Generator[F, SourceAST, Unit] =
    new Generator[F, SourceAST, Unit] {
      override def create[G[_]](convert: F ~> G)(implicit builder: Builder[G, SourceAST]): G[Unit] =
        Iter[F, L, Unit].foreach(convert)(inputFiles) { fileInfo =>
          def toCompileError(error: SyntaxError): CompilationError =
            CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

          implicit val errorHandler: ParseErrorHandler[G, NonEmptyVector[SyntaxError]] =
            new ParseErrorHandler[G, NonEmptyVector[SyntaxError]] {
              override def raiseError[A](errors: NonEmptyVector[SyntaxError]): G[A] =
                convert(monadError.raiseError(NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList)))
            }

          implicit val iter2 = iterInstance.translateEffect(convert)

          val parsed = ParseHandler.parse[G, L](fileInfo.fileSpec)(fileInfo.dataStream)

          Iter[G, Generator[G, ?, ?], Unit].foreach(FunctionK.id)(parsed)(builder.append)
        }
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
    f: backend.TCompilationOutput { val context: Backend.ContextWithComp[F, R, I] } => F[R, NonEmptyList[CompilationError], A]
  )
  (implicit compInstance: CompilationRE[F, R], res: ResourceAccessFactory[Backend.ContextWithComp[F, R, I]])
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
