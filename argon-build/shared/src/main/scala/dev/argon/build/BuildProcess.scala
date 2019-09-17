package dev.argon.build

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.arrow.FunctionK
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.compiler.backend.Backend
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.impl.ParseHandler
import dev.argon.util.FileSpec
import dev.argon.stream._
import dev.argon.stream.builder.{GenEffect, Sink, Source}
import dev.argon.util.AnyExtensions._

object BuildProcess {

  def parseInput[F[_]: Compilation]
  (inputFiles: Source[F, InputFileInfo[F], Unit])
  : Source[F, SourceAST, Unit] =
    new Source[F, SourceAST, Unit] {

      override protected val monadF: Monad[F] = Compilation[F]

      override protected def generateImpl[G[_] : Monad](sink: Sink[G, SourceAST])(implicit genEffect: GenEffect[F, G]): G[Unit] =
        inputFiles.foreachG { fileInfo =>
          def toCompileError(error: SyntaxError): CompilationError =
            CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

          implicit val errorHandler: ParseErrorHandler[F, NonEmptyVector[SyntaxError]] =
            new ParseErrorHandler[F, NonEmptyVector[SyntaxError]] {
              override def raiseError[A](errors: NonEmptyVector[SyntaxError]): F[A] =
                Compilation[F].forErrors(NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList))
            }

          val parsed = ParseHandler.parse[F](fileInfo.fileSpec)(fileInfo.dataStream)

          parsed.foreachG(sink.consume)
        }
    }

  def compile[F[+_], I: Show, A]
  (
    backend: Backend
  )(
    sourceASTs: Vector[SourceAST],
    references: Vector[I],
    compilerOptions: CompilerOptions[Id],
    backendOptions: backend.BackendOptions[Id, I]
  )(
    f: backend.TCompilationOutput { val context: Backend.ContextWithComp[F, I] } => F[A]
  )
  (implicit compInstance: Compilation[F], res: ResourceAccessFactory[Backend.ContextWithComp[F, I]])
  : F[A] = {
    val input = CompilerInput(
      source = sourceASTs,
      references = references,
      options = compilerOptions,
      backendOptions = backendOptions,
    )

    backend.compile[F, I, A](input)(f)
  }


}
