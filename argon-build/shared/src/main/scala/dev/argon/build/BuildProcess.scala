package dev.argon.build

import dev.argon.compiler._
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.arrow.FunctionK
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.backend.{Backend, ResourceAccess}
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.grammar.ParseErrorHandler
import dev.argon.parser.impl.ParseHandler
import dev.argon.util.FileSpec
import dev.argon.stream._
import dev.argon.stream.builder.{GenEffect, Sink, Source}
import dev.argon.util.AnyExtensions._
import zio.{ZIO, ZManaged}
import zio.interop.catz._

object BuildProcess {

  def parseInput[R]
  (inputFiles: Source[ZIO[R, ErrorList, *], InputFileInfo[ZIO[R, ErrorList, *]], Unit])
  : Source[ZIO[R, ErrorList, *], SourceAST, Unit] =
    new Source[ZIO[R, ErrorList, *], SourceAST, Unit] {

      override protected val monadF: Monad[ZIO[R, ErrorList, *]] = implicitly[Monad[ZIO[R, ErrorList, *]]]

      override protected def generateImpl[G[_] : Monad](sink: Sink[G, SourceAST])(implicit genEffect: GenEffect[ZIO[R, ErrorList, *], G]): G[Unit] =
        inputFiles.foreachG { fileInfo =>
          def toCompileError(error: SyntaxError): CompilationError =
            CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

          implicit val errorHandler: ParseErrorHandler[ZIO[R, ErrorList, *], NonEmptyVector[SyntaxError]] =
            new ParseErrorHandler[ZIO[R, ErrorList, *], NonEmptyVector[SyntaxError]] {
              override def raiseError[A](errors: NonEmptyVector[SyntaxError]): ZIO[R, ErrorList, A] =
                Compilation.forErrors(NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList))
            }

          val parsed = ParseHandler.parse[ZIO[R, ErrorList, *]](fileInfo.fileSpec)(fileInfo.dataStream)

          parsed.foreachG(sink.consume)
        }
    }

  def compile
  (
    backend: Backend
  )(
    sourceASTs: Vector[SourceAST],
    references: Vector[ResourceIndicator],
    compilerOptions: CompilerOptions[Id],
    backendOptions: backend.BackendOptions[Id, ResourceIndicator]
  )
  : ZManaged[ResourceAccess, ErrorList, backend.TCompilationOutput] = {
    val input = CompilerInput(
      source = sourceASTs,
      references = references,
      options = compilerOptions,
      backendOptions = backendOptions,
    )

    backend.compile(input)
  }


}
