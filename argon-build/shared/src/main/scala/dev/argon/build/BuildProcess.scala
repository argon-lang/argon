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

  def parseInput
  (inputFiles: Source[ZIO[BuildEnvironment, ErrorList, *], InputFileInfo[ZIO[BuildEnvironment, ErrorList, *]], Unit])
  : Source[ZIO[BuildEnvironment, ErrorList, *], SourceAST, Unit] =
    new Source[ZIO[BuildEnvironment, ErrorList, *], SourceAST, Unit] {

      override protected val monadF: Monad[ZIO[BuildEnvironment, ErrorList, *]] = implicitly[Monad[ZIO[BuildEnvironment, ErrorList, *]]]

      override protected def generateImpl[G[_] : Monad](sink: Sink[G, SourceAST])(implicit genEffect: GenEffect[ZIO[BuildEnvironment, ErrorList, *], G]): G[Unit] =
        inputFiles.foreachG { fileInfo =>
          def toCompileError(error: SyntaxError): CompilationError =
            CompilationError.SyntaxCompilerError(SyntaxErrorData(fileInfo.fileSpec, error))

          implicit val errorHandler: ParseErrorHandler[ZIO[BuildEnvironment, ErrorList, *], NonEmptyVector[SyntaxError]] =
            new ParseErrorHandler[ZIO[BuildEnvironment, ErrorList, *], NonEmptyVector[SyntaxError]] {
              override def raiseError[A](errors: NonEmptyVector[SyntaxError]): ZIO[BuildEnvironment, ErrorList, A] =
                Compilation.forErrors(NonEmptyList(toCompileError(errors.head), errors.tail.map(toCompileError).toList))
            }

          val parsed = ParseHandler.parse[ZIO[BuildEnvironment, ErrorList, *]](fileInfo.fileSpec)(fileInfo.dataStream)

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
