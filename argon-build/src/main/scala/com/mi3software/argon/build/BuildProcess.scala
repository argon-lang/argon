package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.compiler._
import com.mi3software.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import scalaz._
import Scalaz._
import com.mi3software.argon.grammar.SyntaxErrorReporter
import com.mi3software.argon.parser.impl.ParseHandler
import com.mi3software.argon.util.{FileSpec, NonEmptyVector}
import com.mi3software.argon.util.stream.ArStream

object BuildProcess {

  private def errorReporter[F[_]: Compilation](fileSpec: FileSpec): SyntaxErrorReporter[F, SyntaxError] =
    new SyntaxErrorReporter[F, SyntaxError] {

      private def toCompileError(error: SyntaxError): CompilationError =
        CompilationError.SyntaxCompilerError(SyntaxErrorData(fileSpec, error))

      override def reportError[A](error: NonEmptyVector[SyntaxError]): F[A] =
        Compilation[F].forErrors(toCompileError(error.head), error.tail.map(toCompileError): _*)
    }



  def parseInput[F[_]: Compilation](inputFiles: ArStream[F, InputFileInfo[F], Unit]): ArStream[F, SourceAST, Unit] =
    inputFiles.merge[SourceAST, Unit] { case (_: Unit, _) => } { fileInfo =>
      implicit val reporter = errorReporter[F](fileInfo.fileSpec)
      ParseHandler.parse(fileInfo.fileSpec)(fileInfo.dataStream)
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
    f: backend.TCompilationOutput[F, I] => F[A]
  )
  (implicit comp: Compilation[F], res: ResourceAccess[F, I])
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
