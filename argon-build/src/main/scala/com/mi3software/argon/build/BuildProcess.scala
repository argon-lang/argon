package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.compiler.{CompilationError, CompilerInput, CompilerOptions}
import com.mi3software.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import fs2._
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.impl.ParseHandler
import com.mi3software.argon.util.{CatsInstances, FileSpec}
import scalaz.effect.IO
import shims.effect._

object BuildProcess {

  def parseInput[F[_]: cats.effect.Sync](inputFiles: Stream[F, InputFileInfo[F]]): EitherT[F, NonEmptyList[CompilationError], Vector[SourceAST]] =
    inputFiles
      .translate(ParseHandler.addSyntaxErrorEffect[F, SyntaxErrorData])
      .flatMap { case InputFileInfo(fileSpec, dataStream) =>
        dataStream
          .translate(ParseHandler.addSyntaxErrorEffect[F, SyntaxError])
          .through(ParseHandler.parse[F](fileSpec))
          .translate(ParseHandler.convertSyntaxErrorToCompilationError(fileSpec))
      }
      .compile
      .toVector(CatsInstances.scalazEitherTSync)
      .leftMap { _.map[CompilationError](CompilationError.SyntaxCompilerError) }

  def compile
  (
    backend: Backend,
    parsedSource: EitherT[IO, NonEmptyList[CompilationError], Vector[SourceAST]],
    references: Vector[File],
    compilerOptions: CompilerOptions,
  ): IO[CompilationResult] =
    parsedSource
      .run
      .flatMap {
        case -\/(syntaxErrors) =>
          CompilationResult(Set.empty, -\/(syntaxErrors)).point[IO]

        case \/-(sourceASTs) =>
          val input = CompilerInput(
            source = sourceASTs,
            references = references,
            options = compilerOptions
          )

          backend.compile(input)
      }


}
