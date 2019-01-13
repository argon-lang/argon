package com.mi3software.argon.build

import com.mi3software.argon.compiler.{CompilationError, CompilationMessage, CompilerInput}
import com.mi3software.argon.parser.impl.ParseHandler
import com.mi3software.argon.util.CatsInstances
import scalaz.effect.IO
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.{SourceAST, SyntaxErrorData}
import shims.effect._
import fs2.Stream

final class Pipeline(buildInfo: BuildInfo) extends BuildProcess.ParsePhase[IO] {
  override protected def findInputFiles: fs2.Stream[IO, InputFileInfo[IO]] =
    Stream(buildInfo.inputFiles: _*)
      .covary[IO]
      .map { case FileWithSpec(file, fileSpec) =>
        InputFileInfo(fileSpec,
          fs2.io.file.readAll[IO](file.toPath, chunkSize = 1024)
            .through(ParseHandler.decodeText)
        )
      }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): IO[Unit] =
    msgs
      .traverse_ { msg =>
        IO.putStrLn(msg.toString)
      }


  def compileResult: IO[CompilationResult] =
    parseInput
      .run
      .flatMap {
        case -\/(syntaxErrors) =>
          CompilationResult(Set.empty, -\/(syntaxErrors)).point[IO]

        case \/-(sourceASTs) =>
          val input = CompilerInput(
            source = sourceASTs,
            references = buildInfo.references,
            options = buildInfo.compilerOptions
          )

          buildInfo.backend.compile(input)
      }

  def run: IO[Unit] =
    compileResult.flatMap {
      case CompilationResult(msgs, result) =>
        printMessages(msgs.toVector).flatMap { _ =>
          result match {
            case -\/(errors) =>
              printMessages(errors)

            case \/-(result) =>
              result.writeToFile(buildInfo.outputFile)
          }
        }
    }

}

