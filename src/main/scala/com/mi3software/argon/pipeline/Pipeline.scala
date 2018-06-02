package com.mi3software.argon.pipeline

import com.mi3software.argon.parser.ParseHandler
import com.mi3software.argon.util.FileOperations
import scalaz.effect.IO
import scalaz._
import Scalaz._
import com.mi3software.argon.builder._
import com.mi3software.argon.compiler.{CompilationError, CompilationMessage, CompilerInput}

object Pipeline {

  def printMessages(msgs: NonEmptyList[CompilationMessage]): IO[Unit] =
    msgs
      .traverseU { msg =>
        IO.putStrLn(msg.toString)
      }
      .map { _ => () }

  def run(buildInfo: BuildInfo): IO[Unit] =
    buildInfo.inputFiles
      .traverseU {
        case FileWithSpec(file, fileSpec) =>
          EitherT(
            FileOperations.readAllText(file)
              .map(ParseHandler.parse(fileSpec))
          )
      }
      .run
      .flatMap {
        case -\/(syntaxErrors) =>
          printMessages(syntaxErrors.map(CompilationError.SyntaxCompilerError))

        case \/-(sourceASTs) =>
          val input = CompilerInput(
            source = sourceASTs.flatten,
            references = buildInfo.references,
          )

          buildInfo.backend.compile(input).flatMap {
            case -\/(messages) =>
              printMessages(messages)

            case \/-(result) =>
              result.writeToFile(buildInfo.outputFile)
          }
      }



}
