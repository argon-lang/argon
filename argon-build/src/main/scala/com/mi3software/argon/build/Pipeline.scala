package com.mi3software.argon.build

import com.mi3software.argon.compiler.{CompilationError, CompilationMessage, CompilerInput}
import com.mi3software.argon.parser.impl.ParseHandler
import com.mi3software.argon.util.CatsInstances
import scalaz.effect.IO
import scalaz._
import Scalaz._
import shims.effect._

object Pipeline {

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): IO[Unit] =
    msgs
      .traverseU { msg =>
        IO.putStrLn(msg.toString)
      }
      .map { _ => () }

  def run(buildInfo: BuildInfo): IO[Unit] =
    buildInfo.inputFiles
      .traverseU {
        case FileWithSpec(file, fileSpec) =>
          fs2.io.file.readAll[IO](file.toPath, chunkSize = 1024)
            .through(ParseHandler.decodeText)
            .translate(ParseHandler.addSyntaxErrorEffect)
            .through(ParseHandler.parse[IO](fileSpec))
            .translate(ParseHandler.convertSyntaxErrorToCompilationError(fileSpec))
            .compile
            .toVector(CatsInstances.scalazEitherTSync)
      }
      .run
      .flatMap {
        case -\/(syntaxErrors) =>
          printMessages(syntaxErrors.map(CompilationError.SyntaxCompilerError))

        case \/-(sourceASTs) =>
          val input = CompilerInput(
            source = sourceASTs.flatten,
            references = buildInfo.references,
            options = buildInfo.compilerOptions
          )

          buildInfo.backend.compile(input).flatMap {
            case (msgs, result) =>
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



}
