package com.mi3software.argon.build

import com.mi3software.argon.compiler.CompilationMessage
import com.mi3software.argon.parser.impl.ParseHandler
import com.mi3software.argon.util.stream.{ArStream, FileStream}
import scalaz.effect._
import scalaz._
import Scalaz._
import com.mi3software.argon.util.IOHelpers.ioMonadError
import com.mi3software.argon.compiler.IOCompilation

final class Pipeline(buildInfo: BuildInfo) {
  protected def findInputFiles: ArStream[IO, InputFileInfo[IO], Unit] =
    ArStream.fromVector[IO, FileWithSpec, Unit](buildInfo.inputFiles, ())
      .mapItems { case FileWithSpec(file, fileSpec) =>
        InputFileInfo(fileSpec,
          FileStream.readFileText[IO](file, bufferSize = 1024)
        )
      }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): IO[Unit] =
    msgs
      .traverse_ { msg =>
        IO.putStrLn(msg.toString)
      }


  def compileResult: IO[CompilationResult] =
    IOCompilation.compilationInstance.flatMap { implicit compInstance =>
      BuildProcess.parseInput(findInputFiles).toVector(compInstance).flatMap { parsedInput =>
        BuildProcess.compile(
          buildInfo.backend,
          parsedInput,
          buildInfo.references,
          buildInfo.compilerOptions
        )
      }
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

