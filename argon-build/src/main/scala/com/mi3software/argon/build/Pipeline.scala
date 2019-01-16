package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.compiler.{CompilationError, CompilationMessage, IOCompilation}
import com.mi3software.argon.util.stream.{ArStream, FileStream}
import scalaz._
import Scalaz._
import com.mi3software.argon.util.FileOperations
import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import com.mi3software.argon.util.FileOperations.fileShow

object Pipeline {

  type MonadErrorThrowable[F[_, _]] = MonadError[F[Throwable, ?], Throwable]

  protected def findInputFiles[F[_, _] : MonadErrorThrowable : ZIO](buildInfo: BuildInfo): ArStream[F[Throwable, ?], InputFileInfo[F[Throwable, ?]], Unit] =
    ArStream.fromVector[F[Throwable, ?], FileWithSpec, Unit](buildInfo.inputFiles, ())
      .mapItems { case FileWithSpec(file, fileSpec) =>
        InputFileInfo(fileSpec,
          FileStream.readFileText[F](file, bufferSize = 1024)
        )
      }

  def printMessages[F[_, _] : MonadErrorThrowable : ZIO, C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): F[Throwable, Unit] =
    msgs
      .traverse_ { msg =>
        ZIO[F].liftZIO(putStrLn(msg.toString))
      }

  def compileResult(buildInfo: BuildInfo): IO[Throwable, CompilationResult[buildInfo.backend.TCompilationOutput]] =
    IOCompilation.compilationInstance.flatMap { implicit compInstance =>
      BuildProcess.parseInput(findInputFiles[IO](buildInfo)).toVector(compInstance).flatMap { parsedInput =>
        BuildProcess.compile(
          buildInfo.backend,
          parsedInput,
          buildInfo.references,
          buildInfo.compilerOptions
        )(compInstance, implicitly, compInstance, IOCompilation.fileSystemResourceAccess)
      }
    }

  def run(buildInfo: BuildInfo): IO[Throwable, Unit] =
    compileResult(buildInfo).flatMap {
      case CompilationResult(msgs, result) =>
        printMessages[IO, Vector, CompilationMessage](msgs.toVector).flatMap { _ =>
          result match {
            case -\/(errors) =>
              printMessages[IO, NonEmptyList, CompilationError](errors)

            case \/-(result) =>
              FileOperations.fileOutputStream(buildInfo.outputFile) { stream =>
                IO.syncThrowable { result.write(stream) }
              }
          }
        }
    }

}

