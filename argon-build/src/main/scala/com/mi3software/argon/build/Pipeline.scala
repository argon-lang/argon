package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.compiler.{CompilationError, CompilationExec, CompilationMessage, IOCompilation}
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

  def compileResult(buildInfo: BuildInfo)(implicit compInstance: IOCompilation): IO[Throwable, buildInfo.backend.TCompilationOutput[IO[Throwable, +?]]] =
    BuildProcess.parseInput(findInputFiles[IO](buildInfo)).toVector(compInstance).flatMap { parsedInput =>
      BuildProcess.compile(
        buildInfo.backend,
        parsedInput,
        buildInfo.references,
        buildInfo.compilerOptions
      )(implicitly, compInstance, IOCompilation.fileSystemResourceAccess)
    }

  def run(buildInfo: BuildInfo): IO[Throwable, Int] =
    IOCompilation.compilationInstance
      .flatMap { implicit compInstance =>
        compInstance.getResult(
          compileResult(buildInfo).flatMap { output =>
            FileOperations.fileOutputStream(buildInfo.outputFile)(output.write[IO])
          }
        )
      }
    .flatMap {
      case (msgs, -\/(errors)) =>
        printMessages[IO, Vector, CompilationMessage](errors.toVector ++ msgs).map { _ => 1 }

      case (msgs, \/-(_)) =>
        printMessages[IO, Vector, CompilationMessage](msgs).map { _ => 0 }
    }


}

