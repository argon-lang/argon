package com.mi3software.argon.build

import java.io.File

import com.mi3software.argon.compiler._
import com.mi3software.argon.util.stream.{ArStream, FileStream}
import scalaz._
import Scalaz._
import com.mi3software.argon.build.project.{BuildInfo, FileWithSpec}
import com.mi3software.argon.compiler.core.ModuleDescriptor
import com.mi3software.argon.util.{FileID, FileOperations, FileSpec}
import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import com.mi3software.argon.util.FileOperations.fileShow
import IOCompilation.fileSystemResourceAccess

object Pipeline {

  type MonadErrorThrowable[F[_, _]] = MonadError[F[Throwable, ?], Throwable]

  protected def findInputFiles[F[_, _] : MonadErrorThrowable : ZIO](buildInfo: BuildInfo[File]): ArStream[F[Throwable, ?], InputFileInfo[F[Throwable, ?]], Unit] =
    ArStream.fromVector[F[Throwable, ?], (File, Int), Unit](buildInfo.project.inputFiles.toVector.zipWithIndex, ())
      .mapItems { case (file, id) =>
        InputFileInfo(FileSpec(FileID(id), file.getPath),
          FileStream.readFileText[F](file, bufferSize = 1024)
        )
      }

  def printMessages[F[_, _] : MonadErrorThrowable : ZIO, C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): F[Throwable, Unit] =
    msgs
      .traverse_ { msg =>
        ZIO[F].liftZIO(putStrLn(msg.toString))
      }

  def compileResult[A](buildInfo: BuildInfo[File])(f: buildInfo.backend.TCompilationOutput[IO[Throwable, +?], File] => IO[Throwable, A])(implicit compInstance: IOCompilation): IO[Throwable, A] =
    BuildProcess.parseInput(findInputFiles[IO](buildInfo)).toVector(compInstance).flatMap { parsedInput =>
      BuildProcess.compile(
        buildInfo.backend
      )(
        parsedInput,
        buildInfo.project.references.toVector,
        CompilerOptions(
          moduleName = buildInfo.compilerOptions.moduleName
        ),
        buildInfo.backendOptions,
      )(f)(implicitly, compInstance, IOCompilation.fileSystemResourceAccess)
    }

  def run(buildInfo: BuildInfo[File]): IO[Throwable, Int] =
    IOCompilation.compilationInstance
      .flatMap { implicit compInstance =>
        compInstance.getResult(
          compileResult(buildInfo) { output =>
            output.write
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

