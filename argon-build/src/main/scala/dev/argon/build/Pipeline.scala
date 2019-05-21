package dev.argon.build

import java.io
import java.io.File

import dev.argon.compiler._
import dev.argon.util.stream.{ArStream, FileStream}
import scalaz._
import Scalaz._
import dev.argon.build.project.{BuildInfo, FileWithSpec}
import dev.argon.compiler.core.ModuleDescriptor
import dev.argon.util.{FileID, FileOperations, FileSpec}
import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import dev.argon.util.FileOperations.fileShow
import IOCompilation.fileSystemResourceAccess

object Pipeline {

  type MonadErrorThrowable[F[_, _]] = MonadError[F[Throwable, ?], Throwable]

  protected def findInputFiles(buildInfo: BuildInfo[File]): ArStream[IO[NonEmptyList[CompilationError], ?], InputFileInfo[IO[NonEmptyList[CompilationError], ?]], Unit] =
    ArStream.fromVector[IO[NonEmptyList[CompilationError], ?], (File, Int), Unit](buildInfo.project.inputFiles.toVector.zipWithIndex, ())
      .mapItems { case (file, id) =>
        InputFileInfo(FileSpec(FileID(id), file.getPath),
          FileStream.readFileText(file, bufferSize = 1024) {
            case ex: io.IOException => NonEmptyList[CompilationError](CompilationError.ResourceIOError(CompilationMessageSource.ResourceIdentifier(file), ex))
          }
        )
      }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): TaskR[Console, Unit] =
    msgs
      .traverse_ { msg =>
        putStrLn(msg.toString)
      }

  def compileResult[A](buildInfo: BuildInfo[File])(f: buildInfo.backend.TCompilationOutput[IO, File] => IO[NonEmptyList[CompilationError], A])(implicit compInstance: IOCompilation): IO[NonEmptyList[CompilationError], A] =
    BuildProcess.parseInput[IO[NonEmptyList[CompilationError], ?]](findInputFiles(buildInfo)).toVector(compInstance).flatMap { parsedInput =>
      BuildProcess.compile(
        buildInfo.backend
      )(
        parsedInput,
        buildInfo.project.references.toVector,
        CompilerOptions(
          moduleName = buildInfo.compilerOptions.moduleName
        ),
        buildInfo.backendOptions,
      )(f)
    }

  def run(buildInfo: BuildInfo[File]): TaskR[Console, Int] =
    IOCompilation.compilationInstance
      .flatMap { implicit compInstance =>
        compInstance.getResult(
          compileResult(buildInfo) { output =>
            output.write(IOCompilation.fileSystemResourceAccess)
          }
        )
      }
    .flatMap {
      case (msgs, Left(errors)) =>
        printMessages[Vector, CompilationMessage](errors.toVector ++ msgs).map { _ => 1 }

      case (msgs, Right(_)) =>
        printMessages[Vector, CompilationMessage](msgs).map { _ => 0 }
    }


}

