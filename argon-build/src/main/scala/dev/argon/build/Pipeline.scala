package dev.argon.build

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

  protected def findInputFiles(buildInfo: BuildInfo[File]): ArStream[Task, InputFileInfo[Task], Unit] =
    ArStream.fromVector[Task, (File, Int), Unit](buildInfo.project.inputFiles.toVector.zipWithIndex, ())
      .mapItems { case (file, id) =>
        InputFileInfo(FileSpec(FileID(id), file.getPath),
          FileStream.readFileText(file, bufferSize = 1024)
        )
      }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): TaskR[Console, Unit] =
    msgs
      .traverse_ { msg =>
        putStrLn(msg.toString)
      }

  def compileResult[A](buildInfo: BuildInfo[File])(f: buildInfo.backend.TCompilationOutput[Task, File] => Task[A])(implicit compInstance: IOCompilation): Task[A] =
    BuildProcess.parseInput(findInputFiles(buildInfo)).toVector(compInstance).flatMap { parsedInput =>
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

  def run(buildInfo: BuildInfo[File]): TaskR[Console, Int] =
    IOCompilation.compilationInstance
      .flatMap { implicit compInstance =>
        compInstance.getResult(
          compileResult(buildInfo) { output =>
            output.write
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

