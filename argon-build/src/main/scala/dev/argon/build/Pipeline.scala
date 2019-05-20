package dev.argon.build

import java.io
import java.io.File

import dev.argon.compiler._
import dev.argon.util.stream._
import cats._
import cats.implicits._
import dev.argon.build.project.{BuildInfo, FileWithSpec}
import dev.argon.compiler.core.ModuleDescriptor
import dev.argon.util.{FileID, FileOperations, FileSpec}
import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.catz._
import dev.argon.util.FileOperations.fileShow
import IOCompilation.fileSystemResourceAccess
import cats.data.NonEmptyVector
import dev.argon.parser.SourceAST

object Pipeline {

  private implicit val fileShow = scalaz.Show.shows(FileOperations.fileShow.show)

  private def refineIOToCompilationError(file: File): PartialFunction[Throwable, scalaz.NonEmptyList[CompilationError]] = {
    case ex: io.IOException => scalaz.NonEmptyList[CompilationError](CompilationError.ResourceIOError(CompilationMessageSource.ResourceIdentifier(file), ex))
  }

  private def createFileDataStream(file: File): stream.Stream[scalaz.NonEmptyList[CompilationError], Char] =
    stream.ZStream.managed(
      ZManaged.make(
        IO.effect { new io.FileReader(file) }.refineOrDie(refineIOToCompilationError(file))
      )(
        reader => IO.effectTotal { reader.close() }
      )
    ) { reader =>
      IO.effect {
        val b = reader.read()
        if(b < 0)
          None
        else
          Some(b.toChar)
      }.refineOrDie(refineIOToCompilationError(file))
    }



  protected def findInputFiles(buildInfo: BuildInfo[File]): ArStream[IO, scalaz.NonEmptyList[CompilationError], InputFileInfo[IO]] =

    ArStream.fromVector[IO, scalaz.NonEmptyList[CompilationError], (File, Int)](buildInfo.project.inputFiles.toVector.zipWithIndex)
      .map { case (file, id) =>
        InputFileInfo(FileSpec(FileID(id), file.getPath),
          ArStream.fromZStream(createFileDataStream(file))
        )
      }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): TaskR[Console, Unit] =
    msgs
      .traverse_ { msg =>
        putStrLn(msg.toString)
      }

  def compileResult[A]
  (buildInfo: BuildInfo[File])
  (f: buildInfo.backend.TCompilationOutput[IO, File] => IO[scalaz.NonEmptyList[CompilationError], A])
  (implicit compInstance: IOCompilation)
  : IO[scalaz.NonEmptyList[CompilationError], A] =
    BuildProcess.parseInput[IO](findInputFiles(buildInfo)).foldLeft(StreamTransformation.toVector[IO, scalaz.NonEmptyList[CompilationError], SourceAST]).flatMap { parsedInput =>

      implicit val fileShow: cats.Show[File] = FileOperations.fileShow

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
        printMessages[Vector, CompilationMessage](errors.list.toVector ++ msgs).map { _ => 1 }

      case (msgs, Right(_)) =>
        printMessages[Vector, CompilationMessage](msgs).map { _ => 0 }
    }


}

