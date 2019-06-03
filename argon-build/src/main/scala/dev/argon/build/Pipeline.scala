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
import dev.argon.util.FileOperations.fileShow
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.parser.SourceAST
import IOCompilation.fileSystemResourceAccess
import scalaz.zio.blocking.Blocking

object Pipeline {

  private def refineIOToCompilationError(file: File): PartialFunction[Throwable, NonEmptyList[CompilationError]] = {
    case ex: io.IOException => NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))
  }

  private def createFileDataStream(file: File): stream.ZStream[Blocking, NonEmptyList[CompilationError], Char] =
    stream.ZStream.managed(
      ZManaged.make(
        ZIO.environment[Blocking].flatMap(_.blocking.effectBlocking { new io.FileReader(file) }).refineOrDie(refineIOToCompilationError(file))
      )(
        reader => ZIO.environment[Blocking].flatMap(_.blocking.blocking(IO.effectTotal { reader.close() }))
      )
    ).flatMap { reader =>
      stream.ZStream.unfoldM(()) { _ =>
        IO.effect {
          val b = reader.read()
          if(b < 0)
            None
          else
            Some((b.toChar, ()))
        }.refineOrDie(refineIOToCompilationError(file))
      }
    }

  private def findInputFiles(buildInfo: BuildInfo[File]): ArStream[ZIO, Blocking, NonEmptyList[CompilationError], InputFileInfo[ZIO, Blocking]] = {
    import scalaz.zio.interop.catz._

    ArStream.fromVector[ZIO, Blocking, NonEmptyList[CompilationError], (File, Int)](buildInfo.project.inputFiles.toVector.zipWithIndex)
      .map { case (file, id) =>
        InputFileInfo(FileSpec(FileID(id), file.getPath),
          ArStream.fromZStream(createFileDataStream(file))
        )
      }
  }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): TaskR[Console, Unit] = {
    import scalaz.zio.interop.catz._

    msgs
      .traverse_ { msg =>
        putStrLn(msg.toString)
      }
  }

  def compileResult[A]
  (buildInfo: BuildInfo[File])
  (f: buildInfo.backend.TCompilationOutput[ZIO, Blocking, File] => ZIO[Blocking, NonEmptyList[CompilationError], A])
  (implicit compInstance: IOCompilation[Blocking])
  : ZIO[Blocking, NonEmptyList[CompilationError], A] =
    {
      import scalaz.zio.interop.catz._
      BuildProcess.parseInput[ZIO, Blocking](findInputFiles(buildInfo))
    }.foldLeft(StreamTransformation.toVector[ZIO, Blocking, NonEmptyList[CompilationError], SourceAST]).flatMap { parsedInput =>
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

  def run(buildInfo: BuildInfo[File]): TaskR[Console with Blocking, Int] =
    IOCompilation.compilationInstance[Blocking]
      .flatMap { implicit compInstance =>
        compInstance.getResult(
          compileResult(buildInfo) { output =>
            output.write
          }
        )
      }
    .flatMap {
      case (msgs, Left(errors)) =>
        printMessages[Vector, CompilationMessage](errors.toList.toVector ++ msgs).map { _ => 1 }

      case (msgs, Right(_)) =>
        printMessages[Vector, CompilationMessage](msgs).map { _ => 0 }
    }


}

