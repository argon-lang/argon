package dev.argon.build

import java.io
import java.io.{File, FileInputStream, IOException}

import dev.argon.compiler._
import dev.argon.stream._
import cats._
import cats.implicits._
import dev.argon.build.project.{BuildInfo, FileWithSpec}
import dev.argon.compiler.core.ModuleDescriptor
import dev.argon.util.{FileID, FileSpec}
import zio._
import zio.console._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.parser.SourceAST
import dev.argon.compiler.backend.Backend
import dev.argon.io.FileIO
import dev.argon.stream.ArStream
import dev.argon.io.FileOperations.fileShow
import dev.argon.build._

object Pipeline {

  private def ioToCompilationError(ex: IOException): NonEmptyList[CompilationError] =
    NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

  private def createFileDataStream(file: File): stream.ZStream[FileIO, NonEmptyList[CompilationError], Char] =
    stream.ZStream.flatten(stream.ZStream.fromEffect(
      ZIO.access[FileIO] { _.fileIO.readText(ioToCompilationError)(file.toPath) }
    ))

  private def findInputFiles(buildInfo: BuildInfo[File]): ArStream[ZIO, FileIO, NonEmptyList[CompilationError], InputFileInfo[ZIO, FileIO]] = {
    import zio.interop.catz._

    ArStream.fromVector[ZIO, FileIO, NonEmptyList[CompilationError], (File, Int)](buildInfo.project.inputFiles.toVector.zipWithIndex)
      .map { case (file, id) =>
        InputFileInfo(FileSpec(FileID(id), file.getPath),
          ArStream.fromZStream(createFileDataStream(file))
        )
      }
  }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): RIO[Console, Unit] = {
    import zio.interop.catz._

    msgs
      .traverse_ { msg =>
        putStrLn(msg.toString)
      }
  }

  def compileResult[A]
  (buildInfo: BuildInfo[File])
  (f: buildInfo.backend.TCompilationOutput { val context: Backend.ContextWithComp[ZIO, BuildEnvironment, File] } => ZIO[BuildEnvironment, NonEmptyList[CompilationError], A])
  (implicit compInstance: IOCompilation[BuildEnvironment])
  : ZIO[BuildEnvironment, NonEmptyList[CompilationError], A] =
    ZIO.access[FileIO] { res => IOCompilation.fileSystemResourceAccessFactory[BuildEnvironment](res.fileIO) }.flatMap { implicit resFactory =>
      {
        import zio.interop.catz._
        BuildProcess.parseInput[ZIO, BuildEnvironment](findInputFiles(buildInfo))
        }.foldLeft(StreamTransformation.toVector[ZIO, BuildEnvironment, NonEmptyList[CompilationError], SourceAST]).flatMap { parsedInput =>
        BuildProcess.compile[ZIO, BuildEnvironment, File, A](
          buildInfo.backend : buildInfo.backend.type
        )(
          parsedInput,
          buildInfo.project.references.toVector,
          CompilerOptions(
            moduleName = buildInfo.compilerOptions.moduleName
          ),
          buildInfo.backendOptions : buildInfo.backend.BackendOptions[Id, File],
        )(f)
      }
    }

  def run(buildInfo: BuildInfo[File]): RIO[Console with BuildEnvironment, Int] =
    IOCompilation.compilationInstance[BuildEnvironment]
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

