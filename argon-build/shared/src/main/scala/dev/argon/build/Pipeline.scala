package dev.argon.build

import java.io
import java.io.{File, FileInputStream, IOException}

import dev.argon.compiler._
import dev.argon.stream._
import cats._
import cats.arrow.FunctionK
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
import dev.argon.stream.builder.{Generator, Iter}
import zio.stream.ZStream
import dev.argon.stream.builder.ZStreamIter._

object Pipeline {

  private def ioToCompilationError(ex: IOException): NonEmptyList[CompilationError] =
    NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

  private def createFileDataStream(file: File): stream.ZStream[FileIO, NonEmptyList[CompilationError], Char] =
    stream.ZStream.flatten(stream.ZStream.fromEffect(
      ZIO.access[FileIO] { _.fileIO.readText(ioToCompilationError)(file.toPath) }
    ))

  private type IterStream[A, X] = (ZStream[FileIO, NonEmptyList[CompilationError], A], X)

  private def findInputFiles(buildInfo: BuildInfo[File]): ZStream[FileIO, NonEmptyList[CompilationError], InputFileInfo[IterStream]] =
    ZStream.fromIterable(buildInfo.project.inputFiles.view.zipWithIndex)
      .map { case (file, id) =>
        InputFileInfo[IterStream](FileSpec(FileID(id), file.getPath),
          (createFileDataStream(file), ())
        )
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

      type F[B] = ZIO[BuildEnvironment, NonEmptyList[CompilationError], B]

      val parsedInputStream = {
        import zio.interop.catz._
        BuildProcess.parseInput[F, IterStream]((findInputFiles(buildInfo), ()))
      }

      Iter[F, Generator[F, ?, ?], Unit].foldLeftM(FunctionK.id)(parsedInputStream)(Vector.empty[SourceAST]) { (acc, ast) => IO.succeed(acc :+ ast) }
        .flatMap {
          case (parsedInput, _) =>
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

