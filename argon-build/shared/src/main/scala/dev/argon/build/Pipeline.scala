package dev.argon.build

import java.io.IOException
import java.nio.file.Path

import dev.argon.compiler._
import dev.argon.stream._
import cats._
import cats.arrow.FunctionK
import cats.implicits._
import dev.argon.build.project.BuildInfo
import dev.argon.compiler.core.ModuleDescriptor
import dev.argon.util.{FileID, FileSpec}
import zio._
import zio.console._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.parser.SourceAST
import dev.argon.compiler.backend.Backend
import dev.argon.io.{FileIO, FilenameManip}
import dev.argon.stream.ArStream
import dev.argon.io.FileOperations.pathShow
import dev.argon.build._
import dev.argon.stream.builder.ZStreamSource
import zio.stream.{ZSink, ZStream}

object Pipeline {

  private def ioToCompilationError(ex: IOException): NonEmptyList[CompilationError] =
    NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

  private def createFileDataStream(path: Path): stream.ZStream[FileIO, NonEmptyList[CompilationError], Char] =
    stream.ZStream.flatten(stream.ZStream.fromEffect(
      ZIO.access[FileIO] { _.fileIO.readText(ioToCompilationError)(path) }
    ))

  private type FIO[A] = ZIO[FileIO, NonEmptyList[CompilationError], A]

  private def resolveGlob(globs: List[Path]): ZStream[FileIO, NonEmptyList[CompilationError], Path] =
    ZStream.fromIterable(globs)
      .flatMap { glob =>
        ZStream.fromEffect(
          FilenameManip.findGlob(glob).runCollect
            .mapError { ex => NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex))) }
        )
          .flatMap(ZStream.fromIterable)
      }

  private def findInputFiles(buildInfo: BuildInfo.Resolved): ZStream[FileIO, NonEmptyList[CompilationError], InputFileInfo[FIO]] =
    resolveGlob(buildInfo.project.inputFiles)
      .zipWithIndex
      .map { case (path, id) =>
        InputFileInfo[FIO](FileSpec(FileID(id), FilenameManip.pathToString(path)),
          ZStreamSource(createFileDataStream(path))
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
  (buildInfo: BuildInfo.Resolved)
  (f: buildInfo.backend.TCompilationOutput { val context: Backend.ContextWithComp[ZIO, BuildEnvironment, Path] } => ZIO[BuildEnvironment, NonEmptyList[CompilationError], A])
  (implicit compInstance: IOCompilation[BuildEnvironment])
  : ZIO[BuildEnvironment, NonEmptyList[CompilationError], A] =
    ZIO.access[FileIO] { res => IOCompilation.fileSystemResourceAccessFactory[BuildEnvironment](res.fileIO) }.flatMap { implicit resFactory =>

      val parsedInputStream = {
        import zio.interop.catz._
        BuildProcess.parseInput[FIO](ZStreamSource(findInputFiles(buildInfo)))
      }

      parsedInputStream.foldLeftM(Vector.empty[SourceAST]) { (acc, ast) => IO.succeed(acc :+ ast) }
        .flatMap {
          case (parsedInput, _) =>
            resolveGlob(buildInfo.project.references).runCollect.flatMap { references =>
              BuildProcess.compile[ZIO, BuildEnvironment, Path, A](
                buildInfo.backend : buildInfo.backend.type
              )(
                parsedInput,
                references.toVector,
                CompilerOptions(
                  moduleName = buildInfo.compilerOptions.moduleName
                ),
                buildInfo.backendOptions,
              )(f)
            }
        }
    }

  def run(buildInfo: BuildInfo.Resolved): RIO[Console with BuildEnvironment, Int] =
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

