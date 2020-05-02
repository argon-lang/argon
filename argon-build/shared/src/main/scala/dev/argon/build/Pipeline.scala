package dev.argon.build

import java.io.IOException

import dev.argon.io.Path
import dev.argon.io.Path.PathExtensions
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
import dev.argon.backend.{Backend, ResourceAccess}
import dev.argon.parser.SourceAST
import dev.argon.io.FilenameManip
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.module.PathResourceIndicator
import dev.argon.stream.builder.ZStreamSource
import zio.stream.{ZSink, ZStream}
import zio.interop.catz._

object Pipeline {

  private def ioToCompilationError(ex: IOException): ErrorList =
    NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

  private def createFileDataStream[P: Path : Tagged](path: P): stream.ZStream[FileIO[P], ErrorList, Char] =
    stream.ZStream.flatten(stream.ZStream.fromEffect(
      ZIO.access[FileIO[P]] { _.get.readText(ioToCompilationError)(path) }
    ))

  private def resolveGlob[P: Path : Tagged](globs: List[PathResourceIndicator[P]]): ZStream[FileIO[P], ErrorList, P] =
    ZStream.fromIterable(globs)
      .map { _.path }
      .flatMap { glob =>
        ZStream.fromEffect(
          FilenameManip.findGlob(glob).runCollect
            .mapError { ex => NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex))) }
        )
          .flatMap(ZStream.fromIterable(_))
      }

  private def findInputFiles[P: Path : Tagged](buildInfo: BuildInfo.Resolved[P]): ZStream[FileIO[P], ErrorList, InputFileInfo[ZIO[FileIO[P], ErrorList, *]]] =
    resolveGlob(buildInfo.project.inputFiles)
      .zipWithIndex
      .map { case (path, id) =>
        InputFileInfo[ZIO[FileIO[P], ErrorList, *]](FileSpec(FileID(id.toInt), path.fullPathString),
          ZStreamSource(createFileDataStream(path))
        )
      }

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): URIO[Console, Unit] = {
    import zio.interop.catz._

    msgs
      .traverse_ { msg =>
        putStrLn(msg.toString)
      }
  }

  def compileResult[P: Path : Tagged]
  (buildInfo: BuildInfo.Resolved[P])
  : ZManaged[BuildEnvironment with FileIO[P] with ResourceAccess[PathResourceIndicator[P]], ErrorList, buildInfo.backend.TCompilationOutput] =
    ZManaged.fromEffect(
      BuildProcess.parseInput(ZStreamSource(findInputFiles(buildInfo)))
        .foldLeftM(Vector.empty[SourceAST]) { (acc, ast) => IO.succeed(acc :+ ast) }
    )
      .flatMap {
        case (parsedInput, _) =>
          ZManaged.fromEffect(
            resolveGlob(buildInfo.project.references)
              .map(PathResourceIndicator(_))
              .runCollect
          )
            .flatMap { references =>
              BuildProcess.compile(
                buildInfo.backend : buildInfo.backend.type
              )(
                parsedInput,
                references.toVector,
                CompilerOptions(
                  moduleName = buildInfo.compilerOptions.moduleName
                ),
                buildInfo.backendOptions,
              )
            }
      }

  def run[P : Path: Tagged](buildInfo: BuildInfo.Resolved[P]): RIO[Console with BuildEnvironment with FileIO[P] with FileIOLite, Int] =
    compileResult(buildInfo)
      .use { output =>
        output.write(buildInfo.outputOptions)
      }
      .provideSomeLayer[BuildEnvironment with FileIO[P] with FileIOLite](ResourceAccess.forFileIO[P])
      .either
      .map {
        case Left(errors) => (errors.toList.toVector, 1)
        case Right(_) => (Vector.empty, 0)
      }
      .flatMap {
        case (messages, exitCode) =>
          printMessages[Vector, CompilationMessage](messages).as(exitCode)
      }

}

