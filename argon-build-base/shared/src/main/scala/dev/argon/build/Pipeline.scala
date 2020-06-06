package dev.argon.build

import dev.argon.io.Path
import dev.argon.compiler._
import cats._
import cats.implicits._
import zio._
import zio.console._
import dev.argon.backend
import dev.argon.backend.ResourceAccess
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.compiler.options.CompilerOptions
import dev.argon.module.PathResourceIndicator

object Pipeline {

  def printMessages[C[_] : Traverse, TMsg <: CompilationMessage](msgs: C[TMsg]): URIO[Console, Unit] = {
    import zio.interop.catz.core._

    msgs
      .traverse_ { msg =>
        putStrLn(msg.toString)
      }
  }

  def run[P : Path: Tag](buildInfo: backend.BuildInfo.Resolved[P]): ZIO[Console with BuildEnvironment with FileIO[P] with FileIOLite, ExitCode, Unit] =
    BuildProcess.compile(
      buildInfo.backend : buildInfo.backend.type
    )(
      buildInfo.compilerOptions,
      buildInfo.backendOptions,
    )
      .use { output =>
        output.write(buildInfo.outputOptions)
      }
      .provideSomeLayer[BuildEnvironment with FileIO[P] with FileIOLite](ResourceAccess.forFileIO[P])
      .flatMapError { errors =>
        printMessages(errors.toList.toVector).as(ExitCode.failure)
      }

}

