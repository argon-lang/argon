package dev.argon.build

import dev.argon.io.Path
import dev.argon.compiler._
import cats._
import cats.data.Ior
import cats.implicits._
import zio._
import zio.console._
import dev.argon.backend
import dev.argon.backend.ResourceAccess
import dev.argon.io.fileio.{FileIO, FileIOLite}
import dev.argon.compiler.options.CompilerOptions

object Pipeline {

  def printMessages[TMsg <: Diagnostic](msgs: List[TMsg]): URIO[Console, Unit] =
    ZIO.foreach_(msgs) { msg =>
      putStrLn(msg.toString)
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
    .catchAllCause { cause =>
      val errors = cause.failures
      val remaining = cause.stripFailures

      if(errors.nonEmpty) {
        val errorAction =
          if(remaining.isEmpty) IO.fail(ExitCode.failure)
          else IO.halt(remaining)

        printMessages(errors) *> errorAction
      }
      else {
        IO.halt(remaining)
      }
    }

}

