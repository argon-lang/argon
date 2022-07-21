package dev.argon.cli

import dev.argon.platform.*
import zio.*
import java.io.IOException

import scopt.{OParser, OEffect}

object Program extends PlatformApp {
  def runApp: ZIO[Environment & ZIOAppArgs, Any, ExitCode] =
    val builder = OParser.builder[Config]
    val parser =
      import builder.*
      OParser.sequence(
        programName("argon"),
        head("argon", "0.0.0"),

        cmd("compile")
          .action((_, c) => c.copy(command = Some(Command.Compile)))
          .text("Low-level compile command. See build command for higher level build system.")
          .children(
            arg[FilePath]("build-spec")
              .action((path, c) => c.copy(buildSpec = Some(path)))
              .text("Build spec TOML file")
          ),

        cmd("build")
          .action((_, c) => c.copy(command = Some(Command.Build)))
          .text("Automated build system for Argon")
          .children(
            arg[FilePath]("build-spec")
              .action((path, c) => c.copy(buildSpec = Some(path)))
              .text("Build spec TOML file")
          ),
      )
    end parser

    ZIOAppArgs.getArgs.flatMap { args =>
      val (config, effects) = OParser.runParser(parser, args, Config())
      ZIO.foreach(effects)(runOEffect)
        .foldZIO(
          failure = {
            case ex: IOException => ZIO.fail(ex)
            case exitCode: ExitCode => ZIO.succeed(exitCode)
          },
          success = _ => config match {
            case Some(config) => runCommand(config)
            case None => ZIO.succeed(ExitCode.failure)
          }
        )
    }
  end runApp

  private def runOEffect(effect: OEffect): IO[IOException | ExitCode, Unit] =
    effect match
      case OEffect.DisplayToOut(msg) => Console.printLine(msg)
      case OEffect.DisplayToErr(msg) => Console.printLineError(msg)
      case OEffect.ReportError(msg) => Console.printLineError("Error: " + msg)
      case OEffect.ReportWarning(msg) => Console.printLineError("Warning: " + msg)
      case OEffect.Terminate(Left(_)) => ZIO.fail(ExitCode.failure)
      case OEffect.Terminate(Right(_)) => ZIO.fail(ExitCode.success)
    end match

  private def runCommand(config: Config): ZIO[Environment, Any, ExitCode] =
    config.command match
      case None =>
        for
          _ <- Console.printLineError("No command specified")
        yield ExitCode.failure

      case Some(Command.Compile) =>
        for
          _ <- Console.printLineError("Compile command not implemented")
        yield ExitCode.failure

      case Some(Command.Build) =>
        for
          _ <- Console.printLineError("Build command not implemented")
        yield ExitCode.failure

}
