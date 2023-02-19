package dev.argon.cli

import dev.argon.io.{BinaryResourceDecoder, JsonResource, ResourceWriter, ResourceReader}
import dev.argon.platform.*
import dev.argon.util.toml.Toml
import dev.argon.util.{*, given}
import dev.argon.build.Compile
import zio.*

import java.io.IOException
import scopt.{OEffect, OParser}

import java.util.concurrent.TimeUnit

object Program extends PlatformApp {
  def runApp: ZIO[Environment & ZIOAppArgs, Error, ExitCode] =
    val builder = OParser.builder[Options]
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
      val (options, effects) = OParser.runParser(parser, args, Options())
      ZIO.foreach(effects)(runOEffect)
        .foldZIO(
          failure = {
            case ex: IOException => ZIO.fail(ex)
            case exitCode: ExitCode => ZIO.succeed(exitCode)
          },
          success = _ => options match {
            case Some(config) =>
              runCommand(config)
//              for
//                fiber <- runCommand(config).fork
//                _ <- Clock.sleep(Duration.fromSeconds(15))
//                trace <- fiber.trace
//                _ <- Console.printLineError(trace)
//                res <- fiber.join
//              yield res

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

  private def runCommand(options: Options): ZIO[Environment, Error, ExitCode] =
    options.command match
      case None =>
        for
          _ <- Console.printLineError("No command specified")
        yield ExitCode.failure

      case Some(Command.Compile) =>
        for
          configRes <- ZIO.serviceWith[PathUtil](_.binaryResource(options.buildSpec.get))
          config <- summon[BinaryResourceDecoder[JsonResource, Environment, Error]].decode(configRes).decode[Toml]
          baseDir <- ZIO.serviceWithZIO[PathUtil](_.dirname(options.buildSpec.get))
          layer <- ZIO.serviceWith[PathUtil](_.resourceLayer(baseDir))
          _ <- Compile.compile(config, plugins).provideSomeLayer[Environment](layer)
        yield ExitCode.success


      case Some(Command.Build) =>
        for
          _ <- Console.printLineError("Build command not implemented")
        yield ExitCode.failure
    end match

}
