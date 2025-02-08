package dev.argon.cli

import dev.argon.io.*
import dev.argon.platform.*
import dev.argon.util.{*, given}
import dev.argon.build.{BuildError, Compile}
import esexpr.ESExprException
import zio.*
import zio.stream.*

import java.io.IOException
import scopt.{OEffect, OParser}

import java.util.concurrent.TimeUnit
import dev.argon.compiler.TubeName
import dev.argon.build.InvalidTubeName
import dev.argon.compiler.Context
import dev.argon.compiler.ErrorLog
import scala.reflect.TypeTest
import dev.argon.build.CContext
import dev.argon.source.SourceError
import dev.argon.compiler.HasContext
import dev.argon.tube.resource.TubeResourceContext
import dev.argon.build.LogReporter
import dev.argon.tube.loader.TubeFormatException
import dev.argon.io.DirectoryResource
import dev.argon.source.ArgonSourceCodeResource
import dev.argon.compiler.TubeImporter
import dev.argon.build.GenerateIR
import dev.argon.vm.resource.VmIrResourceContext

object Program extends PlatformApp[IOException | BuildError | SourceError | TubeFormatException] {

  def runApp: ZIO[Environment & ZIOAppArgs, Error, ExitCode] =
    val parser = ArgonCommandLineOptions.parser

    ZIOAppArgs.getArgs.flatMap { args =>
      val (options, effects) = OParser.runParser(parser, args, ArgonCommandLineOptions())
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

  private def runCommand(options: ArgonCommandLineOptions): ZIO[Environment, Error, ExitCode] =
    options.command match
      case None =>
        for
          _ <- Console.printLineError("No command specified")
        yield ExitCode.failure

      case Some(Command.Compile) =>
        runCompile(options)
          .as(ExitCode.success)
          .provideSomeLayer[Environment](LogReporter.live)
        
      case Some(Command.GenerateIR) =>
        runGenIR(options)
          .as(ExitCode.success)
          .provideSomeLayer[Environment](LogReporter.live)
    end match

  private def runCompile(options: ArgonCommandLineOptions): ZIO[Environment & ErrorLog & LogReporter, Error, Unit] =
    for
      tubeNameDec <- ZIO.fromOption(options.tubeName.flatMap(TubeName.decode)).mapError(_ => InvalidTubeName(options.tubeName.get))

      ctx = new Context {
        override type Env = Program.this.Environment & ErrorLog & LogReporter
        override type Error = Program.this.Error

        override def environmentTag: EnvironmentTag[Env] = summon

        override def errorTypeTest: TypeTest[Any, Error] = summon
      }

      tubeResContext <- TubeResourceContext.make(ctx)

      compile = new Compile {
        override val context: ctx.type = ctx

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override def tubeName: TubeName = tubeNameDec
        override def inputDir: DirectoryResource[context.Error, ArgonSourceCodeResource] =
          DirectoryResource.decode[Error, BinaryResource, ArgonSourceCodeResource](
            PathUtil.directoryResource(options.inputDir.get)
              .filterFiles(_.endsWith(".argon"))
          )

        override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
          options.referencedTubes.map { refTubePath =>
            summon[BinaryResourceDecoder[tubeResContext.TubeResource, Error]].decode(
              PathUtil.binaryResource(refTubePath)
            )
          }
      }

      _ <- ZIO.scoped(
        for
          buildOutput <- compile.compile()
          _ <- PathUtil.writeFile(options.outputFile.get, buildOutput.tube)
        yield ()
      )

    yield ()

  private def runGenIR(options: ArgonCommandLineOptions): ZIO[Environment & ErrorLog & LogReporter, Error, Unit] =
    val ctx = new Context {
      override type Env = Program.this.Environment & ErrorLog & LogReporter
      override type Error = Program.this.Error

      override def environmentTag: EnvironmentTag[Env] = summon
      override def errorTypeTest: TypeTest[Any, Error] = summon
    }
    for
      tubeResContext <- TubeResourceContext.make(ctx)
      irResContext <- VmIrResourceContext.make(ctx)

      compile = new GenerateIR {
        override val context: ctx.type = ctx

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override val vmirResourceContext: irResContext.type = irResContext

        override def inputTube(using TubeImporter & HasContext[ctx.type]): TubeResource[context.Error] =
          summon[BinaryResourceDecoder[tubeResContext.TubeResource, Error]].decode(
            PathUtil.binaryResource(options.inputFile.get)
          )

        override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
          options.referencedTubes.map { refTubePath =>
            summon[BinaryResourceDecoder[tubeResContext.TubeResource, Error]].decode(
              PathUtil.binaryResource(refTubePath)
            )
          }
      }

      _ <- ZIO.scoped(
        for
          buildOutput <- compile.compile()
          _ <- PathUtil.writeFile(options.outputFile.get, buildOutput.tube)
        yield ()
      )

    yield ()
  end runGenIR

}
