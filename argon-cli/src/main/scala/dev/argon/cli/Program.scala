package dev.argon.cli

import dev.argon.io.*
import dev.argon.platform.*
import dev.argon.util.{*, given}
import dev.argon.build.{BuildError, Compile}
import dev.argon.backend.{BackendException, BackendFactory, Backends, CodeGenerator}
import dev.argon.backend.options.OptionValue
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
import dev.argon.vm.resource.VmIrResource

object Program extends PlatformApp[IOException | BuildError | SourceError | TubeFormatException | BackendException] {

  def runApp: ZIO[Environment & ZIOAppArgs, Error, ExitCode] =
    val parser = ArgonCommandLineOptions.parser

    ZIOAppArgs.getArgs
      .flatMap { args =>
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
//                for
//                  fiber <- runCommand(config).fork
//                  _ <- Clock.sleep(Duration.fromSeconds(15))
//                  trace <- fiber.trace
//                  _ <- Console.printLineError(trace)
//                  res <- fiber.join
//                yield res

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

      case Some(Command.CodeGen) =>
        options.codegenBackend match
          case None =>
            for
              _ <- Console.printLineError("No backend specified")
            yield ExitCode.failure

          case Some(backendName) =>
            Backends.allBackendFactories.find(_.metadata.backend.name == backendName) match
              case None =>
                Console.printLineError(s"Could not find backend $backendName").as(ExitCode.failure)

              case Some(backend) =>
                if options.outputOptions.isEmpty then
                  Console.printLineError(s"No outputs were specified").as(ExitCode.failure)
                else
                  runCodegen(backend, options)
                    .as(ExitCode.success)
            end match
        end match
    end match

  private def runCompile(options: ArgonCommandLineOptions): ZIO[Environment & ErrorLog & LogReporter, Error, Unit] =
    for
      tubeNameDec <- ZIO.fromOption(options.tubeName.flatMap(TubeName.decode)).mapError(_ => InvalidTubeName(options.tubeName.get))

      ctx = Context.Impl[Environment & ErrorLog & LogReporter, Error]()

      tubeResContext <- TubeResourceContext.make(ctx)

      compile = new Compile {
        override val context: ctx.type = ctx

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override def tubeName: TubeName = tubeNameDec
        override def inputDir: DirectoryResource[context.Error, ArgonSourceCodeResource] =
          PathUtil.directoryResource(options.inputDir.get)
            .filterFiles(_.endsWith(".argon"))
            .decode[ArgonSourceCodeResource]

        override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
          options.referencedTubes.map { refTubePath =>
            PathUtil.binaryResource(refTubePath)
              .decode[tubeResContext.TubeResource]
          }
      }

      _ <- ZIO.scoped(
        for
          buildOutput <- compile.compile()
          _ <- PathUtil.writeFile(options.outputFile.get, buildOutput.tube)
        yield ()
      )

      _ <- ZIO.serviceWithZIO[LogReporter](_.reportLogs)
      _ <- ZIO.serviceWithZIO[LogReporter](_.failOnErrors)

    yield ()

  private def runGenIR(options: ArgonCommandLineOptions): ZIO[Environment & ErrorLog & LogReporter, Error, Unit] =
    val ctx = Context.Impl[Environment & ErrorLog & LogReporter, Error]
    for
      tubeResContext <- TubeResourceContext.make(ctx)

      compile = new GenerateIR {
        override val context: ctx.type = ctx

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override def inputTube(using TubeImporter & HasContext[ctx.type]): TubeResource[context.Error] =
          PathUtil.binaryResource(options.inputFile.get)
            .decode[tubeResContext.TubeResource]

        override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
          options.referencedTubes.map { refTubePath =>
            PathUtil.binaryResource(refTubePath)
              .decode[tubeResContext.TubeResource]
          }
      }

      _ <- ZIO.scoped(
        for
          buildOutput <- compile.compile()
          _ <- PathUtil.writeFile(options.outputFile.get, buildOutput.tube)
        yield ()
      )

      _ <- ZIO.serviceWithZIO[LogReporter](_.reportLogs)
      _ <- ZIO.serviceWithZIO[LogReporter](_.failOnErrors)

    yield ()
  end runGenIR

  private def runCodegen(backendFactory: BackendFactory, options: ArgonCommandLineOptions): ZIO[Environment, Error, Unit] =
    ZIO.scoped(
      for
        backend <- backendFactory.load[Error]
        codeGenOpts <- backend.codeGenerator.optionParser.parse(realizeBackendOptions(options.codegenOptions))
        output <- (backend.codeGenerator: CodeGenerator[Error, backend.Output] & backend.codeGenerator.type) match {
          case codeGen: (CodeGenerator.LibraryCodeGenerator[Error, backend.Output] & backend.codeGenerator.type) =>
            codeGen.codegen(
              options = codeGenOpts,
              program = PathUtil.binaryResource(options.inputFile.get).decode[VmIrResource],
              libraries = options.referencedTubes.map { refTubePath =>
                PathUtil.binaryResource(refTubePath)
                  .decode[VmIrResource]
              },
            )
        }

        outputMap <- backend.codeGenerator.outputProvider.outputs(output)

        _ <- ZIO.foreachParDiscard(options.outputOptions) { (outputName, outputPath) =>
          ZIO.fromEither(outputMap.get(outputName).toRight { BackendException(s"Invalid output: $outputName") })
            .flatMap {
              case FileSystemResource.Of(res) => PathUtil.writeFile(outputPath, res)
              case res: DirectoryResource[Error, BinaryResource] => PathUtil.writeDir(outputPath, res)
            }
        }

      yield ()
    )

  private def realizeBackendOptions(o: Map[String, ArgonCommandLineOptions.OptionValue]): Map[String, OptionValue[Error]] =
    o.view
      .mapValues(realizeBackendOptionValue)
      .toMap

  private def realizeBackendOptionValue(o: ArgonCommandLineOptions.OptionValue): OptionValue[Error] =
    def realizeAtom(a: ArgonCommandLineOptions.OptionValueAtom): OptionValue.Atom[Error] =
      a match {
        case ArgonCommandLineOptions.OptionValueAtom.String(s) => OptionValue.Atom.String(s)
        case ArgonCommandLineOptions.OptionValueAtom.Bool(b) => OptionValue.Atom.Bool(b)
        case ArgonCommandLineOptions.OptionValueAtom.File(path) => OptionValue.Atom.BinaryResource(PathUtil.binaryResource(path))
        case ArgonCommandLineOptions.OptionValueAtom.Directory(path) => OptionValue.Atom.DirectoryResource(PathUtil.directoryResource(path))
      }

    o match {
      case ArgonCommandLineOptions.OptionValue.Single(value) => OptionValue.Single(realizeAtom(value))
      case ArgonCommandLineOptions.OptionValue.Many(values) => OptionValue.ManyValues(values.map(realizeAtom))
    }
  end realizeBackendOptionValue

}
