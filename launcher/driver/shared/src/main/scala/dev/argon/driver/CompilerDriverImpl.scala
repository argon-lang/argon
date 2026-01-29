package dev.argon.driver

import dev.argon.backend.options.OptionValue
import dev.argon.backend.{BackendContext, BackendException, BackendExternProvider, BackendProvider, CodeGenerator, DirectoryResourceWrap, scalaApi as bScalaApi}
import dev.argon.backend.scalaApi.DirectoryResourceExtensions.*
import dev.argon.backend.scalaApi.SinkExtensions.*
import dev.argon.build.{BuildError, Compile, GenerateIR, InvalidTubeName, LogReporter}
import dev.argon.compiler.{ErrorLog, ExternProvider, HasContext, TubeImporter, TubeName}
import dev.argon.driver.scalaApi.command as cmd
import dev.argon.io.{BinaryResource, DirectoryResource, FileSystemResource}
import dev.argon.vm.resource.VmIrResource
import dev.argon.parser.SyntaxError
import dev.argon.source.ArgonSourceCodeResource
import dev.argon.tube.loader.TubeFormatException
import dev.argon.tube.resource.TubeResourceContext
import com.monovore.decline.Help
import esexpr.Dictionary

import java.io.IOException
import zio.*
import cats.data.NonEmptySeq
import dev.argon.util.async.ErrorWrapper

private[driver] object CompilerDriverImpl {
  type Error = IOException | BackendException | TubeFormatException | SyntaxError | BuildError

  type LiveDriverCommand = cmd.DriverCommand[
    bScalaApi.BinaryResource[IOException],
    bScalaApi.DirectoryResource[IOException],
    bScalaApi.BinaryResourceSink[IOException],
    bScalaApi.DirectoryResourceSink[IOException],
  ]

  type LiveDriverCompileCommand = cmd.DriverCommand.CompileCommand[
    bScalaApi.BinaryResource[IOException],
    bScalaApi.DirectoryResource[IOException],
    bScalaApi.BinaryResourceSink[IOException],
    bScalaApi.DirectoryResourceSink[IOException],
  ]

  type LiveDriverGenIRCommand = cmd.DriverCommand.GenIrCommand[
    bScalaApi.BinaryResource[IOException],
    bScalaApi.DirectoryResource[IOException],
    bScalaApi.BinaryResourceSink[IOException],
    bScalaApi.DirectoryResourceSink[IOException],
  ]

  type LiveDriverCodegenCommand = cmd.DriverCommand.CodegenCommand[
    bScalaApi.BinaryResource[IOException],
    bScalaApi.DirectoryResource[IOException],
    bScalaApi.BinaryResourceSink[IOException],
    bScalaApi.DirectoryResourceSink[IOException],
  ]

  def runCommand(command: LiveDriverCommand): ZIO[BackendProvider, Error, ExitCode] =
    command match {
      case cmd.DriverCommand.HelpCommand(true, arguments) =>
        ZIO.serviceWith[BackendProvider] { backendProvider => CompilerDriverOptions.command(backendProvider.all.map(_.metadata)) }
          .flatMap { command =>
            val help = command.parse(arguments, Map.empty)
              .swap
              .getOrElse { Help.fromCommand(command) }

            Console.printLineError(help).as(ExitCode.failure)
          }

      case cmd.DriverCommand.HelpCommand(false, _) =>
        ZIO.serviceWith[BackendProvider] { backendProvider => CompilerDriverOptions.command(backendProvider.all.map(_.metadata)) }
          .flatMap { command =>
            val help = Help.fromCommand(command)
            Console.printLineError(help).as(ExitCode.success)
          }

      case cmd.DriverCommand.VersionCommand() =>
        Console.printLine(s"Argon compiler driver version ${BuildInfo.version}")
          .as(ExitCode.success)

      case cmd.DriverCommand.Rpc() =>
        Console.printLineError(s"Argon RPC interface must be implemented by the driver.")
          .as(ExitCode.failure)

      case cmd.DriverCommand.ListBackendsCommand() =>
        ZIO.serviceWithZIO[BackendProvider] { bp =>
          ZIO.foreachDiscard(bp.all) { bf => Console.printLine(bf.metadata.backend.name) }
            .as(ExitCode.success)
        }


      case command: LiveDriverCompileCommand =>
        runCompile(command)
          .as(ExitCode.success)
          .provideSomeLayer[BackendProvider](LogReporter.live)

      case command: LiveDriverGenIRCommand =>
        runGenIR(command)
          .as(ExitCode.success)
          .provideSomeLayer[BackendProvider](LogReporter.live)

      case command: LiveDriverCodegenCommand =>
        runCodegen(command)
          .as(ExitCode.success)
          .provideSomeLayer[BackendProvider](LogReporter.live)
    }

  private def runCompile[E: ErrorWrapper](options: LiveDriverCompileCommand): ZIO[ErrorLog & LogReporter & BackendProvider, Error | E, Unit] =
    ZIO.scoped {
      for
        ctx = BackendContext[ErrorLog & LogReporter, Error]()

        given (ExternProvider & HasContext[ctx.type]) <- BackendExternProvider.make(ctx)(
          options.supportedPlatforms.toSet,
          options.platformOptions.dict.view.mapValues(realizeBackendOptions).toMap,
        )

        tubeResContext <- TubeResourceContext.make(ctx)

        compile: Compile { val context: ctx.type } = new Compile {
          override val context: ctx.type = ctx

          override val tubeResourceContext: tubeResContext.type =
            tubeResContext

          import tubeResourceContext.TubeResource

          override def tubeName: TubeName = TubeName(options.tubeName.head, options.tubeName.tail*)

          override def inputDir: DirectoryResource[context.Error, ArgonSourceCodeResource] =
            options.inputDir.toIOResource
              .filterFiles(_.endsWith(".argon"))
              .decode[ArgonSourceCodeResource]

          override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
            options.referencedTubes.map { refTubePath =>
              refTubePath.toIOResource
                .decode[tubeResContext.TubeResource]
            }
        }

        _ <- ZIO.scoped[ErrorLog & LogReporter](
          for
            buildOutput <- compile.compile()
            _ <- options.outputFile.write(buildOutput.tube)
          yield ()
        )

        _ <- ZIO.serviceWithZIO[LogReporter](_.reportLogs)
        _ <- ZIO.serviceWithZIO[LogReporter](_.failOnErrors)

      yield ()
    }

  private def runGenIR(options: LiveDriverGenIRCommand): ZIO[ErrorLog & LogReporter, Error, Unit] =
    for
      ctx = BackendContext[ErrorLog & LogReporter, Error]()
      tubeResContext <- TubeResourceContext.make(ctx)

      compile = new GenerateIR {
        override val context: ctx.type = ctx

        override def platformId: String = options.platform

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override def inputTube(using TubeImporter & HasContext[ctx.type]): TubeResource[context.Error] =
          options.inputFile.toIOResource
            .decode[tubeResContext.TubeResource]

        override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
          options.referencedTubes.map { refTubePath =>
            refTubePath.toIOResource
              .decode[tubeResContext.TubeResource]
          }
      }

      _ <- ZIO.scoped(
        for
          buildOutput <- compile.compile()
          _ <- options.outputFile.write(buildOutput.tube)
        yield ()
      )

      _ <- ZIO.serviceWithZIO[LogReporter](_.reportLogs)
      _ <- ZIO.serviceWithZIO[LogReporter](_.failOnErrors)

    yield ()
  end runGenIR
  
  private def runCodegen[E: ErrorWrapper](options: LiveDriverCodegenCommand): ZIO[ErrorLog & LogReporter & BackendProvider, Error, Unit] =
    ZIO.scoped(
      for
        backendFactory <- ZIO.serviceWithZIO[BackendProvider](_.getBackendFactory(options.backend))
        backend <- backendFactory.load[Error]
        codeGenOpts <- backend.codeGenerator.optionParser.parse(realizeBackendOptions(options.platformOptions))
        output <- (backend.codeGenerator: CodeGenerator[Error, backend.Output] & backend.codeGenerator.type) match {
          case codeGen: (CodeGenerator.LibraryCodeGenerator[Error, backend.Output] & backend.codeGenerator.type) =>
            codeGen.codegen(
              options = codeGenOpts,
              program = options.inputFile.toIOResource.decode[VmIrResource],
              libraries = options.referencedTubes.map { refTubePath =>
                refTubePath.toIOResource
                  .decode[VmIrResource]
              },
            )
        }

        outputMap <- backend.codeGenerator.outputProvider.outputs(output)

        _ <- ZIO.foreachParDiscard(options.platformOutputOptions.dict) { (outputName, outputSink) =>
          ZIO.fromEither(outputMap.get(outputName).toRight { BackendException(s"Invalid output: $outputName") })
            .flatMap {
              case FileSystemResource.Of(res) =>
                outputSink match {
                  case cmd.CompilerDriverOutput.File(outputSink) => outputSink.write(res)
                  case _ => ZIO.fail(BackendException(s"Got directory for output $outputName when expecting file"))
                }
              case res: DirectoryResource[Error, BinaryResource] =>
                outputSink match {
                  case cmd.CompilerDriverOutput.Directory(outputSink) => outputSink.writeAll(res)
                  case _ => ZIO.fail(BackendException(s"Got directory for output $outputName when expecting file"))
                }
            }
        }

      yield ()
    )

  private def realizeBackendOptions(o: Dictionary[cmd.CompilerDriverOptionValue[bScalaApi.BinaryResource[IOException], bScalaApi.DirectoryResource[IOException]]]): Map[String, OptionValue[Error]] =
    o.dict.view
      .mapValues(realizeBackendOptionValue)
      .toMap

  private def realizeBackendOptionValue(o: cmd.CompilerDriverOptionValue[bScalaApi.BinaryResource[IOException], bScalaApi.DirectoryResource[IOException]]): OptionValue[Error] =
    def realizeAtom(a: cmd.CompilerDriverOptionValueAtom[bScalaApi.BinaryResource[IOException], bScalaApi.DirectoryResource[IOException]]): OptionValue.Atom[Error] =
      a match {
        case cmd.CompilerDriverOptionValueAtom.String(s) => OptionValue.Atom.String(s)
        case cmd.CompilerDriverOptionValueAtom.Bool(b) => OptionValue.Atom.Bool(b)
        case cmd.CompilerDriverOptionValueAtom.File(res) => OptionValue.Atom.BinaryResource(res.toIOResource)
        case cmd.CompilerDriverOptionValueAtom.Directory(res) => OptionValue.Atom.DirectoryResource(res.toIOResource)
      }

    o match {
      case cmd.CompilerDriverOptionValue.Single(value) => OptionValue.Single(realizeAtom(value))
      case cmd.CompilerDriverOptionValue.Many(h, t) => OptionValue.ManyValues(NonEmptySeq(h, t).map(realizeAtom))
    }
  end realizeBackendOptionValue

}
