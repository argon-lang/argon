package dev.argon.driver

import dev.argon.backend.options.OptionValue
import dev.argon.backend.{BackendContext, BackendException, BackendExternProvider, BackendProvider}
import dev.argon.build.{BuildError, Compile, GenerateIR, InvalidTubeName, LogReporter}
import dev.argon.compiler.{ErrorLog, ExternProvider, HasContext, TubeImporter, TubeName}
import dev.argon.io.{DirectoryResource, PathUtil}
import dev.argon.parser.SyntaxError
import dev.argon.source.ArgonSourceCodeResource
import dev.argon.tube.loader.TubeFormatException
import dev.argon.tube.resource.TubeResourceContext

import java.io.IOException
import zio.{ExitCode, ZIO}

private[driver] object CompilerDriverImpl {
  type Error = IOException | BackendException | TubeFormatException | SyntaxError | BuildError

  def runCommand(args: Seq[String]): ZIO[BackendProvider, Error, ExitCode] =
    ZIO.serviceWith[BackendProvider] { backendProvider => CompilerDriverOptions.command(backendProvider.all.toSeq) }
      .flatMap { command =>
        command.parse(args, Map.empty) match {
          case Left(help) if help.errors.nonEmpty =>
            zio.Console.printLineError(help).as(ExitCode.failure)

          case Left(help) =>
            zio.Console.printLineError(help).as(ExitCode.success)

          case Right(CompilerDriverVersion()) =>
            zio.Console.printLine(s"Argon compiler driver version ${BuildInfo.version}")
              .as(ExitCode.success)

          case Right(StandardCompilerDriverOptions(command)) =>
            command match {
              case command: CompileCommand =>
                runCompile(command)
                  .as(ExitCode.success)
                  .provideSomeLayer[BackendProvider](LogReporter.live)

              case command: GenIRCommand =>
                runGenIR(command)
                  .as(ExitCode.success)
                  .provideSomeLayer[BackendProvider](LogReporter.live)
                
              case command: CodegenCommand => ???
            }
        }
      }


  private def runCompile(options: CompileCommand): ZIO[ErrorLog & LogReporter & BackendProvider, Error, Unit] =
    ZIO.scoped {
      val ctx = BackendContext[ErrorLog & LogReporter, Error]()
      for

        given (ExternProvider & HasContext[ctx.type]) <- BackendExternProvider.make(ctx)(
          options.supportedPlatforms,
          options.platformOptions.view.mapValues(realizeBackendOptions).toMap,
        )

        tubeResContext <- TubeResourceContext.make(ctx)

        compile: Compile { val context: ctx.type } = new Compile {
          override val context: ctx.type = ctx

          override val tubeResourceContext: tubeResContext.type =
            tubeResContext

          import tubeResourceContext.TubeResource

          override def tubeName: TubeName = options.tubeName

          override def inputDir: DirectoryResource[context.Error, ArgonSourceCodeResource] =
            PathUtil.directoryResource(options.inputDir)
              .filterFiles(_.endsWith(".argon"))
              .decode[ArgonSourceCodeResource]

          override def referencedTubes(using TubeImporter & HasContext[ctx.type]): Seq[TubeResource[context.Error]] =
            options.referencedTubes.map { refTubePath =>
              PathUtil.binaryResource(refTubePath)
                .decode[tubeResContext.TubeResource]
            }
        }

        _ <- ZIO.scoped[ErrorLog & LogReporter](
          for
            buildOutput <- compile.compile()
            _ <- PathUtil.writeFile(options.outputFile, buildOutput.tube)
          yield ()
        )

        _ <- ZIO.serviceWithZIO[LogReporter](_.reportLogs)
        _ <- ZIO.serviceWithZIO[LogReporter](_.failOnErrors)

      yield ()
    }

  private def runGenIR(options: GenIRCommand): ZIO[ErrorLog & LogReporter, Error, Unit] =
    val ctx = BackendContext[ErrorLog & LogReporter, Error]()
    for
      tubeResContext <- TubeResourceContext.make(ctx)

      compile = new GenerateIR {
        override val context: ctx.type = ctx

        override def platformId: String = options.platform

        override val tubeResourceContext: tubeResContext.type =
          tubeResContext

        import tubeResourceContext.TubeResource

        override def inputTube(using TubeImporter & HasContext[ctx.type]): TubeResource[context.Error] =
          PathUtil.binaryResource(options.inputFile)
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
          _ <- PathUtil.writeFile(options.outputFile, buildOutput.tube)
        yield ()
      )

      _ <- ZIO.serviceWithZIO[LogReporter](_.reportLogs)
      _ <- ZIO.serviceWithZIO[LogReporter](_.failOnErrors)

    yield ()
  end runGenIR

  private def realizeBackendOptions(o: Map[String, CompilerDriverOptions.OptionValue]): Map[String, OptionValue[Error]] =
    o.view
      .mapValues(realizeBackendOptionValue)
      .toMap

  private def realizeBackendOptionValue(o: CompilerDriverOptions.OptionValue): OptionValue[Error] =
    def realizeAtom(a: CompilerDriverOptions.OptionValueAtom): OptionValue.Atom[Error] =
      a match {
        case CompilerDriverOptions.OptionValueAtom.String(s) => OptionValue.Atom.String(s)
        case CompilerDriverOptions.OptionValueAtom.Bool(b) => OptionValue.Atom.Bool(b)
        case CompilerDriverOptions.OptionValueAtom.File(path) => OptionValue.Atom.BinaryResource(PathUtil.binaryResource(path))
        case CompilerDriverOptions.OptionValueAtom.Directory(path) => OptionValue.Atom.DirectoryResource(PathUtil.directoryResource(path))
      }

    o match {
      case CompilerDriverOptions.OptionValue.Single(value) => OptionValue.Single(realizeAtom(value))
      case CompilerDriverOptions.OptionValue.Many(values) => OptionValue.ManyValues(values.map(realizeAtom))
    }
  end realizeBackendOptionValue
}
