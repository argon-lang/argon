package dev.argon.driver

import cats.data.{NonEmptySeq, Validated}
import cats.implicits.given
import cats.kernel.Monoid
import com.monovore.decline.{Argument, Command, Help, Opts}
import dev.argon.backend.metadata.{BackendMetadata, BackendOption, BackendOptionOutput, OptionOccurrence, OptionType, OutputType}
import dev.argon.compiler.TubeName
import dev.argon.driver.scalaApi.command as cmd
import esexpr.Dictionary

sealed trait CompilerDriverOptions

final case class StandardCompilerDriverOptions(
  command: CompilerDriverCommand,
) extends CompilerDriverOptions

final case class CompilerDriverVersion() extends CompilerDriverOptions

sealed trait CompilerDriverCommand

final case class CompileCommand(
  tubeName: TubeName,
  inputDir: String,
  outputFile: String,
  referencedTubes: Seq[String],
  supportedPlatforms: Set[String],
  platformOptions: Map[String, Map[String, CompilerDriverOptions.OptionValue]],
) extends CompilerDriverCommand

final case class GenIRCommand(
  inputFile: String,
  outputFile: String,
  referencedTubes: Seq[String],
  platform: String,
) extends CompilerDriverCommand

final case class CodegenCommand(
  backend: String,
  inputFile: String,
  referencedTubes: Seq[String],
  platformOptions: Map[String, CompilerDriverOptions.OptionValue],
  platformOutputOptions: Map[String, cmd.CompilerDriverOutput[String, String]],
) extends CompilerDriverCommand

final case class BackendsCommand() extends CompilerDriverCommand

object CompilerDriverOptions {
  enum OptionValueAtom {
    case String(s: java.lang.String)
    case Bool(b: Boolean)
    case File(p: java.lang.String)
    case Directory(p: java.lang.String)
  }

  enum OptionValue {
    case Single(value: OptionValueAtom)
    case Many(values: NonEmptySeq[OptionValueAtom])
  }
  
  def command(backends: Seq[BackendMetadata]): Command[CompilerDriverOptions] =
    Command("argon", "Argon compiler")(
      standardOptions(backends).orElse(
        Opts.flag("version", help = "Print the version of the compiler").map(_ => CompilerDriverVersion())
      )
    )
  
  private def compileCommand(backends: Seq[BackendMetadata]): Command[CompileCommand] =
    Command("compile", "Compile a tube from Argon source code")(
      (
        Opts.option[String]("name", help = "The name of the tube.")
          .mapValidated { name =>
            TubeName.decode(name) match {
              case Some(value) => Validated.valid(value)
              case None => Validated.invalidNel(s"Invalid tube name: $name")
            }
          },
        Opts.option[String]("input", short = "i", help = "Input source code directory"),
        Opts.option[String]("output", short = "o", help = "Output file"),
        Opts.options[String]("reference", short = "r", help = "Reference a compiled .artube file as a dependency")
          .orEmpty,
        Opts.options[String]("platform", short = "p", help = "Supported platforms for the tube")
          .orEmpty
          .mapValidated { platforms =>
            val platformSet = platforms.toSet
            if platformSet.size != platforms.size then
              Validated.invalidNel(s"Duplicate platforms: ${platforms.groupBy(identity).collect { case (k, v) if v.size > 1 => k }.mkString(", ")}")
            else
              Validated.valid(platformSet)
          },
        combineOptMapsN(
          backends.map { backend =>
            createOptionsFromMetadata(backend.backend.name)(backend.options.tube)
              .map(opt => Map(backend.backend.name -> opt))
          }
        ),
      ).mapN(CompileCommand.apply)
    )

  private def genirCommand: Command[GenIRCommand] =
    Command("genir", "Generate Argon VM IR from a compiled tube")(
      (
        Opts.option[String]("input", short = "i", help = "Input tube file"),
        Opts.option[String]("output", short = "o", help = "Output file"),
        Opts.options[String]("reference", short = "r", help = "Reference a compiled .artube file as a dependency")
          .orEmpty,
        Opts.option[String]("platform", short = "p", help = "Target platform"),
      ).mapN(GenIRCommand.apply)
    )

  private def codegenCommand(backends: Seq[BackendMetadata]): Command[CodegenCommand] = {
    Command("codegen", "Generate backend-specific output")(
      Monoid.combineAll(
        backends.map { backend =>
          Opts.subcommand(
            Command(backend.backend.name, s"Generate code for ${backend.backend.name}")(
              (
                Opts.option[String]("input", short = "i", help = "Input tube file"),
                Opts.options[String]("reference", short = "r", help = "Reference a compiled .artube file as a dependency")
                  .orEmpty,
                createOptionsFromMetadata(backend.backend.name)(backend.options.codegen),
                createOptionsFromOutputMetadata(backend.backend.name)(backend.options.output),
              ).mapN { (input, references, codegenOptions, outputOptions) =>
                CodegenCommand(backend.backend.name, input, references, codegenOptions, outputOptions)
              }
            )
          )
        }
      )
    )
  }

  private def backendsCommand: Command[BackendsCommand] =
    Command("backends", "List available backends")(
      Opts.unit.map(_ => BackendsCommand())
    )
  

  private def standardOptions(backends: Seq[BackendMetadata]): Opts[StandardCompilerDriverOptions] =
    Opts.subcommand(compileCommand(backends))
      .orElse(Opts.subcommand(genirCommand))
      .orElse(Opts.subcommand(codegenCommand(backends)))
      .orElse(Opts.subcommand(backendsCommand))
      .map(StandardCompilerDriverOptions.apply)
  
  private def combineOptMaps2[K, V](a: Opts[Map[K, V]], b: Opts[Map[K, V]]): Opts[Map[K, V]] =
    (a, b).mapN { (a, b) =>
      a ++ b
    }
    
  private def combineOptMapsN[K, V](opts: Seq[Opts[Map[K, V]]]): Opts[Map[K, V]] =
    opts.foldLeft(Opts.unit.map(_ => Map.empty[K, V]))(combineOptMaps2)

  private def createOptionsFromMetadata(backendName: String)(options: Map[String, BackendOption]): Opts[Map[String, OptionValue]] =
    def createOptionFromMetadata(name: String, optionInfo: BackendOption): Opts[Map[String, OptionValue]] =
      val optionName = s"$backendName-$name"
      
      def createOptionForType[A: Argument](f: A => OptionValueAtom): Opts[Map[String, OptionValue]] =
        optionInfo.occurrence match {
          case OptionOccurrence.Default | OptionOccurrence.Optional =>
            Opts.option[A](optionName, help = optionInfo.description).orNone.map {
              case None => Map()
              case Some(a) => Map(name -> OptionValue.Single(f(a)))
            }

          case OptionOccurrence.Required =>
            Opts.option[A](optionName, help = optionInfo.description).map { a =>
              Map(name -> OptionValue.Single(f(a)))
            }

          case OptionOccurrence.Many =>
            Opts.options[A](optionName, help = optionInfo.description)
              .orEmpty
              .map { a =>
                NonEmptySeq.fromSeq(a) match {
                  case None => Map()
                  case Some(nes) => Map(name -> OptionValue.Many(nes.map(f)))
                }
              }

          case OptionOccurrence.ManyRequired =>
            Opts.options[A](optionName, help = optionInfo.description).map { a =>
              val aSeq = NonEmptySeq(a.head, a.tail)
              Map(name -> OptionValue.Many(aSeq.map(f)))
            }
        }


      optionInfo.`type` match {
        case OptionType.String => createOptionForType[String](OptionValueAtom.String.apply)
        case OptionType.Bool =>
          Opts.flag(optionName, help = optionInfo.description)
            .orFalse
            .map(b => Map(name -> OptionValue.Single(OptionValueAtom.Bool(b))))

        case OptionType.BinaryResource => createOptionForType[String](OptionValueAtom.File.apply)
        case OptionType.DirectoryResource => createOptionForType[String](OptionValueAtom.Directory.apply)
      }
    end createOptionFromMetadata


    combineOptMapsN(
      options
        .toSeq
        .sortBy(_._1)
        .map(createOptionFromMetadata)
    )
  end createOptionsFromMetadata


  def createOptionsFromOutputMetadata(backendName: String)(options: Map[String, BackendOptionOutput]): Opts[Map[String, cmd.CompilerDriverOutput[String, String]]] =
    def createOptionFromMetadata(name: String, optionInfo: BackendOptionOutput): Opts[Map[String, cmd.CompilerDriverOutput[String, String]]] =
      Opts.option[String](s"$backendName-$name", help = optionInfo.description)
        .orNone
        .map {
          case Some(p) =>
            val value = optionInfo.`type` match {
              case OutputType.BinaryResource => cmd.CompilerDriverOutput.File(p)
              case OutputType.DirectoryResource => cmd.CompilerDriverOutput.Directory(p)
            }
            Map(name -> value)
          case None => Map()
        }


    combineOptMapsN(
      options
        .toSeq
        .sortBy(_._1)
        .map(createOptionFromMetadata)
    )
  end createOptionsFromOutputMetadata


  def toDriverCommand(arguments: Seq[String], command: Either[Help, CompilerDriverOptions]): cmd.DriverCommand[String, String, String, String] =
    command match {
      case Left(help) => cmd.DriverCommand.HelpCommand(
        isError = help.errors.nonEmpty,
        arguments = arguments
      )
      case Right(CompilerDriverVersion()) => cmd.DriverCommand.VersionCommand()
      case Right(StandardCompilerDriverOptions(BackendsCommand())) => cmd.DriverCommand.ListBackendsCommand()
      case Right(StandardCompilerDriverOptions(compileCommand: CompileCommand)) =>
        cmd.DriverCommand.CompileCommand(
          tubeName = dev.argon.vm.TubeName(compileCommand.tubeName.parts.head, compileCommand.tubeName.parts.tail),
          inputDir = compileCommand.inputDir,
          outputFile = compileCommand.outputFile,
          referencedTubes = compileCommand.referencedTubes,
          supportedPlatforms = compileCommand.supportedPlatforms.toSeq,
          platformOptions = Dictionary(
            compileCommand.platformOptions
              .view
              .mapValues { platformOpt =>
                Dictionary(
                  platformOpt.view.mapValues(convertOptionValue).toMap
                )
              }
              .toMap
          ),
        )

      case Right(StandardCompilerDriverOptions(genIRCommand: GenIRCommand)) =>
        cmd.DriverCommand.GenIrCommand(
          inputFile = genIRCommand.inputFile,
          outputFile = genIRCommand.outputFile,
          referencedTubes = genIRCommand.referencedTubes,
          platform = genIRCommand.platform,
        )

      case Right(StandardCompilerDriverOptions(codegenCommand: CodegenCommand)) =>
        cmd.DriverCommand.CodegenCommand(
          backend = codegenCommand.backend,
          inputFile = codegenCommand.inputFile,
          referencedTubes = codegenCommand.referencedTubes,
          platformOptions = Dictionary(
            codegenCommand.platformOptions
              .view
              .mapValues(convertOptionValue)
              .toMap
          ),
          platformOutputOptions = Dictionary(codegenCommand.platformOutputOptions),
        )
    }
    
  

  private def convertOptionValue(value: OptionValue): cmd.CompilerDriverOptionValue[String, String] =
    value match {
      case OptionValue.Single(value) => cmd.CompilerDriverOptionValue.Single(convertOptionValueAtom(value))
      case OptionValue.Many(values) =>
        val convValues = values.map(convertOptionValueAtom)

        cmd.CompilerDriverOptionValue.Many(
          convValues.head,
          convValues.tail
        )
    }

  private def convertOptionValueAtom(value: OptionValueAtom): cmd.CompilerDriverOptionValueAtom[String, String] =
    value match {
      case OptionValueAtom.String(s) => cmd.CompilerDriverOptionValueAtom.String(s)
      case OptionValueAtom.Bool(b) => cmd.CompilerDriverOptionValueAtom.Bool(b)
      case OptionValueAtom.File(p) => cmd.CompilerDriverOptionValueAtom.File(p)
      case OptionValueAtom.Directory(p) => cmd.CompilerDriverOptionValueAtom.Directory(p)
    }

}


