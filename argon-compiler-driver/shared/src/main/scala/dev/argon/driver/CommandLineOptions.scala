package dev.argon.driver

import cats.data.{NonEmptySeq, Validated}
import cats.implicits.given
import cats.kernel.Monoid
import com.monovore.decline.{Argument, Command, Opts}
import dev.argon.backend.BackendFactory
import dev.argon.backend.metadata.{BackendOption, BackendOptionOutput, OptionOccurrence, OptionType}
import dev.argon.compiler.TubeName
import dev.argon.io.PathLike

sealed trait CompilerDriverOptions

final case class StandardCompilerDriverOptions(
  command: CompilerDriverCommand,
) extends CompilerDriverOptions

final case class CompilerDriverVersion() extends CompilerDriverOptions

sealed trait CompilerDriverCommand

final case class CompileCommand(
  tubeName: TubeName,
  inputDir: PathLike,
  outputFile: PathLike,
  referencedTubes: Seq[PathLike],
  supportedPlatforms: Set[String],
  platformOptions: Map[String, Map[String, CompilerDriverOptions.OptionValue]],
) extends CompilerDriverCommand

final case class GenIRCommand(
  inputFile: PathLike,
  outputFile: PathLike,
  referencedTubes: Seq[PathLike],
  platform: String,
) extends CompilerDriverCommand

final case class CodegenCommand(
  platform: String,
  inputFile: PathLike,
  referencedTubes: Seq[PathLike],
  platformOptions: Map[String, CompilerDriverOptions.OptionValue],
  platformOutputOptions: Map[String, PathLike],
) extends CompilerDriverCommand

object CompilerDriverOptions {
  enum OptionValueAtom {
    case String(s: java.lang.String)
    case Bool(b: Boolean)
    case File(p: PathLike)
    case Directory(p: PathLike)
  }

  enum OptionValue {
    case Single(value: OptionValueAtom)
    case Many(values: NonEmptySeq[OptionValueAtom])
  }
  
  def command(backends: Seq[BackendFactory]): Command[CompilerDriverOptions] =
    Command("argon", "Argon compiler")(
      standardOptions(backends).orElse(
        Opts.flag("version", help = "Print the version of the compiler").map(_ => CompilerDriverVersion())
      )
    )
  
  private def compileCommand(backends: Seq[BackendFactory]): Command[CompileCommand] =
    Command("compile", "Compile a tube from Argon source code")(
      (
        Opts.option[String]("name", help = "The name of the tube.")
          .mapValidated { name =>
            TubeName.decode(name) match {
              case Some(value) => Validated.valid(value)
              case None => Validated.invalidNel(s"Invalid tube name: $name")
            }
          },
        Opts.option[PathLike]("input", short = "i", help = "Input source code directory"),
        Opts.option[PathLike]("output", short = "o", help = "Output file"),
        Opts.options[PathLike]("reference", short = "r", help = "Reference a compiled .artube file as a dependency")
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
            createOptionsFromMetadata(backend.metadata.backend.name)(backend.metadata.options.tube)
              .map(opt => Map(backend.metadata.backend.name -> opt))
          }
        ),
      ).mapN(CompileCommand.apply)
    )

  private def genirCommand: Command[GenIRCommand] =
    Command("genir", "Generate Argon VM IR from a compiled tube")(
      (
        Opts.option[PathLike]("input", short = "i", help = "Input tube file"),
        Opts.option[PathLike]("output", short = "o", help = "Output file"),
        Opts.options[PathLike]("reference", short = "r", help = "Reference a compiled .artube file as a dependency")
          .orEmpty,
        Opts.option[String]("platform", short = "p", help = "Target platform"),
      ).mapN(GenIRCommand.apply)
    )

  private def codegenCommand(backends: Seq[BackendFactory]): Command[CodegenCommand] = {
    Command("codegen", "Generate backend-specific output")(
      Monoid.combineAll(
        backends.map { backend =>
          Opts.subcommand(
            Command(backend.metadata.backend.name, s"Generate code for ${backend.metadata.backend.name}")(
              (
                Opts.option[PathLike]("input", short = "i", help = "Input tube file"),
                Opts.options[PathLike]("reference", short = "r", help = "Reference a compiled .artube file as a dependency")
                  .orEmpty,
                createOptionsFromMetadata(backend.metadata.backend.name)(backend.metadata.options.codegen),
                createOptionsFromOutputMetadata(backend.metadata.backend.name)(backend.metadata.options.output),
              ).mapN { (input, references, codegenOptions, outputOptions) =>
                CodegenCommand(backend.metadata.backend.name, input, references, codegenOptions, outputOptions)
              }
            )
          )
        }
      )
    )
  }


  private def standardOptions(backends: Seq[BackendFactory]): Opts[StandardCompilerDriverOptions] =
    Opts.subcommand(compileCommand(backends))
      .orElse(Opts.subcommand(genirCommand))
      .orElse(Opts.subcommand(codegenCommand(backends)))
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

        case OptionType.BinaryResource => createOptionForType[PathLike](OptionValueAtom.File.apply)
        case OptionType.DirectoryResource => createOptionForType[PathLike](OptionValueAtom.Directory.apply)
      }
    end createOptionFromMetadata


    combineOptMapsN(
      options
        .toSeq
        .sortBy(_._1)
        .map(createOptionFromMetadata)
    )
  end createOptionsFromMetadata


  def createOptionsFromOutputMetadata(backendName: String)(options: Map[String, BackendOptionOutput]): Opts[Map[String, PathLike]] =
    def createOptionFromMetadata(name: String, optionInfo: BackendOptionOutput): Opts[Map[String, PathLike]] =
      Opts.option[PathLike](s"$backendName-$name", help = optionInfo.description)
        .map { p => Map(name -> p) }


    combineOptMapsN(
      options
        .toSeq
        .sortBy(_._1)
        .map(createOptionFromMetadata)
    )
  end createOptionsFromOutputMetadata

}


