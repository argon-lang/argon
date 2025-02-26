package dev.argon.cli

import cats.data.NonEmptySeq
import dev.argon.backend.Backends
import dev.argon.backend.metadata.{BackendOption, BackendOptionOutput, OptionOccurrence, OptionType, OutputType}
import dev.argon.io.PathLike
import dev.argon.compiler.TubeName
import monocle.Lens
import monocle.macros.GenLens
import scopt.*



enum Command derives CanEqual {
  case Compile
  case GenerateIR
  case CodeGen
}

final case class ArgonCommandLineOptions(
  command: Option[Command] = None,
  selectedBackend: Option[String] = None,

  tubeName: Option[String] = None,
  inputDir: Option[PathLike] = None,
  inputFile: Option[PathLike] = None,
  outputFile: Option[PathLike] = None,
  outputDir: Option[PathLike] = None,
  referencedTubes: Seq[PathLike] = Seq(),

  supportedPlatforms: Set[String] = Set(),
  platformTubeOptions: Map[String, Map[String, ArgonCommandLineOptions.OptionValue]] = Map(),

  codegenOptions: Map[String, ArgonCommandLineOptions.OptionValue] = Map(),
  outputOptions: Map[String, PathLike] = Map(),
)

object ArgonCommandLineOptions {
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


  def parser: OParser[?, ArgonCommandLineOptions] =
    val builder = OParser.builder[ArgonCommandLineOptions]
    import builder.*


    def createOptionsFromMetadata(backendName: String, lens: Lens[ArgonCommandLineOptions, Map[String, OptionValue]])(options: Map[String, BackendOption]): Seq[OParser[?, ArgonCommandLineOptions]] =
      def createOptionFromMetadata(name: String, optionInfo: BackendOption): OParser[?, ArgonCommandLineOptions] =
        def createOptionForType[A: Read](f: A => OptionValueAtom): OParser[?, ArgonCommandLineOptions] =
          val action: (A, ArgonCommandLineOptions) => ArgonCommandLineOptions =
            optionInfo.occurrence match {
              case OptionOccurrence.Default | OptionOccurrence.Optional | OptionOccurrence.Required =>
                (a, o) => lens.at(name).replace(Some(OptionValue.Single(f(a))))(o)

              case OptionOccurrence.Many | OptionOccurrence.ManyRequired =>
                (a, o) =>
                  lens.at(name).modify {
                    case Some(OptionValue.Many(items)) => Some(OptionValue.Many(items :+ f(a)))
                    case _ => Some(OptionValue.Many(NonEmptySeq.of(f(a))))
                  }(o)
            }

          var o = opt[A](s"$backendName-$name")
            .action(action)


          optionInfo.occurrence match {
            case OptionOccurrence.Default | OptionOccurrence.Optional => ()
            case OptionOccurrence.Required =>
              o = o.required()

            case OptionOccurrence.Many =>
              o = o.unbounded()

            case OptionOccurrence.Many | OptionOccurrence.ManyRequired =>
              o = o.required().unbounded()
          }

          o
        end createOptionForType


        optionInfo.`type` match {
          case OptionType.String => createOptionForType[String](OptionValueAtom.String.apply)
          case OptionType.Bool =>
            optionInfo.occurrence match {
              case OptionOccurrence.Default =>
                createOptionForType[Unit](_ => OptionValueAtom.Bool(true))

              case _ =>
                createOptionForType[Boolean](OptionValueAtom.Bool.apply)
            }

          case OptionType.BinaryResource => createOptionForType[PathLike](OptionValueAtom.File.apply)
          case OptionType.DirectoryResource => createOptionForType[PathLike](OptionValueAtom.Directory.apply)
        }
      end createOptionFromMetadata


      options
        .toSeq
        .sortBy(_._1)
        .map(createOptionFromMetadata)
    end createOptionsFromMetadata


    def createOptionsFromOutputMetadata(backendName: String, lens: Lens[ArgonCommandLineOptions, Map[String, PathLike]])(options: Map[String, BackendOptionOutput]): Seq[OParser[?, ArgonCommandLineOptions]] =
      def createOptionFromMetadata(name: String, optionInfo: BackendOptionOutput): OParser[?, ArgonCommandLineOptions] =
        opt[PathLike](s"$backendName-$name")
          .action((a, o) => lens.at(name).replace(Some(a))(o))

      end createOptionFromMetadata


      options
        .toSeq
        .sortBy(_._1)
        .map(createOptionFromMetadata)
    end createOptionsFromOutputMetadata

    OParser.sequence(
      programName("argon"),
      head("argon", "0.0.0"),

      cmd("compile")
        .action((_, c) => c.copy(command = Some(Command.Compile)))
        .text("Compile Argon source code into a tube")
        .children(
          (
            Seq(
              opt[String]("name")
                .required()
                .action((name, c) => c.copy(tubeName = Some(name))),

              opt[PathLike]('i', "input")
                .required()
                .action((path, c) => c.copy(inputDir = Some(path))),

              opt[PathLike]('o', "output")
                .required()
                .action((path, c) => c.copy(outputFile = Some(path))),

              opt[PathLike]('r', "reference")
                .action((path, c) => c.copy(referencedTubes = c.referencedTubes :+ path)),

              opt[String]("platform")
                .action((name, c) => c.copy(supportedPlatforms = c.supportedPlatforms + name)),
            ) ++
              Backends.allBackendFactories
                .flatMap { backendFactory =>
                  val backendName = backendFactory.metadata.backend.name

                  createOptionsFromMetadata(backendName, GenLens[ArgonCommandLineOptions](_.platformTubeOptions).at(backendName).withDefault(Map.empty))(backendFactory.metadata.options.codegen)
                }
          )*
        ),

      cmd("genir")
        .action((_, c) => c.copy(command = Some(Command.GenerateIR)))
        .text("Generate Argon VM IR from a compiled tube")
        .children(
          opt[PathLike]('i', "input")
            .required()
            .action((path, c) => c.copy(inputFile = Some(path))),

          opt[PathLike]('o', "output")
            .required()
            .action((path, c) => c.copy(outputFile = Some(path))),
          
          opt[PathLike]('r', "reference")
            .unbounded()
            .action((path, c) => c.copy(referencedTubes = c.referencedTubes :+ path)),

          opt[String]("platform")
            .required()
            .action((platform, c) => c.copy(selectedBackend = Some(platform))),
        ),

      cmd("codegen")
        .action((_, c) => c.copy(command = Some(Command.CodeGen)))
        .text("Generate backend-specific output")
        .children(
          Backends.allBackendFactories
            .map { backendFactory =>
              val backendName = backendFactory.metadata.backend.name

              val codegenOpts =
                Seq(
                  opt[PathLike]('i', "input")
                    .required()
                    .action((path, c) => c.copy(inputFile = Some(path))),

                  opt[PathLike]('r', "reference")
                    .unbounded()
                    .action((path, c) => c.copy(referencedTubes = c.referencedTubes :+ path)),

                ) ++
                  createOptionsFromMetadata(backendName, GenLens[ArgonCommandLineOptions](_.codegenOptions))(backendFactory.metadata.options.codegen) ++
                  createOptionsFromOutputMetadata(backendName, GenLens[ArgonCommandLineOptions](_.outputOptions))(backendFactory.metadata.options.output)

              cmd(backendName)
                .action((_, c) => c.copy(selectedBackend = Some(backendName)))
                .text(s"Generate output using the $backendName backend")
                .children(codegenOpts*)
            }*
        )
    )
  end parser

}

