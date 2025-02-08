package dev.argon.cli

import dev.argon.io.PathLike
import dev.argon.compiler.TubeName

import scopt.*

enum Command derives CanEqual {
  case Compile
  case GenerateIR
}

final case class ArgonCommandLineOptions(
  command: Option[Command] = None,  

  tubeName: Option[String] = None,
  inputDir: Option[PathLike] = None,
  inputFile: Option[PathLike] = None,
  outputFile: Option[PathLike] = None,
  referencedTubes: Seq[PathLike] = Seq(),
)

object ArgonCommandLineOptions {
  def parser: OParser[?, ArgonCommandLineOptions] =
    val builder = OParser.builder[ArgonCommandLineOptions]
    import builder.*

    OParser.sequence(
      programName("argon"),
      head("argon", "0.0.0"),

      cmd("compile")
        .action((_, c) => c.copy(command = Some(Command.Compile)))
        .text("Compile Argon source code into a tube")
        .children(
          opt[String]("name")
            .action((name, c) => c.copy(tubeName = Some(name))),

          opt[PathLike]('i', "input")
            .action((path, c) => c.copy(inputDir = Some(path))),

          opt[PathLike]('o', "output")
            .action((path, c) => c.copy(outputFile = Some(path))),
          
          opt[PathLike]('r', "reference")
            .action((path, c) => c.copy(referencedTubes = c.referencedTubes :+ path)),
        ),

      cmd("genir")
        .action((_, c) => c.copy(command = Some(Command.GenerateIR)))
        .text("Generate Argon VM IR from a compiled tube")
        .children(
          opt[PathLike]('i', "input")
            .action((path, c) => c.copy(inputFile = Some(path))),

          opt[PathLike]('o', "output")
            .action((path, c) => c.copy(outputFile = Some(path))),
          
          opt[PathLike]('r', "reference")
            .action((path, c) => c.copy(referencedTubes = c.referencedTubes :+ path)),
        ),
    )
  end parser
}

