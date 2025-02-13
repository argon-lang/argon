package dev.argon.cli

import dev.argon.io.PathLike
import dev.argon.compiler.TubeName

import scopt.*

enum Command derives CanEqual {
  case Compile
  case GenerateIR
  case CodeGen
}

enum CodegenBackend derives CanEqual {
  case JS
}

final case class ArgonCommandLineOptions(
  command: Option[Command] = None,
  codegenBackend: Option[CodegenBackend] = None,

  tubeName: Option[String] = None,
  inputDir: Option[PathLike] = None,
  inputFile: Option[PathLike] = None,
  outputFile: Option[PathLike] = None,
  outputDir: Option[PathLike] = None,
  referencedTubes: Seq[PathLike] = Seq(),
  externs: Seq[PathLike] = Seq(),
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
            .action((path, c) => c.copy(referencedTubes = c.referencedTubes :+ path)),
        ),

      cmd("codegen")
        .action((_, c) => c.copy(command = Some(Command.CodeGen)))
        .text("Generate backend-specific output")
        .children(
          cmd("js")
            .action((_, c) => c.copy(codegenBackend = Some(CodegenBackend.JS)))
            .text("Generate JS output")
            .children(
              opt[PathLike]('i', "input")
                .required()
                .action((path, c) => c.copy(inputFile = Some(path))),

              opt[PathLike]('o', "output")
                .required()
                .action((path, c) => c.copy(outputDir = Some(path))),
          
              opt[PathLike]('e', "externs")
                .action((path, c) => c.copy(externs = c.externs :+ path)),
            )
        )
    )
  end parser
}

