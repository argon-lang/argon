package dev.argon

import shapeless._

import ArgumentParser._
import CommandLineParser.Implicits._

final case class CommandLineArguments[F[_]]
(
  command: F[ArgonCommand[Id]]
)

sealed trait ArgonCommand[F[_]]

final case class CompileCommand[F[_]]
(
  file: F[String],
) extends ArgonCommand[F]


object CommandLineArguments {

  val parser: CommandLineArguments[ArgumentParser] = CommandLineArguments(
    command = subcommands[ArgonCommand[Id]](
      subcommand("compile", description = "Compile an argon program.")(
        CompileCommand(
          file = rest("file", description = "Build definition TOML file.")
        )
      ),
    )
  )

}