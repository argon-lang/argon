package dev.argon.compiler.options

import dev.argon.compiler.output.BuildArtifact
import dev.argon.options.{OptionID, Options, SingleFile}

final case class CompilerOutput[B <: OptionID { type ElementType <: BuildArtifact }]
(
  generalOutput: Options[Lambda[X => Option[SingleFile]], GeneralOutputOptionID],
  backendOutput: Options[Lambda[X => Option[SingleFile]], B],
)