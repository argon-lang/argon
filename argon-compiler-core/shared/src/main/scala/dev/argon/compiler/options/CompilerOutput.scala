package dev.argon.compiler.options

import dev.argon.backend.Backend.AsFileOption
import dev.argon.compiler.output.BuildArtifact
import dev.argon.options.{OptionID, Options}

final case class CompilerOutput[B <: OptionID { type ElementType <: BuildArtifact }]
(
  generalOutput: Options[AsFileOption, GeneralOutputOptionID],
  backendOutput: Options[AsFileOption, B],
)