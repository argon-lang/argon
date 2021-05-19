package dev.argon.compiler.options

import dev.argon.backend.Backend.AsFile
import dev.argon.compiler.output.{BuildArtifact, ModuleBuildArtifact}
import dev.argon.options.{OptionID, OptionIDBase, OptionInfo, OptionsHandler, OptionsHandlerImpl, SingleFile}

sealed trait GeneralOutputOptionID extends OptionID {
  override type ElementType <: BuildArtifact
  override type Decoded[_] = SingleFile
}

object GeneralOutputOptionID {
  case object DeclarationModule extends OptionIDBase[AsFile, ModuleBuildArtifact] with GeneralOutputOptionID {
    override val info: OptionInfo[ModuleBuildArtifact] =
      OptionInfo(
        name = "referenceModule",
        description = "The reference module that will contain the interface of the compiled module"
      )
  }
  case object InterfaceModule extends OptionIDBase[AsFile, ModuleBuildArtifact] with GeneralOutputOptionID {
    override val info: OptionInfo[ModuleBuildArtifact] =
      OptionInfo(
        name = "declarationModule",
        description = "The declaration module that will contain the serialized compiled module"
      )
  }
}

object GeneralOutputOptions {

  val handler: OptionsHandler[GeneralOutputOptionID, AsFile] =
    new OptionsHandlerImpl[GeneralOutputOptionID, AsFile]

}
