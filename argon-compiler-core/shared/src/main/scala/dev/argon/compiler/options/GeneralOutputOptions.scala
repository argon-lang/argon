package dev.argon.compiler.options

import dev.argon.compiler.output.{BuildArtifact, ModuleBuildArtifact}
import dev.argon.options.{OptionID, OptionInfo, Options, OptionsHandler, OptionsHandlerImpl, SingleFile, TypedOptionID}

sealed trait GeneralOutputOptionID extends OptionID {
  override type ElementType <: BuildArtifact
  override type Decoded[_] = SingleFile
}

object GeneralOutputOptionID {
  type AsFile[_] = SingleFile
  case object DeclarationModule extends TypedOptionID[AsFile, ModuleBuildArtifact] with GeneralOutputOptionID
  case object InterfaceModule extends TypedOptionID[AsFile, ModuleBuildArtifact] with GeneralOutputOptionID
}

object GeneralOutputOptions {

  val handler: OptionsHandler[GeneralOutputOptionID, Lambda[X => SingleFile]] =
    new OptionsHandlerImpl[GeneralOutputOptionID, Lambda[X => SingleFile]] {
      override def info: Options[OptionInfo, GeneralOutputOptionID] =
        Options.fromFunction(new Options.OptionValueFunction[OptionInfo, GeneralOutputOptionID] {
          override def apply[E](id: GeneralOutputOptionID { type ElementType = E }): OptionInfo[E] = id match {
            case GeneralOutputOptionID.DeclarationModule =>
              OptionInfo(
                name = "referenceModule",
                description = "The reference module that will contain the interface of the compiled module"
              )

            case GeneralOutputOptionID.InterfaceModule =>
              OptionInfo(
                name = "declarationModule",
                description = "The declaration module that will contain the serialized compiled module"
              )
          }
        })
    }

}
