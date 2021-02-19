package dev.argon.compiler.options

import cats.Applicative
import dev.argon.compiler.output.{BuildArtifact, ModuleBuildArtifact}
import dev.argon.options.{OptionDecoder, OptionID, OptionInfo, Options, OptionsHandler, SingleFile}
import shapeless.{::, HNil}

sealed trait GeneralOutputOptionID extends OptionID {
  override type ElementType <: BuildArtifact
}

object GeneralOutputOptionID {
  case object DeclarationModule extends GeneralOutputOptionID {
    override type ElementType = ModuleBuildArtifact
  }
  case object InterfaceModule extends GeneralOutputOptionID {
    override type ElementType = ModuleBuildArtifact
  }
}

object GeneralOutputOptions {

  val handler: OptionsHandler[GeneralOutputOptionID, Lambda[X => SingleFile]] =
    new OptionsHandler[GeneralOutputOptionID, Lambda[X => SingleFile]] {
      override type OptRepr[A[_]] = A[ModuleBuildArtifact] :: A[ModuleBuildArtifact] :: HNil

      override def ids: OptRepr[Lambda[X => GeneralOutputOptionID { type ElementType = X }]] =
        GeneralOutputOptionID.DeclarationModule :: GeneralOutputOptionID.InterfaceModule :: HNil

      override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: OptionsHandler.CombineFunction[GeneralOutputOptionID, A, B, C, F]): F[OptRepr[C]] = {
        import dev.argon.options.OptionCombineHelper._
        combineHLists(ids, lista, listb)(f)
      }

      override def reprToOptions[A[_]](list: OptRepr[A]): Options[A, GeneralOutputOptionID] =
        Options.fromFunction(new Options.OptionValueFunction[A, GeneralOutputOptionID] {
          override def apply[E](id: GeneralOutputOptionID { type ElementType = E }): A[E] = id match {
            case GeneralOutputOptionID.DeclarationModule => list.head
            case GeneralOutputOptionID.InterfaceModule => list.tail.head
          }
        })

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

      override def decoder: Options[Lambda[X => OptionDecoder[SingleFile]], GeneralOutputOptionID] = {
        import dev.argon.options.OptionDecoderHelper._
        reprToOptions(createDecoders[OptRepr[Lambda[X => OptionDecoder[SingleFile]]]])
      }
    }

}
