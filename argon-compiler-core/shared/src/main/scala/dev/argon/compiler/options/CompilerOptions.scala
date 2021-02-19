package dev.argon.compiler.options
import cats.{Applicative, Id}
import dev.argon.options.{FileList, OptionDecoder, OptionID, OptionInfo, Options, OptionsHandler}


sealed trait CompilerOptionID extends OptionID
object CompilerOptionID {
  case object ModuleName extends CompilerOptionID {
    override type ElementType = String
  }

  case object InputFiles extends CompilerOptionID {
    override type ElementType = FileList
  }

  case object References extends CompilerOptionID {
    override type ElementType = FileList
  }


}

object CompilerOptions {

  def apply[A[_]]
  (
    moduleName: A[String],
    inputFiles: A[FileList],
    references: A[FileList],
  ): Options[A, CompilerOptionID] =
    Options.fromFunction[A, CompilerOptionID](new Options.OptionValueFunction[A, CompilerOptionID] {
      override def apply[E](id: CompilerOptionID { type ElementType = E }): A[E] = id match {
        case CompilerOptionID.ModuleName => moduleName
        case CompilerOptionID.InputFiles => inputFiles
        case CompilerOptionID.References => references
      }
    })

  val handler: OptionsHandler[CompilerOptionID, Id] = new OptionsHandler[CompilerOptionID, Id] {
    import shapeless.{Id => _, _}

    override type OptRepr[A[_]] = A[String] :: A[FileList] :: A[FileList] :: HNil

    override def ids: OptRepr[Lambda[X => CompilerOptionID { type ElementType = X }]] =
      CompilerOptionID.ModuleName :: CompilerOptionID.InputFiles :: CompilerOptionID.References :: HNil

    override def combineRepr[A[_], B[_], C[_], F[_] : Applicative](lista: OptRepr[A], listb: OptRepr[B])(f: OptionsHandler.CombineFunction[CompilerOptionID, A, B, C, F]): F[OptRepr[C]] = {
      import dev.argon.options.OptionCombineHelper._
      combineHLists(ids, lista, listb)(f)
    }

    override def reprToOptions[A[_]](list: OptRepr[A]): Options[A, CompilerOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[A, CompilerOptionID] {
        override def apply[E](id: CompilerOptionID { type ElementType = E }): A[E] = id match {
          case CompilerOptionID.ModuleName => list.head
          case CompilerOptionID.InputFiles => list.tail.head
          case CompilerOptionID.References => list.tail.tail.head
        }

      })

    override def info: Options[OptionInfo, CompilerOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[OptionInfo, CompilerOptionID] {
        override def apply[E](id: CompilerOptionID { type ElementType = E }): OptionInfo[E] = id match {
          case CompilerOptionID.ModuleName =>
            OptionInfo(
              name = "moduleName",
              description = "The name of the module to be compiled.",
            )

          case CompilerOptionID.InputFiles =>
            OptionInfo(
              name = "inputFiles",
              description = "Argon source code to be compiled.",
              defaultValue = new FileList(List.empty),
            )

          case CompilerOptionID.References =>
            OptionInfo(
              name = "references",
              description = "Libraries to be referenced.",
              defaultValue = new FileList(List.empty)
            )
        }
      })


    override def decoder: Options[OptionDecoder, CompilerOptionID] = {
      import dev.argon.options.OptionDecoderHelper._
      reprToOptions(createDecoders[OptRepr[OptionDecoder]])
    }
  }

}
