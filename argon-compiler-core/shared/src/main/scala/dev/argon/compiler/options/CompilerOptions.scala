package dev.argon.compiler.options

import cats.Id
import dev.argon.options.{FileList, OptionID, OptionIDBase, OptionInfo, Options, OptionsHandler, OptionsHandlerImpl, TypedOptionID}


sealed trait CompilerOptionID extends OptionID {
  override type Decoded[A] = A
}
object CompilerOptionID {
  case object ModuleName extends OptionIDBase[Id, String] with CompilerOptionID {
    override val info: OptionInfo[String] = OptionInfo(
      name = "moduleName",
      description = "The name of the module to be compiled.",
    )
  }
  case object InputFiles extends OptionIDBase[Id, FileList] with CompilerOptionID {
    override val info: OptionInfo[FileList] =
      OptionInfo(
        name = "inputFiles",
        description = "Argon source code to be compiled.",
        defaultValue = new FileList(Seq.empty),
      )
  }
  case object References extends OptionIDBase[Id, FileList] with CompilerOptionID {
    override val info: OptionInfo[FileList] =
      OptionInfo(
        name = "references",
        description = "Libraries to be referenced.",
        defaultValue = new FileList(Seq.empty)
      )
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
      override def apply[E](id: CompilerOptionID with TypedOptionID[E]): A[E] = id match {
        case CompilerOptionID.ModuleName => moduleName
        case CompilerOptionID.InputFiles => inputFiles
        case CompilerOptionID.References => references
      }
    })

  val handler: OptionsHandler[CompilerOptionID, Id] = new OptionsHandlerImpl[CompilerOptionID, Id]

}
