package dev.argon.compiler.options
import cats.Id
import dev.argon.options.{FileList, OptionID, OptionInfo, Options, OptionsHandler, OptionsHandlerImpl, TypedOptionID}


sealed trait CompilerOptionID extends OptionID {
  override type Decoded[A] = A
}
object CompilerOptionID {
  case object ModuleName extends TypedOptionID[Id, String] with CompilerOptionID
  case object InputFiles extends TypedOptionID[Id, FileList] with CompilerOptionID
  case object References extends TypedOptionID[Id, FileList] with CompilerOptionID
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

  val handler: OptionsHandler[CompilerOptionID, Id] = {
    new OptionsHandlerImpl[CompilerOptionID, Id] {
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
                defaultValue = new FileList(Seq.empty),
              )

            case CompilerOptionID.References =>
              OptionInfo(
                name = "references",
                description = "Libraries to be referenced.",
                defaultValue = new FileList(Seq.empty)
              )
          }
        })
    }
  }

}
