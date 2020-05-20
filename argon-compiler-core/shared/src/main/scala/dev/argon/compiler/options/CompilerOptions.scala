package dev.argon.compiler.options
import cats.Applicative
import shapeless.Id
import CodecSelector.Instances._

final case class CompilerOptions[F[_], I]
(
  moduleName: F[String],
  inputFiles: F[FileList[I]],
  references: F[FileList[I]],
)

object CompilerOptions {

  val handler: OptionsHandler[CompilerOptions] = new OptionsHandler[CompilerOptions] {
    override def info[I]: CompilerOptions[OptionInfo[*, I], I] = CompilerOptions[OptionInfo[*, I], I](
      moduleName = OptionInfo(
        name = "moduleName",
        description = "The name of the module to be compiled.",
      ),
      inputFiles = OptionInfo(
        name = "inputFiles",
        description = "Argon source code to be compiled.",
        defaultValue = new FileList[I](List.empty),
      ),
      references = OptionInfo(
        name = "references",
        description = "Libraries to be referenced.",
        defaultValue = new FileList[I](List.empty)
      ),
    )

    override def converter[I]: OptionsConverter[CompilerOptions[*[_], I]] =
      new OptionsConverter[CompilerOptions[*[_], I]] {
        override def convert[A[_], B[_], C[_], F[_] : Applicative](optionsA: CompilerOptions[A, I], optionsB: CompilerOptions[B, I])(f: OptionsConverterFunction[A, B, C, F]): F[CompilerOptions[C, I]] =
          Applicative[F].map3(
            f(optionsA.moduleName, optionsB.moduleName),
            f(optionsA.inputFiles, optionsB.inputFiles),
            f(optionsA.references, optionsB.references),
          ) { (moduleName, inputFiles, references) =>
            CompilerOptions(
              moduleName = moduleName,
              inputFiles = inputFiles,
              references = references
            )
          }
      }

    override def optionsLoader[IOld, I]: OptionsLoader[CompilerOptions[Id, IOld], CompilerOptions[Id, I], IOld, I] = {
      import dev.argon.compiler.options.OptionsLoader.Implicits._
      OptionsLoader.apply
    }
  }

}
