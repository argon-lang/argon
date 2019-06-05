package dev.argon.backend.module

import dev.argon.compiler._
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.compiler.backend.{Backend, CompilationOutput, ProjectLoader}
import dev.argon.stream.{ArStream, ZipEntryInfo}
import toml.Codecs._
import shapeless.{Id => _, _}



object ArModuleBackend extends Backend {
  override type TCompilationOutput[F[-_, +_, +_], R, I] = CompilationOutput[F, R, I]
  override type BackendOptions[F[_], I] = ModuleBackendOptions[F, I]


  override val id: String = "argon-module"
  override val name: String = "Argon Module"

  override def emptyBackendOptions[I]: ModuleBackendOptions[Option, I] = ModuleBackendOptions(None)
  override def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: ModuleBackendOptions[Option, String]): BackendOptionsId[String] =
    ModuleBackendOptions[Id, String](
      referenceModule = options.referenceModule.getOrElse(compilerOptions.moduleName + ".armodule")
    )

  override def projectLoader[I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I] = {
    import ProjectLoader.Implicits._

    ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, ModuleBackendOptions[Option, String]] =
    toml.Toml.parseAs[ModuleBackendOptions[Option, String]](table)


  override def compile[F[-_, +_, +_], R, I: Show, A](input: CompilerInput[I, ModuleBackendOptions[Id, I]])(f: CompilationOutput[F, R, I] => F[R, NonEmptyList[CompilationError], A])(implicit compInstance: CompilationRE[F, R], res: ResourceAccess[F, R, I]): F[R, NonEmptyList[CompilationError], A] = {
    val context = new ModuleContext[F, R, I](input)
    val emitter = new ModuleEmitter[F, R, context.type](context)

    context.createModule { module =>
      f(createOutput(input.backendOptions.referenceModule)(emitter.emitModule(module)))
    }
  }

  private def createOutput[F[-_, +_, +_], R, I](outputFile: I)(moduleStream: ArStream[F, R, NonEmptyList[CompilationError], (String, GeneratedMessage)])(implicit compInstance: CompilationRE[F, R]): CompilationOutput[F, R, I] = new CompilationOutput[F, R, I] {

    override def write(implicit resourceAccess: ResourceAccess[F, R, I]): F[R, NonEmptyList[CompilationError], Unit] =
      resourceAccess.resourceSink(outputFile).use { sink =>
        resourceAccess.zipFromEntries(
          moduleStream.map { case (path, message) =>
            ZipEntryInfo(path, resourceAccess.protocolBufferStream(message))
          }
        )
          .foldLeft(sink)
      }


  }

}
