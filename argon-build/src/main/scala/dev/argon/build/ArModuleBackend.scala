package dev.argon.build
import java.io.OutputStream
import java.util.zip.ZipOutputStream

import dev.argon.compiler._
import dev.argon.compiler.module._
import dev.argon.util.stream._
import scalapb.GeneratedMessage
import cats._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.build.project.ProjectLoader
import toml.Codecs._
import shapeless.{Id => _, _}

object ArModuleBackend extends Backend {
  override type TCompilationOutput[F[+_, +_], I] = CompilationOutput[F, I]
  override type BackendOptions[F[_], I] = ModuleBackendOptions[F, I]


  override val id: String = "argon-module"
  override val name: String = "Argon Module"

  override def emptyBackendOptions[I]: ModuleBackendOptions[Option, I] = ModuleBackendOptions(None)
  override def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: ModuleBackendOptions[Option, String]): BackendOptionsId[String] =
    ModuleBackendOptions[Id, String](
      referenceModule = options.referenceModule.getOrElse(compilerOptions.moduleName + ".armodule")
    )

  override def projectLoader[I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I] = {
    import shapeless._
    import ProjectLoader.Implicits._

    ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, ModuleBackendOptions[Option, String]] =
    toml.Toml.parseAs[ModuleBackendOptions[Option, String]](table)


  override def compile[F[+_, +_]: CompilationE, I: Show, A](input: CompilerInput[I, ModuleBackendOptions[Id, I]])(f: CompilationOutput[F, I] => F[NonEmptyList[CompilationError], A])(implicit res: ResourceAccess[F[NonEmptyList[CompilationError], ?], I]): F[NonEmptyList[CompilationError], A] = {
    val context = new ModuleContext[F, I](input)
    val emitter = new ModuleEmitter[F, context.type](context)

    context.createModule { module =>
      f(createOutput(input.backendOptions.referenceModule)(emitter.emitModule(module)))
    }
  }

  private def createOutput[F[+_, +_]: CompilationE, I](outputFile: I)(moduleStream: ArStream[F, NonEmptyList[CompilationError], (String, GeneratedMessage)]): CompilationOutput[F, I] = new CompilationOutput[F, I] {

    override def write(implicit resourceAccess: ResourceAccess[F[NonEmptyList[CompilationError], ?], I]): F[NonEmptyList[CompilationError], Unit] =
      resourceAccess.createOutputStream(outputFile) { stream =>
        resourceAccess.createZipWriter(stream) { zip =>
          moduleStream.forEach { case (path, message) =>
            resourceAccess.writeZipEntry(zip, path) { entry =>
              resourceAccess.writeProtocolBufferMessage(entry, message)
            }
          }

        }
      }


  }

}
