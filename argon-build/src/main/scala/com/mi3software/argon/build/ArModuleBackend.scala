package com.mi3software.argon.build
import java.io.OutputStream
import java.util.zip.ZipOutputStream

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.module._
import com.mi3software.argon.util.stream.ArStream
import scalapb.GeneratedMessage
import scalaz._
import Scalaz._
import com.mi3software.argon.build.project.{ProjectFileHandler, ProjectLoader}
import com.mi3software.argon.util.FileOperations
import scalaz.Leibniz.===
import scalaz.zio.{IO, ZIO}
import toml.Codecs._
import shapeless.{ Id => _, _ }

object ArModuleBackend extends Backend {
  override type TCompilationOutput[F[+_], I] = CompilationOutput[F, I]
  override type BackendOptions[F[_], I] = ModuleBackendOptions[F, I]


  override val id: String = "argon-module"
  override val name: String = "Argon Module"

  override def emptyBackendOptions[I]: ModuleBackendOptions[Option, I] = ModuleBackendOptions(None)
  override def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: ModuleBackendOptions[Option, String]): BackendOptionsId[String] =
    ModuleBackendOptions[Id, String](
      referenceModule = options.referenceModule.getOrElse(compilerOptions.moduleName + ".armodule")
    )

  override def projectLoader[F[_, _], I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I] = {
    import shapeless._
    import ProjectLoader.Implicits._

    ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Codec.Error, ModuleBackendOptions[Option, String]] =
    toml.Toml.parseAs[ModuleBackendOptions[Option, String]](table)


  override def compile[F[+ _], I: Show, A](input: CompilerInput[I, ModuleBackendOptions[Id, I]])(f: CompilationOutput[F, I] => F[A])(implicit comp: Compilation[F], res: ResourceAccess[F, I]): F[A] = {
    val context = new ModuleContext[F, I](input)
    val emitter = new ModuleEmitter[F, context.type](context)

    context.createModule { module =>
      f(createOutput(input.backendOptions.referenceModule)(emitter.emitModule(module)))
    }
  }

  private def createOutput[F[+_]: Monad, I](outputFile: I)(moduleStream: ArStream[F, (String, GeneratedMessage), Unit]): CompilationOutput[F, I] = new CompilationOutput[F, I] {

    override def write(implicit resourceAccess: ResourceAccess[F, I]): F[Unit] =
      resourceAccess.createOutputStream(outputFile) { stream =>
        resourceAccess.createZipOutputStream(stream) { zip =>
          moduleStream.foldLeftM(()) {
            case (_, _) => ()
          } {
            case (_, (path, message)) =>
              resourceAccess.createZipEntry(zip, path) { entry =>
                resourceAccess.writeProtocolBufferMessage(zip, message)
              }
          }
        }
      }


  }

}
