package dev.argon.backend.module

import dev.argon.compiler._
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, CompilationOutput, ProjectLoader, ResourceAccess}
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.ZipEntryInfo
import dev.argon.stream.builder.Source
import toml.Codecs._
import shapeless.{Id => _, _}
import zio.{ZIO, ZManaged}



object ArModuleBackend extends Backend {
  override type TCompilationOutput = CompilationOutput
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

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, ModuleBackendOptions[Option, String]] =
    toml.Toml.parseAs[ModuleBackendOptions[Option, String]](table)


  override def compile(input: CompilerInput[ResourceIndicator, ModuleBackendOptions[Id, ResourceIndicator]]): ZManaged[ResourceAccess, ErrorList, CompilationOutput] = {
    val context = new ModuleContext(input)
    val emitter = new ModuleEmitter[context.type](context)

    context.module[ModuleBackendLoadService].map { module =>
      createOutput(input.backendOptions.referenceModule)(emitter.emitModule(module))
    }.provideSomeLayer(ModuleBackendLoadService.uponResourceAccess)
  }

  private def createOutput
  (outputFile: ResourceIndicator)
  (moduleGen: Source[Comp, (String, GeneratedMessage), Unit])
  : CompilationOutput = new CompilationOutput {

    override def write: RComp[ResourceAccess, Unit] =
      ZIO.accessM[ResourceAccess] { env =>
        val res = env.get

        val zipEntries = moduleGen.map { case (path, message) =>
          ZipEntryInfo(path, res.serializeProtocolBuffer(message))
        }

        val zipData = res.zipFromEntries(zipEntries)

        res.writeToResource(outputFile)(zipData)
      }

  }

}
