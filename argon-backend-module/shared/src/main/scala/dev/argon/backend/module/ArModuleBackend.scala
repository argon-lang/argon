package dev.argon.backend.module

import dev.argon.compiler._
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, CompilationOutput, ResourceAccess, ResourceReader, ResourceWriter}
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.ZipEntryInfo
import dev.argon.project.{ProjectLoader, SingleFile}
import dev.argon.project.ExtraTomlCodecs._
import dev.argon.stream.builder.Source
import toml.Codecs._
import shapeless._
import toml.Parse.{Address, Message}
import toml.Value
import zio._
import zio.stream._



object ArModuleBackend extends Backend {

  override type BackendOptions[F[_], I] = ModuleBackendOptions[F, I]
  override type BackendOutputOptions[F[_], I] = ModuleOutputOptions[F, I]
  override type TCompilationOutput = CompilationOutput[BackendOutputOptionsId]


  override val id: String = "argon-module"
  override val name: String = "Argon Module"

  override def emptyBackendOptions[I]: ModuleBackendOptions[Option, I] = ModuleBackendOptions()
  override def inferBackendOptions[I](compilerOptions: CompilerOptions[Id], options: ModuleBackendOptions[Option, I]): BackendOptionsId[I] =
    ModuleBackendOptions[Id, I]()

  override def backendOptionsProjectLoader[IOld, I]: ProjectLoader[BackendOptionsId[IOld], BackendOptionsId[I], IOld, I] = {
    import dev.argon.project.ProjectLoader.Implicits._
    ProjectLoader.apply
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, ModuleBackendOptions[Option, String]] =
    toml.Toml.parseAs[ModuleBackendOptions[Option, String]](table)

  override def emptyOutputOptions[I]: ModuleOutputOptions[Option, I] =
    ModuleOutputOptions[Option, I](None)

  override def inferOutputOptions(compilerOptions: CompilerOptions[Id], options: ModuleOutputOptions[Option, String]): BackendOutputOptionsId[String] =
    ModuleOutputOptions[Id, String](
      referenceModule = options.referenceModule.getOrElse(new SingleFile(compilerOptions.moduleName + ".armodule"))
    )

  override def outputOptionsProjectLoader[IOld, I]: ProjectLoader[BackendOutputOptionsId[IOld], BackendOutputOptionsId[I], IOld, I] = {
    import dev.argon.project.ProjectLoader.Implicits._
    ProjectLoader.apply
  }

  override def parseOutputOptions(table: Value.Tbl): Either[(Address, Message), ModuleOutputOptions[Option, String]] =
    toml.Toml.parseAs[ModuleOutputOptions[Option, String]](table)

  override def compile[I <: ResourceIndicator : Tagged](input: CompilerInput[I, ModuleBackendOptions[Id, I]]): ZManaged[ResourceReader[I], ErrorList, TCompilationOutput] = {
    val context = ModuleContext(input)

    context.module[ModuleContext with Context.WithRes[I]].map { module =>
      createOutput(ModuleEmitter.emitModule(context)(module))
    }.provideSomeLayer(ModuleBackendLoadService.forResourceReader[I, ModuleContext with Context.WithRes[I]])
  }

  private def createOutput(moduleGen: Stream[ErrorList, ModuleEmitter.StreamElem]): CompilationOutput[BackendOutputOptionsId] =
    new CompilationOutput[BackendOutputOptionsId] {

      override def write[I <: ResourceIndicator : Tagged](options: BackendOutputOptionsId[I]): RComp[ResourceWriter[I], Unit] =
        ZIO.accessM[ResourceWriter[I]] { env =>
          val res = env.get

          val zipEntries = moduleGen.map { case (path, message) =>
            ZipEntryInfo(path, res.serializeProtocolBuffer(message))
          }

          val zipData = res.zipFromEntries(zipEntries)

          res.writeToResource(options.referenceModule.file)(zipData)
        }

    }

}
