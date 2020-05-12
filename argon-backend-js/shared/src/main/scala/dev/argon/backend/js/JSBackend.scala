package dev.argon.backend.js

import dev.argon.compiler._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.backend._
import dev.argon.compiler.core.Context
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.loaders.armodule.ArgonModuleLoader
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader
import dev.argon.project.{ProjectLoader, SingleFile}
import dev.argon.stream.builder.Source
import zio._
import zio.interop.catz._
import toml.Codecs._
import shapeless._
import dev.argon.project.ExtraTomlCodecs._
import toml.Parse.{Address, Message}
import toml.Value


object JSBackend extends Backend {

  override type BackendOptions[F[_], I] = JSBackendOptions[F, I]
  override type BackendOutputOptions[F[_], I] = JSOutputOptions[F, I]
  override type TCompilationOutput = CompilationOutputText[BackendOutputOptionsId]

  override val id: String = "js"
  override val name: String = "JavaScript"

  override def emptyBackendOptions[I]: JSBackendOptions[Option, I] = JSBackendOptions[Option, I](
    extern = None,
    inject = None,
  )
  override def inferBackendOptions[I](compilerOptions: CompilerOptions[Id], options: JSBackendOptions[Option, I]): BackendOptionsId[I] =
    JSBackendOptions[Id, I](
      extern = options.extern.getOrElse(Map.empty),
      inject = options.inject.getOrElse(JSInjectCode[Option](before = None, after = None)) match {
        case JSInjectCode(beforeOpt, afterOpt) =>
          JSInjectCode[Id](
            before = beforeOpt.flatten,
            after = afterOpt.flatten
          )
      }
    )

  override def backendOptionsProjectLoader[IOld, I]: ProjectLoader[BackendOptionsId[IOld], BackendOptionsId[I], IOld, I] = {
    import dev.argon.project.ProjectLoader.Implicits._
    ProjectLoader.apply
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, JSBackendOptions[Option, String]] =
    toml.Toml.parseAs[JSBackendOptions[Option, String]](table)

  override def emptyOutputOptions[I]: JSOutputOptions[Option, I] =
    JSOutputOptions[Option, I](None)

  override def inferOutputOptions(compilerOptions: CompilerOptions[Id], options: JSOutputOptions[Option, String]): BackendOutputOptionsId[String] =
    JSOutputOptions[Id, String](
      outputFile = options.outputFile.getOrElse(SingleFile(compilerOptions.moduleName + ".js")),
    )

  override def outputOptionsProjectLoader[IOld, I]: ProjectLoader[JSBackend.BackendOutputOptionsId[IOld], JSBackend.BackendOutputOptionsId[I], IOld, I] = {
    import dev.argon.project.ProjectLoader.Implicits._
    ProjectLoader.apply
  }

  override def parseOutputOptions(table: Value.Tbl): Either[(Address, Message), JSOutputOptions[Option, String]] =
    toml.Toml.parseAs[JSOutputOptions[Option, String]](table)

  override def compile[I <: ResourceIndicator: Tagged](input: CompilerInput[I, JSBackendOptions[Id, I]]): ZManaged[ResourceReader[I], ErrorList, TCompilationOutput] = {
    val context = JSContext(input)

    ZManaged.fromEffect(JSEmitter.make(context, input.backendOptions.inject)).flatMap { emitter =>
      context.module[JSContext with Context.WithRes[I]].mapM { module =>
        emitter.emitModule(module).map { jsModule =>
          createOutput(jsModule)
        }
      }.provideSomeLayer(JSBackendLoadService.forResourceReader[I, JSContext with Context.WithRes[I]])
    }
  }

  private def createOutput(jsModule: JSModule): CompilationOutputText[BackendOutputOptionsId] = new CompilationOutputText[BackendOutputOptionsId] {

    override def outputResource[I](options: BackendOutputOptionsId[I]): I = options.outputFile.file

    override val textStream: Source[Any, ErrorList, String, Unit] =
      JSAst.writeModule(jsModule)

  }

}
