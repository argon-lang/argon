package dev.argon.backend.js

import dev.argon.compiler._
import cats._
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.backend.{Backend, CompilationOutputText, ProjectLoader, ResourceAccess}
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.stream.builder.Source
import zio.{IO, ZIO, ZManaged}
import zio.interop.catz._
import toml.Codecs._
import shapeless.{Id => _, _}
import dev.argon.util.ExtraTomlCodecs._


object JSBackend extends Backend {

  override type TCompilationOutput = CompilationOutputText
  override type BackendOptions[F[_], I] = JSBackendOptions[F, I]

  override val id: String = "js"
  override val name: String = "JavaScript"

  override def emptyBackendOptions[I]: JSBackendOptions[Option, I] = JSBackendOptions[Option, I](
    outputFile = None,
    extern = None,
    inject = None,
  )
  override def inferBackendOptions(compilerOptions: CompilerOptions[Id], options: JSBackendOptions[Option, String]): BackendOptionsId[String] =
    JSBackendOptions[Id, String](
      outputFile = options.outputFile.getOrElse(compilerOptions.moduleName + ".js"),
      extern = options.extern.getOrElse(Map.empty),
      inject = options.inject.getOrElse(JSInjectCode[Option](before = None, after = None)) match {
        case JSInjectCode(beforeOpt, afterOpt) =>
          JSInjectCode[Id](
            before = beforeOpt.flatten,
            after = afterOpt.flatten
          )
      }
    )

  override def projectLoader[I]: ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I] = {
    import ProjectLoader.Implicits._

    ProjectLoader[BackendOptionsId[String], BackendOptionsId[I], I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, JSBackendOptions[Option, String]] =
    toml.Toml.parseAs[JSBackendOptions[Option, String]](table)

  override def compile(input: CompilerInput[ResourceIndicator, JSBackendOptions[Id, ResourceIndicator]]): ZManaged[ResourceAccess, ErrorList, CompilationOutputText] = {
    val context = new JSContext(input)
    val emitter = new JSEmitter[context.type](context, input.backendOptions.inject)

    context.module[JSBackendLoadService].mapM { module =>
      emitter.emitModule(module).map { jsModule =>
        createOutput(input.backendOptions.outputFile)(jsModule)
      }
    }.provideSomeLayer(JSBackendLoadService.uponResourceAccess)
  }

  private def createOutput(outputRes: ResourceIndicator)(jsModule: JSModule): CompilationOutputText = new CompilationOutputText {

    override def outputResource: ResourceIndicator = outputRes

    override val textStream: Source[Comp, String, Unit] =
      JSAst.writeModule(jsModule)

  }

}
