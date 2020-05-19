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
import dev.argon.project.{FileList, ProjectLoader, SingleFile}
import dev.argon.stream.builder.Source
import zio._
import zio.stream._
import zio.interop.catz.core._
import toml.Codecs._
import shapeless._
import dev.argon.project.ExtraTomlCodecs._
import dev.argon.util.ValueCache
import toml.Parse.{Address, Message}
import toml.Value


final case class JSBackend(private val moduleExtractor: JSModuleExtractor) extends Backend {

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
      extern = options.extern.getOrElse(FileList[I](List.empty)),
      inject = options.inject.getOrElse(JSInjectCode[Option, I](before = None, after = None)) match {
        case JSInjectCode(beforeOpt, afterOpt) =>
          JSInjectCode[Id, I](
            before = beforeOpt.flatten,
            after = afterOpt.flatten
          )
      }
    )

  override def backendOptionsProjectLoader[IOld, I]: ProjectLoader[BackendOptionsId[IOld], BackendOptionsId[I], IOld, I] = {
    import dev.argon.project.ProjectLoader.Implicits._

    implicit val injectCode: ProjectLoader[JSInjectCode[Id, IOld], JSInjectCode[Id, I], IOld, I] = productGenericLoader

    ProjectLoader[BackendOptionsId[IOld], BackendOptionsId[I], IOld, I]
  }

  override def parseBackendOptions(table: toml.Value.Tbl): Either[toml.Parse.Error, JSBackendOptions[Option, String]] =
    toml.Toml.parseAs[JSBackendOptions[Option, String]](table)

  override def emptyOutputOptions[I]: JSOutputOptions[Option, I] =
    JSOutputOptions[Option, I](None)

  override def inferOutputOptions(compilerOptions: CompilerOptions[Id], options: JSOutputOptions[Option, String]): BackendOutputOptionsId[String] =
    JSOutputOptions[Id, String](
      outputFile = options.outputFile.getOrElse(SingleFile(compilerOptions.moduleName + ".js")),
    )

  override def outputOptionsProjectLoader[IOld, I]: ProjectLoader[BackendOutputOptionsId[IOld], BackendOutputOptionsId[I], IOld, I] = {
    import dev.argon.project.ProjectLoader.Implicits._
    ProjectLoader.apply
  }

  override def parseOutputOptions(table: Value.Tbl): Either[(Address, Message), JSOutputOptions[Option, String]] =
    toml.Toml.parseAs[JSOutputOptions[Option, String]](table)

  override def compile[I <: ResourceIndicator: Tagged](input: CompilerInput[I, JSBackendOptions[Id, I]]): ZManaged[ResourceReader[I], ErrorList, TCompilationOutput] = for {
    externCache <- ZManaged.fromEffect(ValueCache.make[ErrorList, Map[String, ResolvedExtern]])
    resReader <- ZManaged.access[ResourceReader[I]](_.get)
    context: JSContext with Context.WithRes[I] = new JSContext {
      override type ResIndicator = I
      override val resIndicatorTag: zio.Tagged[I] = implicitly
      override protected val compilerInput: CompilerInput[I, JSBackendOptions[Id, I]] = input

      override def extractJSModuleFunctions(jsModule: String): IO[Throwable, Map[String, String]] =
        moduleExtractor.exportedFunctions(jsModule)

      override protected val externFunctionsCache: ValueCache[ErrorList, Map[String, ResolvedExtern]] = externCache
      override protected val resourceReader: ResourceReader.Service[I] = resReader
    }

    emitter <- ZManaged.fromEffect(JSEmitter.make(context)(input.backendOptions.inject))
    module <- context.module[JSContext with Context.WithRes[I]].provideLayer(JSBackendLoadService.forResourceReader[I, JSContext with Context.WithRes[I]])
    jsModule <- ZManaged.fromEffect(emitter.emitModule(module))

  } yield createOutput(jsModule)

  private def createOutput(jsModule: JSModule): CompilationOutputText[BackendOutputOptionsId] = new CompilationOutputText[BackendOutputOptionsId] {

    override def outputResource[I](options: BackendOutputOptionsId[I]): I = options.outputFile.file

    override val textStream: Stream[ErrorList, String] =
      JSAst.writeModule(jsModule).toZStream

  }

}
