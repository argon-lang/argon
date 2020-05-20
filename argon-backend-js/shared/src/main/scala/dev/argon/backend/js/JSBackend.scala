package dev.argon.backend.js

import cats.{Applicative, Monad}
import cats.arrow.FunctionK
import dev.argon.compiler._
import cats.implicits._
import cats.data.NonEmptyList
import dev.argon.backend._
import dev.argon.compiler.options.SingleFile
import dev.argon.compiler.core.Context
import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader, SourceParser}
import dev.argon.compiler.options.{CompilerInput, FileList, OptionInfo, OptionsConverter, OptionsConverterFunction, OptionsHandler, OptionsLoader, SingleFile}
import dev.argon.loaders.armodule.ArgonModuleLoader
import dev.argon.loaders.armodule.ArgonModuleLoader.PayloadLoader
import dev.argon.stream.builder.Source
import zio._
import zio.stream._
import zio.interop.catz.core._
import shapeless._
import dev.argon.util.ValueCache
import dev.argon.compiler.options.CodecSelector.Instances._


final case class JSBackend(private val moduleExtractor: JSModuleExtractor) extends Backend {

  override type BackendOptions[F[_], I] = JSBackendOptions[F, I]
  override type BackendOutputOptions[F[_], I] = JSOutputOptions[F, I]
  override type TCompilationOutput = CompilationOutputText[BackendOutputOptionsId]

  override val id: String = "js"
  override val name: String = "JavaScript"


  override val backendOptions: OptionsHandler[JSBackendOptions] = new OptionsHandler[JSBackendOptions] {
    override def info[I]: JSBackendOptions[OptionInfo[*, I], I] =
      JSBackendOptions[OptionInfo[*, I], I](
        extern = OptionInfo(
          name = "extern",
          description = "JS module defining extern function implementations",
          defaultValue = new FileList[I](List.empty),
        ),
        inject = JSInjectCode[OptionInfo[*, I], I](
          before = OptionInfo(
            name = "inject.before",
            description = "JS code injected at the top of the module",
            defaultValue = None,
          ),
          after = OptionInfo(
            name = "inject.after",
            description = "JS code injected at the bottom of the module",
            defaultValue = None,
          ),
        )
      )


    override def converter[I]: OptionsConverter[JSBackendOptions[*[_], I]] =
      new OptionsConverter[JSBackendOptions[*[_], I]] {

        override def convert[A[_], B[_], C[_], F[_] : Applicative](optionsA: JSBackendOptions[A, I], optionsB: JSBackendOptions[B, I])(f: OptionsConverterFunction[A, B, C, F]): F[JSBackendOptions[C, I]] =
          Applicative[F].map3(
            f(optionsA.extern, optionsB.extern),
            f(optionsA.inject.before, optionsB.inject.before),
            f(optionsA.inject.after, optionsB.inject.after),
          ) {
            (convExtern, convInjectBefore, convInjectAfter) =>
              JSBackendOptions(
                extern = convExtern,
                inject = JSInjectCode(
                  before = convInjectBefore,
                  after = convInjectAfter,
                ),
              )
          }
      }

    override def optionsLoader[IOld, I]: OptionsLoader[JSBackendOptions[Id, IOld], JSBackendOptions[Id, I], IOld, I] = {
      import dev.argon.compiler.options.OptionsLoader.Implicits._
      OptionsLoader.apply
    }
  }


  override val outputOptions: OptionsHandler[JSOutputOptions] = new OptionsHandler[JSOutputOptions] {
    override def info[I]: JSOutputOptions[OptionInfo[*, I], I] =
      JSOutputOptions[OptionInfo[*, I], I](
        outputFile = OptionInfo(
          name = "outputFile",
          description = "The compiled JS file",
        )
      )

    override def converter[I]: OptionsConverter[JSOutputOptions[*[_], I]] =
      new OptionsConverter[JSOutputOptions[*[_], I]] {

        override def convert[A[_], B[_], C[_], F[_] : Applicative](optionsA: JSOutputOptions[A, I], optionsB: JSOutputOptions[B, I])(f: OptionsConverterFunction[A, B, C, F]): F[JSOutputOptions[C, I]] =
          for {
            convOutputFile <- f(optionsA.outputFile, optionsB.outputFile)
          } yield JSOutputOptions(
            outputFile = convOutputFile
          )
      }

    override def optionsLoader[IOld, I]: OptionsLoader[JSOutputOptions[Id, IOld], JSOutputOptions[Id, I], IOld, I] = {
      import dev.argon.compiler.options.OptionsLoader.Implicits._
      OptionsLoader.apply
    }
  }

  override def testOutputOptions[I](dummyFile: I): JSOutputOptions[Id, I] =
    JSOutputOptions[Id, I](outputFile = new SingleFile[I](dummyFile))

  override def compile[I <: ResourceIndicator: Tag](input: CompilerInput[I, JSBackendOptions[Id, I]]): ZManaged[ResourceReader[I] with SourceParser, ErrorList, TCompilationOutput] = for {
    externCache <- ZManaged.fromEffect(ValueCache.make[ErrorList, Map[String, ResolvedExtern]])
    resReader <- ZManaged.access[ResourceReader[I]](_.get)
    context: JSContext with Context.WithRes[I] = new JSContext {
      override type ResIndicator = I
      override val resIndicatorTag: zio.Tag[I] = implicitly
      override protected val compilerInput: CompilerInput[I, JSBackendOptions[Id, I]] = input

      override def extractJSModuleFunctions(jsModule: String): IO[Throwable, Map[String, String]] =
        moduleExtractor.exportedFunctions(jsModule)

      override protected val externFunctionsCache: ValueCache[ErrorList, Map[String, ResolvedExtern]] = externCache
      override protected val resourceReader: ResourceReader.Service[I] = resReader
    }

    emitter <- ZManaged.fromEffect(JSEmitter.make(context)(input.backendOptions.inject))
    module <- context.module[JSContext with Context.WithRes[I]].provideSomeLayer[ResourceReader[I] with SourceParser](JSBackendLoadService.forResourceReader[I, JSContext with Context.WithRes[I]])
    jsModule <- ZManaged.fromEffect(emitter.emitModule(module))

  } yield createOutput(jsModule)

  private def createOutput(jsModule: JSModule): CompilationOutputText[BackendOutputOptionsId] = new CompilationOutputText[BackendOutputOptionsId] {

    override def outputResource[I](options: BackendOutputOptionsId[I]): I = options.outputFile.file

    override val textStream: Stream[ErrorList, String] =
      JSAst.writeModule(jsModule).toZStream

  }

}
