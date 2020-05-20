package dev.argon.backend.module

import cats.{Applicative, Monad}
import cats.implicits._
import dev.argon.compiler._
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.compiler.options.SingleFile
import dev.argon.backend.{Backend, CompilationOutput, ResourceAccess, ResourceWriter}
import dev.argon.compiler.core.Context
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader, SourceParser}
import dev.argon.compiler.options.{CompilerInput, OptionInfo, OptionsConverter, OptionsConverterFunction, OptionsHandler, OptionsLoader, SingleFile}
import dev.argon.io.ZipEntryInfo
import dev.argon.stream.builder.Source
import shapeless._
import zio._
import zio.stream._
import dev.argon.compiler.options.CodecSelector.Instances._


object ArModuleBackend extends Backend {

  override type BackendOptions[F[_], I] = ModuleBackendOptions[F, I]
  override type BackendOutputOptions[F[_], I] = ModuleOutputOptions[F, I]
  override type TCompilationOutput = CompilationOutput[BackendOutputOptionsId]


  override val id: String = "argon-module"
  override val name: String = "Argon Module"

  override val backendOptions: OptionsHandler[ModuleBackendOptions] = new OptionsHandler[ModuleBackendOptions] {
    override def info[I]: ModuleBackendOptions[OptionInfo[*, I], I] =
      ModuleBackendOptions()

    override def converter[I]: OptionsConverter[ModuleBackendOptions[*[_], I]] =
      new OptionsConverter[ModuleBackendOptions[*[_], I]] {
        override def convert[A[_], B[_], C[_], F[_] : Applicative](optionsA: ModuleBackendOptions[A, I], optionsB: ModuleBackendOptions[B, I])(f: OptionsConverterFunction[A, B, C, F]): F[ModuleBackendOptions[C, I]] =
          Applicative[F].pure(ModuleBackendOptions())
      }

    override def optionsLoader[IOld, I]: OptionsLoader[ModuleBackendOptions[Id, IOld], ModuleBackendOptions[Id, I], IOld, I] = {
      import dev.argon.compiler.options.OptionsLoader.Implicits._
      OptionsLoader.apply
    }

  }

  override val outputOptions: OptionsHandler[ModuleOutputOptions] = new OptionsHandler[ModuleOutputOptions] {
    override def info[I]: ModuleOutputOptions[OptionInfo[*, I], I] =
      ModuleOutputOptions[OptionInfo[*, I], I](
        referenceModule = OptionInfo(
          name = "referenceModule",
          description = "The reference module that will contain the interface of the compiled module"
        )
      )

    override def converter[I]: OptionsConverter[ModuleOutputOptions[*[_], I]] =
      new OptionsConverter[ModuleOutputOptions[*[_], I]] {
        override def convert[A[_], B[_], C[_], F[_] : Applicative](optionsA: ModuleOutputOptions[A, I], optionsB: ModuleOutputOptions[B, I])(f: OptionsConverterFunction[A, B, C, F]): F[ModuleOutputOptions[C, I]] =
          for {
            convRefModule <- f(optionsA.referenceModule, optionsB.referenceModule)
          } yield ModuleOutputOptions(
            referenceModule = convRefModule
          )
      }

    override def optionsLoader[IOld, I]: OptionsLoader[ModuleOutputOptions[Id, IOld], ModuleOutputOptions[Id, I], IOld, I] = {
      import dev.argon.compiler.options.OptionsLoader.Implicits._
      OptionsLoader.apply
    }
  }

  override def testOutputOptions[I](dummyFile: I): ModuleOutputOptions[Id, I] =
    ModuleOutputOptions[Id, I](
      referenceModule = new SingleFile(dummyFile)
    )

  override def compile[I <: ResourceIndicator : Tag](input: CompilerInput[I, ModuleBackendOptions[Id, I]]): ZManaged[ResourceReader[I] with SourceParser, ErrorList, TCompilationOutput] = {
    val context = ModuleContext(input)

    context.module[ModuleContext with Context.WithRes[I]].map { module =>
      createOutput(ModuleEmitter.emitModule(context)(module))
    }
      .provideSomeLayer[ResourceReader[I] with SourceParser](ModuleBackendLoadService.forResourceReader[I, ModuleContext with Context.WithRes[I]])
  }

  private def createOutput(moduleGen: Stream[ErrorList, ModuleEmitter.StreamElem]): CompilationOutput[BackendOutputOptionsId] =
    new CompilationOutput[BackendOutputOptionsId] {

      override def write[I <: ResourceIndicator : Tag](options: BackendOutputOptionsId[I]): RComp[ResourceWriter[I], Unit] =
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
