package dev.argon.backend.generic

import cats.{Applicative, Monad}
import cats.implicits._
import dev.argon.compiler._
import dev.argon.stream._
import scalapb.GeneratedMessage
import cats.instances._
import cats.data.NonEmptyList
import dev.argon.armodule.emitter.{ModuleEmitOptions, ModuleEmitter}
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


object GenericBackend extends Backend {

  override type BackendOptions[F[_], I] = GenericBackendOptions[F, I]
  override type BackendOutputOptions[F[_], I] = GenericOutputOptions[F, I]
  override type TCompilationOutput = CompilationOutput[BackendOutputOptionsId]


  override val id: String = "generic"
  override val name: String = "Generic"

  override val backendOptions: OptionsHandler[GenericBackendOptions] = new OptionsHandler[GenericBackendOptions] {
    override def info[I]: GenericBackendOptions[OptionInfo[*, I], I] =
      GenericBackendOptions()

    override def converter[I]: OptionsConverter[GenericBackendOptions[*[_], I]] =
      new OptionsConverter[GenericBackendOptions[*[_], I]] {
        override def convert[A[_], B[_], C[_], F[_] : Applicative](optionsA: GenericBackendOptions[A, I], optionsB: GenericBackendOptions[B, I])(f: OptionsConverterFunction[A, B, C, F]): F[GenericBackendOptions[C, I]] =
          Applicative[F].pure(GenericBackendOptions())
      }

    override def optionsLoader[IOld, I]: OptionsLoader[GenericBackendOptions[Id, IOld], GenericBackendOptions[Id, I], IOld, I] = {
      import dev.argon.compiler.options.OptionsLoader.Implicits._
      OptionsLoader.apply
    }

  }

  override val outputOptions: OptionsHandler[GenericOutputOptions] = new OptionsHandler[GenericOutputOptions] {
    override def info[I]: GenericOutputOptions[OptionInfo[*, I], I] =
      GenericOutputOptions[OptionInfo[*, I], I](
        referenceModule = OptionInfo(
          name = "referenceModule",
          description = "The reference module that will contain the interface of the compiled module",
          defaultValue = None
        ),
        declarationModule = OptionInfo(
          name = "declarationModule",
          description = "The declaration module that will contain the serialized compiled module",
          defaultValue = None
        ),
      )

    override def converter[I]: OptionsConverter[GenericOutputOptions[*[_], I]] =
      new OptionsConverter[GenericOutputOptions[*[_], I]] {
        override def convert[A[_], B[_], C[_], F[_] : Applicative](optionsA: GenericOutputOptions[A, I], optionsB: GenericOutputOptions[B, I])(f: OptionsConverterFunction[A, B, C, F]): F[GenericOutputOptions[C, I]] =
          Applicative[F].map2(
            f(optionsA.referenceModule, optionsB.referenceModule),
            f(optionsA.declarationModule, optionsB.declarationModule),
          ) { (convRefModule, convDeclModule) =>
            GenericOutputOptions(
              referenceModule = convRefModule,
              declarationModule = convDeclModule,
            )
          }
      }

    override def optionsLoader[IOld, I]: OptionsLoader[GenericOutputOptions[Id, IOld], GenericOutputOptions[Id, I], IOld, I] = {
      import dev.argon.compiler.options.OptionsLoader.Implicits._
      OptionsLoader.apply
    }
  }

  override def testOutputOptions[I](dummyFile: I): GenericOutputOptions[Id, I] =
    GenericOutputOptions[Id, I](
      referenceModule = Some(new SingleFile(dummyFile)),
      declarationModule = Some(new SingleFile(dummyFile)),
    )

  override def compile[I <: ResourceIndicator : Tag](input: CompilerInput[I, GenericBackendOptions[Id, I]]): ZManaged[ResourceReader[I] with SourceParser, CompError, TCompilationOutput] = {
    val ctx = ModuleContext(input)

    val moduleEmitter = new ModuleEmitterImpl {
      override val context: ctx.type = ctx
    }

    ctx.module[ModuleContext with Context.WithRes[I]].map { module =>
      createOutput(
        refModuleGen = ModuleEmitter.emitModule(moduleEmitter)(ModuleEmitOptions(ModuleEmitOptions.ReferenceModule))(module),
        declModuleGen = ModuleEmitter.emitModule(moduleEmitter)(ModuleEmitOptions(ModuleEmitOptions.DeclarationModule))(module),
      )
    }
      .provideSomeLayer[ResourceReader[I] with SourceParser](GenericBackendLoadService.forResourceReader[I, ModuleContext with Context.WithRes[I]])
  }

  private def createOutput(refModuleGen: Stream[CompError, ModuleEmitter.StreamElem], declModuleGen: Stream[CompError, ModuleEmitter.StreamElem]): CompilationOutput[BackendOutputOptionsId] =
    new CompilationOutput[BackendOutputOptionsId] {

      private def outputModule[I <: ResourceIndicator : Tag](outputFile: Option[SingleFile[I]])(moduleGen: Stream[CompError, ModuleEmitter.StreamElem]): RComp[ResourceWriter[I], Unit] =
        ZIO.foreach(outputFile) { outputFile =>
          ZIO.accessM[ResourceWriter[I]] { env =>
            val res = env.get

            val zipEntries = moduleGen.map { case (path, message) =>
              ZipEntryInfo(path, res.serializeProtocolBuffer(message))
            }

            val zipData = res.zipFromEntries(zipEntries)

            res.writeToResource(outputFile.file)(zipData)
          }
        }.unit

      override def write[I <: ResourceIndicator : Tag](options: BackendOutputOptionsId[I]): RComp[ResourceWriter[I], Unit] =
        outputModule(options.referenceModule)(refModuleGen) *>
          outputModule(options.declarationModule)(declModuleGen)

    }

}
