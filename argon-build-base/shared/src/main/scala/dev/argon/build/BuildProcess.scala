package dev.argon.build

import dev.argon.compiler.{options, _}
import dev.argon.parser.{SourceAST, SyntaxError, SyntaxErrorData}
import cats._
import cats.arrow.FunctionK
import cats.implicits._
import cats.data.{NonEmptyList, NonEmptyVector}
import dev.argon.armodule.loader.{AggregateModuleLoader, ArgonModuleLoader}
import dev.argon.armodule.emitter.{ModuleEmitOptions, ModuleSerializer, ZipModuleWriter}
import dev.argon.backend.Backend
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.core.{ArModule, Context, ModuleId}
import dev.argon.compiler.loaders.{ModuleLinker, UnlinkedModule}
import dev.argon.compiler.loaders.source.UnlinkedSourceModule
import dev.argon.compiler.options.{CompilerInput, CompilerOptionID, CompilerOutput, GeneralOutputOptionID, GeneralOutputOptions}
import dev.argon.options.{FileList, OptionID, Options, OptionsHandler, SingleFile}
import dev.argon.compiler.output.{ArgonModuleSerialized, BuildArtifact, ModuleBuildArtifact}
import dev.argon.io.{ZipCreator, ZipEntryInfo}
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.parser.impl.{ArgonSourceParser, ParseHandler}
import dev.argon.util.{FileID, FileSpec, MaybeBlocking, ProtoBufCodecs}
import dev.argon.stream._
import dev.argon.stream.builder.Source
import dev.argon.util.AnyExtensions._
import zio._
import zio.interop.catz.core._
import zio.stream._
import zio.NeedsEnv.needsEnv

object BuildProcess {

  def parse(inputFiles: FileList): RCompStream[FileIO, SourceAST] = {
    ZStream.fromIterable(
      inputFiles.files.zipWithIndex.map { case (file, index) => FileSpec(FileID(index), file) }
    )
      .flatMap { inputFile =>
        ArgonSourceParser.parse(inputFile)(
          ZStream.accessStream[FileIO](_.get.readText(inputFile.name))
            .catchAll(Compilation.unwrapThrowableStream)
        )
      }
  }

  def loadReferences(backend: Backend)(input: CompilerInput[backend.BackendOptionID]): RCompManaged[ZipRead with MaybeBlocking, Vector[UnlinkedModule[Context.Aux[backend.type], ReferencePayloadSpecifier]]] = for {
    arModuleLoader <- ZManaged.fromEffect(ArgonModuleLoader.make[Context.Aux[backend.type]])
    moduleLoaders = backend.moduleLoaders(input.backendOptions)
    aggLoader = new AggregateModuleLoader(arModuleLoader +: moduleLoaders.toVector)

    unlinkedModules <- Managed.foreach(input.options.get(CompilerOptionID.References).files) { referenceFile =>
      aggLoader.loadResource(referenceFile).mapM {
        case Some(value) => IO.succeed(value)
        case None => Compilation.forErrors(DiagnosticError.CouldNotFindCompatibleModuleLoader(DiagnosticSource.LinkPhase()))
      }
    }
  } yield unlinkedModules.toVector

  def createModule(options: Options[Id, CompilerOptionID])(context: Context)(references: Vector[UnlinkedModule[context.type, ReferencePayloadSpecifier]])(source: CompStream[SourceAST]): CompManaged[ArModule[context.type, DeclarationPayloadSpecifier]] =
    ModuleLinker.loadReferencedModules(context)(references).flatMap { loadedRefs =>
      new UnlinkedSourceModule[context.type](ModuleId(options.get(CompilerOptionID.ModuleName)), references.map(_.descriptor), source)
        .load(context)(loadedRefs)
    }


  private def emit[OutputOptionID <: OptionID { type ElementType <: BuildArtifact }](emittedFiles: Ref[Set[String]])(handler: OptionsHandler[OutputOptionID, Lambda[X => SingleFile]])(fileNames: Options[Lambda[X => Option[SingleFile]], OutputOptionID], data: Options[Id, OutputOptionID]): RComp[FileIO, Unit] =
    handler.combineRepr[
        Lambda[AX => Option[SingleFile]],
        Id,
        Lambda[CX => Unit],
        RComp[FileIO, *]
      ](handler.optionsToRepr(fileNames), handler.optionsToRepr(data))(
        new OptionsHandler.CombineFunction[
          OutputOptionID,
          Lambda[AX => Option[SingleFile]],
          Id,
          Lambda[CX => Unit],
          RComp[FileIO, *]
        ] {
          override def apply(id: OutputOptionID)(ax: Option[SingleFile], bx: Id[id.ElementType]): RComp[FileIO, Unit] =
            ZIO.foreach_(ax.toList) { outputFile =>
              emittedFiles.update { _ + outputFile.file } *>
              ZIO.accessM[FileIO](_.get.writeToFile(outputFile.file)(bx.asStream))
                .catchAll(Compilation.unwrapThrowable)
            }
        }
      ).unit



  def compile
  (
    backend: Backend
  )(
    compilerInput: CompilerInput[backend.BackendOptionID],
    compilerOutput: CompilerOutput[backend.OutputOptionID],
  )
  : RComp[FileIO with ZipRead with MaybeBlocking, Unit] =
    ZIO.accessM[FileIO with ZipRead with MaybeBlocking] { env =>
      buildResult(backend)(
        compilerInput,
        parse(compilerInput.options.get(CompilerOptionID.InputFiles)).provide(env)
      ).use { buildResult =>
        for {
          emittedFiles <- Ref.make(Set.empty[String])
          _ <- emit(emittedFiles)(GeneralOutputOptions.handler)(compilerOutput.generalOutput, buildResult.generalOutput)
          _ <- emit(emittedFiles)(backend.outputOptions)(compilerOutput.backendOutput, buildResult.backendOutput)
        } yield ()
      }
    }

  def buildResult
  (
    backend: Backend
  )(
    compilerInput: CompilerInput[backend.BackendOptionID],
    sourceCodeStream: CompStream[SourceAST]
  )
  : RCompManaged[FileIO with ZipRead with MaybeBlocking, BuildResult.Aux[backend.type]] = for {
    env <- ZManaged.environment[MaybeBlocking]
    ctx <- ZManaged.fromEffect(Context.make(backend)(compilerInput))

    backend2: backend.type = backend

    references <- loadReferences(backend)(compilerInput)
    module <- createModule(compilerInput.options)(ctx)(references)(sourceCodeStream)
  } yield new BuildResult {
    override val backend: backend2.type = backend2
    override val context: ctx.type = ctx

    private final class ModuleBuildArtifactImpl(moduleOptions: ModuleEmitOptions) extends ModuleBuildArtifact {


      override def serialized: UIO[ArgonModuleSerialized] =
        ModuleSerializer.serialize(context)(moduleOptions)(module)

      override def asStream: CompStream[Byte] =
        ZStream.unwrap(serialized.map(ZipModuleWriter.writeModule)).provide(env)

    }

    override val generalOutput: Options[Id, GeneralOutputOptionID] =
      Options.fromFunction(new Options.OptionValueFunction[Id, GeneralOutputOptionID] {
        override def apply[E](id: GeneralOutputOptionID { type ElementType = E }): Id[E] = id match {
          case GeneralOutputOptionID.DeclarationModule =>
            new ModuleBuildArtifactImpl(ModuleEmitOptions(moduleType = ModuleEmitOptions.DeclarationModule))

          case GeneralOutputOptionID.InterfaceModule =>
            new ModuleBuildArtifactImpl(ModuleEmitOptions(moduleType = ModuleEmitOptions.ReferenceModule))

        }
      })

    override val backendOutput: Options[Id, backend.OutputOptionID] =
      backend.emitModule(compilerInput.backendOptions)(ctx)(module)
  }



}
