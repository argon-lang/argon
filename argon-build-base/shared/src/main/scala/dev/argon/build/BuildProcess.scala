package dev.argon.build

import dev.argon.compiler._
import dev.argon.parser.SourceAST
import cats._
import cats.implicits._
import dev.argon.armodule.loader.{AggregateModuleLoader, ArgonModuleDeserializer, ArgonModuleLoader}
import dev.argon.armodule.emitter.{ModuleEmitOptions, ModuleSerializer, ZipModuleWriter}
import dev.argon.backend.Backend
import dev.argon.backend.Backend.{AsFile, AsFileOption, AsUnit}
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.core.{ArModule, Context, ModuleId}
import dev.argon.compiler.loaders.{ModuleLinker, UnlinkedModule}
import dev.argon.compiler.loaders.source.UnlinkedSourceModule
import dev.argon.compiler.options.{CompilerInput, CompilerOptionID, CompilerOutput, GeneralOutputOptionID, GeneralOutputOptions}
import dev.argon.options.{FileList, OptionID, Options, OptionsHandler, SingleFile, TypedOptionID}
import dev.argon.compiler.output.{ArgonModuleSerialized, BuildArtifact, ModuleBuildArtifact}
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.parser.impl.ArgonSourceParser
import dev.argon.util.{FileID, FileSpec, MaybeBlocking}
import zio._
import zio.interop.catz.core._
import zio.stream._

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
    arModuleLoader <- ZManaged.fromEffect(ArgonModuleLoader.make)
    moduleLoaders = backend.moduleLoaders(input.backendOptions)
    aggLoader = new AggregateModuleLoader(arModuleLoader +: moduleLoaders.toVector)

    unlinkedModules <- Managed.foreach(input.options.get(CompilerOptionID.References).files) { referenceFile =>
      aggLoader.loadResource(referenceFile).mapM {
        case Some(value) => ArgonModuleDeserializer.deserialize(value)
        case None => Compilation.forErrors(DiagnosticError.CouldNotFindCompatibleModuleLoader(DiagnosticSource.LinkPhase()))
      }
    }
  } yield unlinkedModules.toVector

  def createModule(options: Options[Id, CompilerOptionID])(context: Context)(references: Vector[UnlinkedModule[context.type, ReferencePayloadSpecifier]])(source: CompStream[SourceAST]): CompManaged[ArModule[context.type, DeclarationPayloadSpecifier]] =
    ModuleLinker.loadReferencedModules(context)(references).flatMap { loadedRefs =>
      new UnlinkedSourceModule[context.type](ModuleId(options.get(CompilerOptionID.ModuleName)), references.map(_.descriptor), source)
        .load(context)(loadedRefs)
    }


  private def emit[OutputOptionID <: OptionID { type ElementType <: BuildArtifact; type Decoded[_] = SingleFile }](emittedFiles: Ref[Set[String]])(handler: OptionsHandler[OutputOptionID, AsFile])(fileNames: Options[AsFileOption, OutputOptionID], data: Options[Id, OutputOptionID]): RComp[FileIO, Unit] =
    handler.combineRepr[
        AsFileOption,
        Id,
        AsUnit,
        RComp[FileIO, *]
      ](handler.optionsToRepr(fileNames), handler.optionsToRepr(data))(
        new OptionsHandler.CombineFunction[
          OutputOptionID,
          AsFileOption,
          Id,
          AsUnit,
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
  : RCompManaged[FileIO with ZipRead with MaybeBlocking, BuildResult.Aux[backend.type]] =
    ZManaged.environment[MaybeBlocking].flatMap { env =>
      ZManaged.fromEffect(Context.make(backend)(compilerInput)).flatMap { ctx =>
        val backend2: backend.type = backend
        loadReferences(backend)(compilerInput).flatMap { references =>
          createModule(compilerInput.options)(ctx)(references)(sourceCodeStream).map { module =>
            new BuildResult {
              override val backend: backend2.type = backend2
              override val context: ctx.type = ctx

              private final class ModuleBuildArtifactImpl(moduleOptions: ModuleEmitOptions) extends ModuleBuildArtifact {


                override def serialized: UIO[ArgonModuleSerialized] =
                  ModuleSerializer.serialize(context)(moduleOptions)(module)

                override def asStream: CompStream[Byte] =
                  ZStream.unwrap(serialized.map(ZipModuleWriter.writeModule)).provide(env)(zio.NeedsEnv)

              }

              override val generalOutput: Options[Id, GeneralOutputOptionID] =
                Options.fromFunction(new Options.OptionValueFunction[Id, GeneralOutputOptionID] {
                  override def apply[E](id: GeneralOutputOptionID with TypedOptionID[E]): Id[E] = id match {
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
        }
      }
    }



}
