package dev.argon.compiler.loaders

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.DependencyTree._
import cats._
import cats.data.NonEmptyList
import cats.implicits._
import zio._
import zio.interop.catz._

import scala.collection.immutable._

object ModuleLoader {

  def loadReferencedModules[TContext <: Context with Singleton, TLoad <: ModuleLoad.Service[TContext] : Tagged]
  (context: TContext)
  (refFiles: Vector[ResourceIndicator])
  : ZManaged[Has[TLoad], ErrorList, Vector[ArModule[context.type, ReferencePayloadSpecifier]]] = {

    type ModInfo = ModuleMetadata[TContext]

    def findWorkingLoader(id: ResourceIndicator): ZManaged[Has[TLoad], ErrorList, ModInfo] =
      ZManaged.accessManaged[Has[TLoad]](_.get[TLoad].loadResource(id))
        .map { _.toRight { NonEmptyList.of(CompilationError.CouldNotFindCompatibleModuleLoader(CompilationMessageSource.ResourceIdentifier(id))) } }
        .absolve


    type PayloadResult = ArModule[context.type, ReferencePayloadSpecifier]

    val dependencyTreeOps
    : DependencyTreeOperations[Comp, ModInfo, ModuleDescriptor, PayloadResult, Either[CompilationError, ?]] =
      new DependencyTreeOperations[Comp, ModInfo, ModuleDescriptor, PayloadResult, Either[CompilationError, ?]] {
        override def getItemKey(item: ModInfo): ModuleDescriptor = item.descriptor

        override def getItemDependencies(item: ModInfo): Vector[ModuleDescriptor] =
          item.referencedModules

        override def loadItem(item: ModInfo, dependencies: Vector[PayloadResult]): Comp[PayloadResult] =
          item.loadReference(context)(dependencies)

        override def circularReferenceHandler(item: ModInfo): Either[CompilationError, Nothing] =
          Left(CompilationError.CircularDependencyLoadingModule(CompilationMessageSource.ReferencedModule(item.descriptor)))

        override def missingDependencyHandler(item: ModInfo, missingDepKey: ModuleDescriptor): Either[CompilationError, Nothing] =
          Left(CompilationError.ModuleDependencyNotFound(missingDepKey, CompilationMessageSource.ReferencedModule(item.descriptor)))

      }

    def loadModuleRefFromData
    (refDataPairs: Vector[ModInfo])
    : RComp[Has[TLoad], Vector[Either[CompilationError, PayloadResult]]] =
      loadDependencies[Comp, ModInfo, ModuleDescriptor, PayloadResult, Either[CompilationError, *]](dependencyTreeOps)(refDataPairs)




    def impl(refFiles: Vector[ResourceIndicator], loadedFiles: Vector[ModInfo]): ZManaged[Has[TLoad], ErrorList, Vector[ArModule[context.type, ReferencePayloadSpecifier]]] =
      refFiles match {
        case id +: tail =>
          findWorkingLoader(id).flatMap { loadedFile => impl(tail, loadedFiles :+ loadedFile) }

        case Vector() =>
          ZManaged.fromEffect(
            loadModuleRefFromData(loadedFiles)
              .map { moduleResults =>
                moduleResults.sequence.left.map(NonEmptyList.of(_))
              }
              .absolve
          )
      }

    impl(refFiles, Vector())
  }
}

