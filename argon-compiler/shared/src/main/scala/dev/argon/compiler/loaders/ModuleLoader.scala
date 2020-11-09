package dev.argon.compiler.loaders

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.DependencyTree._
import cats._
import cats.data.NonEmptyList
import cats.implicits._
import zio._
import zio.interop.catz.core._

import scala.collection.immutable._

object ModuleLoader {

  def loadReferencedModules[I <: ResourceIndicator: Tag, TContext <: Context.WithRes[I]: Tag]
  (context: TContext)
  (refFiles: Vector[I])
  : ZManaged[ModuleLoad[I, TContext], CompilationError, Vector[ArModule[context.type, ReferencePayloadSpecifier]]] = {

    type ModInfo = ModuleMetadata[TContext]

    def findWorkingLoader(id: I): ZManaged[ModuleLoad[I, TContext], CompilationError, ModInfo] =
      ZManaged.accessManaged[ModuleLoad[I, TContext]](_.get.loadResource(id))
        .map { _.toRight { DiagnosticError.CouldNotFindCompatibleModuleLoader(DiagnosticSource.ResourceIdentifier(id)) } }
        .absolve


    type PayloadResult = ArModule[context.type, ReferencePayloadSpecifier]

    val dependencyTreeOps
    : DependencyTreeOperations[CompManaged, ModInfo, ModuleId, PayloadResult, Either[CompilationError, ?]] =
      new DependencyTreeOperations[CompManaged, ModInfo, ModuleId, PayloadResult, Either[CompilationError, ?]] {
        override def getItemKey(item: ModInfo): ModuleId = item.descriptor

        override def getItemDependencies(item: ModInfo): Vector[ModuleId] =
          item.referencedModules

        override def loadItem(item: ModInfo, dependencies: Vector[PayloadResult]): CompManaged[PayloadResult] =
          item.loadReference(context)(dependencies)

        override def circularReferenceHandler(item: ModInfo): Either[CompilationError, Nothing] =
          Left(DiagnosticError.CircularDependencyLoadingModule(DiagnosticSource.ReferencedModule(item.descriptor)))

        override def missingDependencyHandler(item: ModInfo, missingDepKey: ModuleId): Either[CompilationError, Nothing] =
          Left(DiagnosticError.ModuleDependencyNotFound(missingDepKey, DiagnosticSource.ReferencedModule(item.descriptor)))

      }

    def loadModuleRefFromData
    (refDataPairs: Vector[ModInfo])
    : RCompManaged[ModuleLoad[I, TContext], Vector[Either[CompilationError, PayloadResult]]] =
      loadDependencies[CompManaged, ModInfo, ModuleId, PayloadResult, Either[CompilationError, *]](dependencyTreeOps)(refDataPairs)




    def impl(refFiles: Vector[I], loadedFiles: Vector[ModInfo]): ZManaged[ModuleLoad[I, TContext], CompilationError, Vector[ArModule[context.type, ReferencePayloadSpecifier]]] =
      refFiles match {
        case id +: tail =>
          findWorkingLoader(id).flatMap { loadedFile => impl(tail, loadedFiles :+ loadedFile) }

        case Vector() =>
          loadModuleRefFromData(loadedFiles)
            .map { moduleResults =>
              moduleResults.sequence
            }
            .absolve
      }

    impl(refFiles, Vector())
  }
}

