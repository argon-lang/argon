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

object ModuleLinker {

  def loadReferencedModules[TContext <: Context]
  (context: TContext)
  (unlinkedRefs: Vector[UnlinkedModule[context.type, ReferencePayloadSpecifier]])
  : CompManaged[Vector[ArModule[context.type, ReferencePayloadSpecifier]]] = {

    type ModInfo = UnlinkedModule[context.type, ReferencePayloadSpecifier]


    type PayloadResult = ArModule[context.type, ReferencePayloadSpecifier]

    val dependencyTreeOps
    : DependencyTreeOperations[CompManaged, ModInfo, ModuleId, PayloadResult, Either[CompilationError, *]] =
      new DependencyTreeOperations[CompManaged, ModInfo, ModuleId, PayloadResult, Either[CompilationError, *]] {
        override def getItemKey(item: ModInfo): ModuleId = item.descriptor

        override def getItemDependencies(item: ModInfo): Vector[ModuleId] =
          item.referencedModules

        override def loadItem(item: ModInfo, dependencies: Vector[PayloadResult]): CompManaged[PayloadResult] =
          item.load(context)(dependencies)

        override def circularReferenceHandler(item: ModInfo): Either[CompilationError, Nothing] =
          Left(DiagnosticError.CircularDependencyLoadingModule(DiagnosticSource.ReferencedModule(item.descriptor)))

        override def missingDependencyHandler(item: ModInfo, missingDepKey: ModuleId): Either[CompilationError, Nothing] =
          Left(DiagnosticError.ModuleDependencyNotFound(missingDepKey, DiagnosticSource.ReferencedModule(item.descriptor)))

      }


    loadDependencies[CompManaged, ModInfo, ModuleId, PayloadResult, Either[CompilationError, *]](dependencyTreeOps)(unlinkedRefs)
      .flatMap { loadedModules =>
        ZManaged.foreach(loadedModules)(ZManaged.fromEither(_))
      }
  }
}

