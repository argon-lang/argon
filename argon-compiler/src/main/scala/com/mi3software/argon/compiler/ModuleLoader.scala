package com.mi3software.argon.compiler

import java.io.File

import scalaz._
import Scalaz._
import com.mi3software.argon.Compilation
import com.mi3software.argon.util.MonadHelpers._
import com.mi3software.argon.util.DependencyTree._
import scalaz.effect.IO

import scala.collection.immutable._

trait ModuleLoader {

  type ModuleData

  def loadFile(file: File): IO[Option[ModuleData]]
  def dataDescriptor(data: ModuleData): Option[ModuleDescriptor]
  def dataReferencedModules(data: ModuleData): Vector[ModuleDescriptor]
  def loadModuleReference(context: Context)(data: ModuleData)(referencedModules: Vector[ArModule[context.type]]): context.Comp[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]]

}

object ModuleLoader {

  type PayloadResult[TContext <: Context] = ArModuleWithPayload[TContext, PayloadSpecifiers.ReferencePayloadSpecifier]
  type PayloadMap[TContext <: Context] = Map[ModuleDescriptor, PayloadResult[TContext]]

  private trait LoaderAndData {
    val loader: ModuleLoader
    val data: loader.ModuleData
    val descriptor: ModuleDescriptor
  }

  private object LoaderAndData {
    def apply(loader2: ModuleLoader)(data2: loader2.ModuleData)(desc: ModuleDescriptor): LoaderAndData =
      new LoaderAndData {
        override val loader: loader2.type = loader2
        override val data: loader.ModuleData = data2
        override val descriptor: ModuleDescriptor = desc
      }
  }

  private def findWorkingLoader(loaders: Vector[ModuleLoader])(file: File): IO[CompilationMessage \/ LoaderAndData] =
    findFirst(loaders) { load =>
      OptionT(load.loadFile(file))
        .flatMap { data =>
          val descOpt = load.dataDescriptor(data)
          OptionT(IO(descOpt))
            .map(LoaderAndData(load)(data))
        }
        .run
    }
    .map {
      _.toRightDisjunction(CompilationError.CouldNotFindCompatibleModuleLoader(CompilationMessageSource.ModuleFile(file)))
    }

  private def dependencyTreeOps
  (context: Context)
  : DependencyTreeOperations[context.Comp, LoaderAndData, ModuleDescriptor, PayloadResult[context.type], CompilationMessage \/ ?] =
    new DependencyTreeOperations[context.Comp, LoaderAndData, ModuleDescriptor, PayloadResult[context.type], CompilationMessage \/ ?] {
      override def getItemKey(item: LoaderAndData): ModuleDescriptor = item.descriptor

      override def getItemDependencies(item: LoaderAndData): Vector[ModuleDescriptor] =
        item.loader.dataReferencedModules(item.data)

      override def loadItem(item: LoaderAndData, dependencies: Vector[PayloadResult[context.type]]): context.Comp[PayloadResult[context.type]] =
        item.loader.loadModuleReference(context)(item.data)(dependencies)

      override def circularReferenceHandler(item: LoaderAndData): CompilationMessage \/ PayloadResult[context.type] =
        -\/(CompilationError.CircularDependencyLoadingModule(CompilationMessageSource.ReferencedModule(item.descriptor)))

      override def missingDependencyHandler(item: LoaderAndData, missingDepKey: ModuleDescriptor): CompilationMessage \/ PayloadResult[context.type] =
        -\/(CompilationError.ModuleDependencyNotFound(missingDepKey, CompilationMessageSource.ReferencedModule(item.descriptor)))

    }

  private def loadModuleRefFromData[TComp[+_] : Monad]
  (context: ContextComp[TComp])
  (refDataPairs: Vector[LoaderAndData])
  : TComp[Vector[CompilationMessage \/ PayloadResult[context.type]]] =
    loadDependencies[TComp, LoaderAndData, ModuleDescriptor, PayloadResult[context.type], CompilationMessage \/ ?](dependencyTreeOps(context))(refDataPairs)



  private def loadReferencedModulesImpl[TComp[+_] : Monad : Compilation]
  (context: ContextComp[TComp])
  (refFiles: Vector[File])
  : IO[TComp[Vector[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]]]] =
    refFiles.traverseU(findWorkingLoader(context.moduleLoaders))
      .map { loadedRefFiles =>
        loadedRefFiles
          .traverseM {
            case \/-(loaderAndData) => context.compMonadInstance.point(Vector(loaderAndData))
            case -\/(loadError) => context.compCompilationInstance.forErrors(Vector[LoaderAndData](), loadError)
          }
          .flatMap { refDataPairs =>
            loadModuleRefFromData[TComp](context)(refDataPairs)
          }
          .flatMap { moduleResults =>
            moduleResults.traverseM {
              case \/-(module) => context.compMonadInstance.point(Vector(module))
              case -\/(loadError) => context.compCompilationInstance.forErrors(Vector[PayloadResult[context.type]](), loadError)
            }
          }
      }

  def loadReferencedModules(context: Context)(refFiles: Vector[File]): IO[context.Comp[Vector[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]]]] =
    loadReferencedModulesImpl[context.Comp](context.withCompType)(refFiles)(context.compMonadInstance, context.compCompilationInstance)
      .map(identity)


}

