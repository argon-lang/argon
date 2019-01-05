package com.mi3software.argon.compiler.loaders

import java.io.File

import com.mi3software.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.DependencyTree._
import com.mi3software.argon.util.MonadHelpers._
import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO

import scala.collection.immutable._

trait ModuleLoader[TContext <: Context with Singleton] {

  type ModuleData

  def loadFile(file: File): IO[Option[ModuleData]]
  def dataDescriptor(data: ModuleData): Option[ModuleDescriptor]
  def dataReferencedModules(data: ModuleData): Vector[ModuleDescriptor]
  def loadModuleReference(context: TContext)(data: ModuleData)(referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]]): context.Comp[ArModule[context.type, ReferencePayloadSpecifier]]

}

object ModuleLoader {

  type PayloadResult[TContext <: Context] = ArModule[TContext, ReferencePayloadSpecifier]
  type PayloadMap[TContext <: Context] = Map[ModuleDescriptor, PayloadResult[TContext]]

  private trait LoaderAndData[TContext <: Context with Singleton] {
    val loader: ModuleLoader[TContext]
    val data: loader.ModuleData
    val descriptor: ModuleDescriptor
  }

  private object LoaderAndData {
    def apply[TContext <: Context with Singleton](loader2: ModuleLoader[TContext])(data2: loader2.ModuleData)(desc: ModuleDescriptor): LoaderAndData[TContext] =
      new LoaderAndData[TContext] {
        override val loader: loader2.type = loader2
        override val data: loader.ModuleData = data2
        override val descriptor: ModuleDescriptor = desc
      }
  }

  private def findWorkingLoader[TContext <: Context with Singleton](loaders: Vector[ModuleLoader[TContext]])(file: File): IO[CompilationError \/ LoaderAndData[TContext]] =
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
  : DependencyTreeOperations[context.Comp, LoaderAndData[context.type], ModuleDescriptor, PayloadResult[context.type], CompilationError \/ ?] =
    new DependencyTreeOperations[context.Comp, LoaderAndData[context.type], ModuleDescriptor, PayloadResult[context.type], CompilationError \/ ?] {
      override def getItemKey(item: LoaderAndData[context.type]): ModuleDescriptor = item.descriptor

      override def getItemDependencies(item: LoaderAndData[context.type]): Vector[ModuleDescriptor] =
        item.loader.dataReferencedModules(item.data)

      override def loadItem(item: LoaderAndData[context.type], dependencies: Vector[PayloadResult[context.type]]): context.Comp[PayloadResult[context.type]] =
        item.loader.loadModuleReference(context)(item.data)(dependencies)

      override def circularReferenceHandler(item: LoaderAndData[context.type]): CompilationError \/ PayloadResult[context.type] =
        -\/(CompilationError.CircularDependencyLoadingModule(CompilationMessageSource.ReferencedModule(item.descriptor)))

      override def missingDependencyHandler(item: LoaderAndData[context.type], missingDepKey: ModuleDescriptor): CompilationError \/ PayloadResult[context.type] =
        -\/(CompilationError.ModuleDependencyNotFound(missingDepKey, CompilationMessageSource.ReferencedModule(item.descriptor)))

    }

  private def loadModuleRefFromData[TComp[+_] : Monad]
  (context: ContextComp[TComp])
  (refDataPairs: Vector[LoaderAndData[context.type]])
  : TComp[Vector[CompilationError \/ PayloadResult[context.type]]] =
    loadDependencies[TComp, LoaderAndData[context.type], ModuleDescriptor, PayloadResult[context.type], CompilationError \/ ?](dependencyTreeOps(context))(refDataPairs)



  def loadReferencedModules[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (refFiles: Vector[File])
  : IO[TComp[Vector[ArModule[context.type, ReferencePayloadSpecifier]]]] =
    refFiles.traverseU(findWorkingLoader(context.moduleLoaders)(_))
      .map { loadedRefFiles =>
        loadedRefFiles
          .traverseM {
            case \/-(loaderAndData) => context.compCompilationInstance.point(Vector(loaderAndData))
            case -\/(loadError) => context.compCompilationInstance.forErrors(loadError)
          }
          .flatMap { refDataPairs =>
            loadModuleRefFromData[TComp](context)(refDataPairs)
          }
          .flatMap { moduleResults =>
            moduleResults.traverseM {
              case \/-(module) => context.compCompilationInstance.point(Vector(module))
              case -\/(loadError) => context.compCompilationInstance.forErrors(loadError)
            }
          }
      }

}

