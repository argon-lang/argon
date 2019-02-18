package com.mi3software.argon.compiler.loaders

import java.io.File

import com.mi3software.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.DependencyTree._
import com.mi3software.argon.util.MonadHelpers._
import scalaz.Scalaz._
import scalaz._

import scala.collection.immutable._

trait ModuleLoader[TContext <: Context with Singleton] {

  type ModuleData[I, TRes <: ResourceAccess[TContext#Comp, I] with Singleton]

  def loadResource[I, TRes <: ResourceAccess[TContext#Comp, I] with Singleton, A](id: I)(f: Option[ModuleData[I, TRes]] => TContext#Comp[A])(implicit res: TRes): TContext#Comp[A]
  def dataDescriptor[I, TRes <: ResourceAccess[TContext#Comp, I] with Singleton](data: ModuleData[I, TRes]): ModuleDescriptor
  def dataReferencedModules[I, TRes <: ResourceAccess[TContext#Comp, I] with Singleton](data: ModuleData[I, TRes]): Vector[ModuleDescriptor]
  def loadModuleReference[I, TRes <: ResourceAccess[TContext#Comp, I] with Singleton](data: ModuleData[I, TRes])(referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]])(implicit res: TRes): TContext#Comp[ArModule[TContext, ReferencePayloadSpecifier]]

}

object ModuleLoader {


  def loadReferencedModules[TComp[+_] : Compilation, I: Show, A]
  (context: ContextComp[TComp])
  (refFiles: Vector[I])
  (f: Vector[ArModule[context.type, ReferencePayloadSpecifier]] => TComp[A])
  (implicit res: ResourceAccess[TComp, I])
  : TComp[A] = {

    trait LoaderAndData {
      val loader: ModuleLoader[context.type]
      val data: loader.ModuleData[I, res.type]
    }

    object LoaderAndData {
      def apply(loader2: ModuleLoader[context.type])(data2: loader2.ModuleData[I, res.type]): LoaderAndData =
        new LoaderAndData {
          override val loader: loader2.type = loader2
          override val data: loader.ModuleData[I, res.type] = data2
        }
    }

    def findWorkingLoader(loaders: Vector[ModuleLoader[context.type]])(id: I)(f: LoaderAndData => TComp[A]): TComp[A] =
      loaders match {
        case load +: tail =>
          load.loadResource[I, res.type, A](id) {
            case Some(data) =>
              f(LoaderAndData(load)(data))

            case None =>
              findWorkingLoader(tail)(id)(f)
          }(res)

        case Vector() =>
          Compilation[TComp].forErrors(CompilationError.CouldNotFindCompatibleModuleLoader(CompilationMessageSource.ModuleResource(id)))
      }

    type PayloadResult = ArModule[context.type, ReferencePayloadSpecifier]

    val dependencyTreeOps
    : DependencyTreeOperations[TComp, LoaderAndData, ModuleDescriptor, PayloadResult, CompilationError \/ ?] =
      new DependencyTreeOperations[TComp, LoaderAndData, ModuleDescriptor, PayloadResult, CompilationError \/ ?] {
        override def getItemKey(item: LoaderAndData): ModuleDescriptor = item.loader.dataDescriptor[I, res.type](item.data)

        override def getItemDependencies(item: LoaderAndData): Vector[ModuleDescriptor] =
          item.loader.dataReferencedModules[I, res.type](item.data)

        override def loadItem(item: LoaderAndData, dependencies: Vector[PayloadResult]): TComp[PayloadResult] =
          item.loader.loadModuleReference[I, res.type](item.data)(dependencies)(res)

        override def circularReferenceHandler(item: LoaderAndData): CompilationError \/ PayloadResult =
          -\/(CompilationError.CircularDependencyLoadingModule(CompilationMessageSource.ReferencedModule(item.loader.dataDescriptor[I, res.type](item.data))))

        override def missingDependencyHandler(item: LoaderAndData, missingDepKey: ModuleDescriptor): CompilationError \/ PayloadResult =
          -\/(CompilationError.ModuleDependencyNotFound(missingDepKey, CompilationMessageSource.ReferencedModule(item.loader.dataDescriptor[I, res.type](item.data))))

      }

    def loadModuleRefFromData
    (context: ContextComp[TComp])
    (refDataPairs: Vector[LoaderAndData])
    : TComp[Vector[CompilationError \/ PayloadResult]] =
    loadDependencies[TComp, LoaderAndData, ModuleDescriptor, PayloadResult, CompilationError \/ ?](dependencyTreeOps)(refDataPairs)




    def impl(refFiles: Vector[I], loadedFiles: Vector[LoaderAndData]): TComp[A] =
      refFiles match {
        case id +: tail =>
          findWorkingLoader(context.moduleLoaders)(id) { loadedFile =>
            impl(tail, loadedFiles :+ loadedFile)
          }

        case Vector() =>
          loadModuleRefFromData(context)(loadedFiles)
            .flatMap { moduleResults =>
              moduleResults.traverseM {
                case \/-(module) => context.compCompilationInstance.point(Vector(module))
                case -\/(loadError) => context.compCompilationInstance.forErrors(loadError)
              }
            }
            .flatMap(f)
      }

    impl(refFiles, Vector())
  }
}

