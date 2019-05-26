package dev.argon.compiler.loaders

import java.io.File

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.DependencyTree._
import cats._
import cats.implicits._

import scala.collection.immutable._

trait ModuleLoader[TContext <: Context with Singleton] {

  type ModuleData[I, TRes <: ResourceAccess[TContext#CompRE, TContext#Environment, I] with Singleton]

  def loadResource[I, TRes <: ResourceAccess[TContext#CompRE, TContext#Environment, I] with Singleton, A](res: TRes)(id: I)(f: Option[ModuleData[I, res.type]] => TContext#Comp[A]): TContext#Comp[A]
  def dataDescriptor[I, TRes <: ResourceAccess[TContext#CompRE, TContext#Environment, I] with Singleton](data: ModuleData[I, TRes]): ModuleDescriptor
  def dataReferencedModules[I, TRes <: ResourceAccess[TContext#CompRE, TContext#Environment, I] with Singleton](data: ModuleData[I, TRes]): Vector[ModuleDescriptor]
  def loadModuleReference[I, TRes <: ResourceAccess[TContext#CompRE, TContext#Environment, I] with Singleton](res: TRes)(data: ModuleData[I, res.type])(referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]]): TContext#Comp[ArModule[TContext, ReferencePayloadSpecifier]]

}

object ModuleLoader {


  def loadReferencedModules[TCompRE[-_, +_, +_], R, I: Show, A]
  (context: ContextCompRE[TCompRE, R])
  (refFiles: Vector[I])
  (f: Vector[ArModule[context.type, ReferencePayloadSpecifier]] => context.Comp[A])
  (implicit compInstance: CompilationRE[TCompRE, R], res: ResourceAccess[TCompRE, R, I])
  : context.Comp[A] = {

    import context.Comp

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

    def findWorkingLoader(loaders: Vector[ModuleLoader[context.type]])(id: I)(f: LoaderAndData => Comp[A]): Comp[A] =
      loaders match {
        case load +: tail =>
          load.loadResource[I, res.type, A](res)(id) {
            case Some(data) =>
              f(LoaderAndData(load)(data))

            case None =>
              findWorkingLoader(tail)(id)(f)
          }

        case Vector() =>
          Compilation[Comp].forErrors(CompilationError.CouldNotFindCompatibleModuleLoader(CompilationMessageSource.ResourceIdentifier(id)))
      }

    type PayloadResult = ArModule[context.type, ReferencePayloadSpecifier]

    val dependencyTreeOps
    : DependencyTreeOperations[Comp, LoaderAndData, ModuleDescriptor, PayloadResult, Either[CompilationError, ?]] =
      new DependencyTreeOperations[Comp, LoaderAndData, ModuleDescriptor, PayloadResult, Either[CompilationError, ?]] {
        override def getItemKey(item: LoaderAndData): ModuleDescriptor = item.loader.dataDescriptor[I, res.type](item.data)

        override def getItemDependencies(item: LoaderAndData): Vector[ModuleDescriptor] =
          item.loader.dataReferencedModules[I, res.type](item.data)

        override def loadItem(item: LoaderAndData, dependencies: Vector[PayloadResult]): Comp[PayloadResult] =
          item.loader.loadModuleReference[I, res.type](res)(item.data)(dependencies)

        override def circularReferenceHandler(item: LoaderAndData): Either[CompilationError, PayloadResult] =
          Left(CompilationError.CircularDependencyLoadingModule(CompilationMessageSource.ReferencedModule(item.loader.dataDescriptor[I, res.type](item.data))))

        override def missingDependencyHandler(item: LoaderAndData, missingDepKey: ModuleDescriptor): Either[CompilationError, PayloadResult] =
          Left(CompilationError.ModuleDependencyNotFound(missingDepKey, CompilationMessageSource.ReferencedModule(item.loader.dataDescriptor[I, res.type](item.data))))

      }

    def loadModuleRefFromData
    (context: ContextComp[Comp])
    (refDataPairs: Vector[LoaderAndData])
    : Comp[Vector[Either[CompilationError, PayloadResult]]] =
      loadDependencies[Comp, LoaderAndData, ModuleDescriptor, PayloadResult, Either[CompilationError, ?]](dependencyTreeOps)(refDataPairs)




    def impl(refFiles: Vector[I], loadedFiles: Vector[LoaderAndData]): Comp[A] =
      refFiles match {
        case id +: tail =>
          findWorkingLoader(context.moduleLoaders)(id) { loadedFile =>
            impl(tail, loadedFiles :+ loadedFile)
          }

        case Vector() =>
          loadModuleRefFromData(context)(loadedFiles)
            .flatMap { moduleResults =>
              moduleResults.flatTraverse {
                case Right(module) => context.compCompilationInstance.point(Vector(module))
                case Left(loadError) => context.compCompilationInstance.forErrors(loadError)
              }
            }
            .flatMap(f)
      }

    impl(refFiles, Vector())
  }
}

