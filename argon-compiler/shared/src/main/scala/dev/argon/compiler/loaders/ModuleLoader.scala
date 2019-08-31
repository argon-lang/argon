package dev.argon.compiler.loaders

import dev.argon.compiler.core.PayloadSpecifiers.ReferencePayloadSpecifier
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.DependencyTree._
import cats._
import cats.implicits._

import scala.collection.immutable._

trait ModuleLoader[TContext <: Context with Singleton] {

  type ModuleData[TRes <: ResourceAccess[TContext] with Singleton]

  def loadResource[TRes <: ResourceAccess[TContext] with Singleton, A](res: TRes)(id: TContext#ResIndicator)(f: Option[ModuleData[res.type]] => TContext#Comp[A]): TContext#Comp[A]
  def dataDescriptor[TRes <: ResourceAccess[TContext] with Singleton](data: ModuleData[TRes]): ModuleDescriptor
  def dataReferencedModules[TRes <: ResourceAccess[TContext] with Singleton](data: ModuleData[TRes]): Vector[ModuleDescriptor]
  def loadModuleReference[TRes <: ResourceAccess[TContext] with Singleton](res: TRes)(data: ModuleData[res.type])(referencedModules: Vector[ArModule[TContext, ReferencePayloadSpecifier]]): TContext#Comp[ArModule[TContext, ReferencePayloadSpecifier]]

}

object ModuleLoader {


  def loadReferencedModules[A]
  (context: Context)
  (refFiles: Vector[context.ResIndicator])
  (f: Vector[ArModule[context.type, ReferencePayloadSpecifier]] => context.Comp[A])
  (implicit resShow: Show[context.ResIndicator], res: ResourceAccess[context.type])
  : context.Comp[A] = {

    import context._

    trait LoaderAndData {
      val loader: ModuleLoader[context.type]
      val data: loader.ModuleData[res.type]
    }

    object LoaderAndData {
      def apply(loader2: ModuleLoader[context.type])(data2: loader2.ModuleData[res.type]): LoaderAndData =
        new LoaderAndData {
          override val loader: loader2.type = loader2
          override val data: loader.ModuleData[res.type] = data2
        }
    }

    def findWorkingLoader(loaders: Vector[ModuleLoader[context.type]])(id: ResIndicator)(f: LoaderAndData => Comp[A]): Comp[A] =
      loaders match {
        case load +: tail =>
          load.loadResource[res.type, A](res)(id) {
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
        override def getItemKey(item: LoaderAndData): ModuleDescriptor = item.loader.dataDescriptor[res.type](item.data)

        override def getItemDependencies(item: LoaderAndData): Vector[ModuleDescriptor] =
          item.loader.dataReferencedModules[res.type](item.data)

        override def loadItem(item: LoaderAndData, dependencies: Vector[PayloadResult]): Comp[PayloadResult] =
          item.loader.loadModuleReference[res.type](res)(item.data)(dependencies)

        override def circularReferenceHandler(item: LoaderAndData): Either[CompilationError, PayloadResult] =
          Left(CompilationError.CircularDependencyLoadingModule(CompilationMessageSource.ReferencedModule(item.loader.dataDescriptor[res.type](item.data))))

        override def missingDependencyHandler(item: LoaderAndData, missingDepKey: ModuleDescriptor): Either[CompilationError, PayloadResult] =
          Left(CompilationError.ModuleDependencyNotFound(missingDepKey, CompilationMessageSource.ReferencedModule(item.loader.dataDescriptor[res.type](item.data))))

      }

    def loadModuleRefFromData
    (refDataPairs: Vector[LoaderAndData])
    : Comp[Vector[Either[CompilationError, PayloadResult]]] =
      loadDependencies[Comp, LoaderAndData, ModuleDescriptor, PayloadResult, Either[CompilationError, ?]](dependencyTreeOps)(refDataPairs)




    def impl(refFiles: Vector[ResIndicator], loadedFiles: Vector[LoaderAndData]): Comp[A] =
      refFiles match {
        case id +: tail =>
          findWorkingLoader(context.moduleLoaders)(id) { loadedFile =>
            impl(tail, loadedFiles :+ loadedFile)
          }

        case Vector() =>
          loadModuleRefFromData(loadedFiles)
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

