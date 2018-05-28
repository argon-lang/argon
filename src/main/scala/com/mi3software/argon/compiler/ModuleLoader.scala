package com.mi3software.argon.compiler

import java.io.File

import scalaz._
import Scalaz._
import com.mi3software.argon.util.MonadHelpers._
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

  private def findWorkingLoader(loaders: Vector[ModuleLoader])(file: File): OptionT[IO, LoaderAndData] =
    OptionT(findFirst(loaders){ load =>
      OptionT(load.loadFile(file))
        .flatMap { data =>
          val descOpt = load.dataDescriptor(data)
          OptionT(IO(descOpt))
            .map(LoaderAndData(load)(data))
        }
        .run
    })

  private def loadModuleRefFromData
  (context: Context)
  (dataPair: LoaderAndData)
  (refDataPairs: Vector[LoaderAndData])
  : context.Comp[Option[PayloadResult[context.type]]] =
    loadDependencyTree[context.Comp, LoaderAndData, ModuleDescriptor, PayloadResult[context.type]](dataPair)(refDataPairs)(pair => pair.loader.dataDescriptor(pair.data))(pair => pair.loader.dataReferencedModules(pair.data))(
      (dataPair, deps) => dataPair.loader.loadModuleReference(context)(dataPair.data)(deps.flatMap(_.toList.toVector))
    )(context.compMonadInstance, implicitly[Equal[ModuleDescriptor]])

  def loadModuleReference(context: Context)(file: File)(refFiles: Vector[File]): IO[context.Comp[Option[ArModuleWithPayload[context.type, PayloadSpecifiers.ReferencePayloadSpecifier]]]] =
    (
      for {
        loadedFile <- findWorkingLoader(context.moduleLoaders)(file)
        loadedRefFiles <- refFiles.traverse[OptionT[IO, ?], LoaderAndData](findWorkingLoader(context.moduleLoaders))
      } yield loadModuleRefFromData(context)(loadedFile)(loadedRefFiles)
    ).run.map { _.getOrElse(context.compMonadInstance.pure(None)) }

}

