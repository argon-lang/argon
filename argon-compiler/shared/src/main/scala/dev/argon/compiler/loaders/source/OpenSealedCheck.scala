package dev.argon.compiler.loaders.source

import dev.argon.compiler.core._
import dev.argon.compiler._
import dev.argon.util.FileID
import cats._
import cats.implicits._
import zio.IO

trait OpenSealedCheck {

  def ownerModuleId: ModuleId
  val fileId: FileID

  def getClassModule[TContext <: Context with Singleton, TPS[_, _]](arClass: ArClass[TContext, TPS]): ModuleId =
    arClass.owner match {
      case ClassOwner.ByNamespace(module, _, _) => module.id
    }

  def getTraitModule[TContext <: Context with Singleton, TPS[_, _]](arTrait: ArTrait[TContext, TPS]): ModuleId =
    arTrait.owner match {
      case TraitOwner.ByNamespace(module, _, _) => module.id
    }

  def getDataCtorModule[TContext <: Context with Singleton, TPS[_, _]](ctor: DataConstructor[TContext, TPS]): ModuleId =
    ctor.owner match {
      case DataConstructorOwner.ByNamespace(module, _, _) => module.id
    }

  final def checkExtendClass[TContext <: Context with Singleton, TPS[_, _]](arClass: ArClass[TContext, TPS])(source: CompilationMessageSource): Comp[Unit] =
    if(!arClass.isOpen)
      Compilation.forErrors(CompilationError.NonOpenClassExtendedError(source))
    else if(arClass.isSealed && (getClassModule(arClass) =!= ownerModuleId || arClass.fileId =!= fileId))
      Compilation.forErrors(CompilationError.SealedClassExtendedError(source))
    else
      IO.unit

  final def checkExtendTrait[TContext <: Context with Singleton, TPS[_, _]](arTrait: ArTrait[TContext, TPS])(source: CompilationMessageSource): Comp[Unit] =
    if(arTrait.isSealed && (getTraitModule(arTrait) =!= ownerModuleId || arTrait.fileId =!= fileId))
      Compilation.forErrors(CompilationError.SealedTraitExtendedError(source))
    else
      IO.unit

}
