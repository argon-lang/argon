package dev.argon.compiler.loaders.source

import dev.argon.compiler.core._
import dev.argon.compiler._
import dev.argon.util.FileID

import scalaz._
import Scalaz._

import cats.implicits._

trait OpenSealedCheck {

  val descriptor: Descriptor
  val fileId: FileID

  final def checkExtendClass[TComp[+_] : Compilation, TContext <: Context with Singleton, TPS[_, _]](arClass: ArClass[TContext, TPS])(source: CompilationMessageSource): TComp[Unit] =
    if(!arClass.isOpen)
      Compilation[TComp].forErrors(CompilationError.NonOpenClassExtendedError(source))
    else if(arClass.isSealed && (arClass.descriptor.moduleDescriptor =/= descriptor.moduleDescriptor || arClass.fileId =/= fileId))
      Compilation[TComp].forErrors(CompilationError.SealedClassExtendedError(source))
    else
      ().point[TComp]

  final def checkExtendTrait[TComp[+_] : Compilation, TContext <: Context with Singleton, TPS[_, _]](arTrait: ArTrait[TContext, TPS])(source: CompilationMessageSource): TComp[Unit] =
    if(arTrait.isSealed && (arTrait.descriptor.moduleDescriptor =/= descriptor.moduleDescriptor || arTrait.fileId =/= fileId))
      Compilation[TComp].forErrors(CompilationError.SealedTraitExtendedError(source))
    else
      ().point[TComp]

}
