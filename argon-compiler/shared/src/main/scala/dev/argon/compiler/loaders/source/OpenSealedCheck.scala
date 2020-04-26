package dev.argon.compiler.loaders.source

import dev.argon.compiler.core._
import dev.argon.compiler._
import dev.argon.util.FileID
import cats._
import cats.implicits._
import zio.IO

trait OpenSealedCheck {

  val descriptor: Descriptor
  val fileId: FileID

  final def checkExtendClass[TContext <: Context with Singleton, TPS[_, _]](arClass: ArClass[TContext, TPS])(source: CompilationMessageSource): Comp[Unit] =
    if(!arClass.isOpen)
      Compilation.forErrors(CompilationError.NonOpenClassExtendedError(source))
    else if(arClass.isSealed && (arClass.descriptor.moduleDescriptor =!= descriptor.moduleDescriptor || arClass.fileId =!= fileId))
      Compilation.forErrors(CompilationError.SealedClassExtendedError(source))
    else
      IO.unit

  final def checkExtendTrait[TContext <: Context with Singleton, TPS[_, _]](arTrait: ArTrait[TContext, TPS])(source: CompilationMessageSource): Comp[Unit] =
    if(arTrait.isSealed && (arTrait.descriptor.moduleDescriptor =!= descriptor.moduleDescriptor || arTrait.fileId =!= fileId))
      Compilation.forErrors(CompilationError.SealedTraitExtendedError(source))
    else
      IO.unit

}
