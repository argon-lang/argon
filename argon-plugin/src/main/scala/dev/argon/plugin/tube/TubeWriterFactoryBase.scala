package dev.argon.plugin.tube

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.io.ZipFileResource
import zio.*
import zio.stm.TMap

private[tube] abstract class TubeWriterFactoryBase[IsImpl <: Boolean] {

  protected def ifImplementation[A, B, C](value: IsImpl match {
    case true => A
    case false => B
  })(whenImplementation: A => C, whenInterface: Either[A, B] => C): C

  protected def dummyImplementationValue: IsImpl match {
    case true => Unit
    case false => Unit
  }

  def apply
  (ctx: Context)
  (tube: ArTubeC & HasContext[ctx.type] & HasImplementation[IsImpl])
  : UIO[TubeWriterBase {val context: ctx.type; type IsImplementation = IsImpl}] =
    for
      classIdMap <- IdentifierMaps.make[ArClassC & HasContext[ctx.type], IsImpl]
      traitIdMap <- IdentifierMaps.make[ArTraitC & HasContext[ctx.type], IsImpl]
      functionIdMap <- IdentifierMaps.make[ArFuncC & HasContext[ctx.type], IsImpl]
      methodIdMap <- IdentifierMaps.make[ArMethodC & HasContext[ctx.type], IsImpl]
      classCtorIdMap <- IdentifierMaps.make[ClassConstructorC & HasContext[ctx.type], IsImpl]

      writer =
        new TubeWriterBase:
          override val context: ctx.type = ctx
          override type IsImplementation = IsImpl

          override protected def ifImplementation[A, B, C](value: IsImplementation match {
            case true => A
            case false => B
          })(whenImplementation: A => C, whenInterface: Either[A, B] => C): C =
            TubeWriterFactoryBase.this.ifImplementation(value)(whenImplementation, whenInterface)

          override protected def dummyImplementationValue: IsImplementation match {
            case true => Unit
            case false => Unit
          } =
            TubeWriterFactoryBase.this.dummyImplementationValue

          override val currentTube: ArTube & HasImplementation[IsImpl] = tube

          override val classIds: IdentifierMaps[ArClass, IsImpl] = classIdMap
          override val traitIds: IdentifierMaps[ArTrait, IsImpl] = traitIdMap
          override val functionIds: IdentifierMaps[ArFunc, IsImpl] = functionIdMap
          override val methodIds: IdentifierMaps[ArMethod, IsImpl] = methodIdMap
          override val classCtorIds: IdentifierMaps[ClassConstructor, IsImpl] = classCtorIdMap
        end new
    yield writer

}
