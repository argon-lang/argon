package dev.argon.plugins.tube

import dev.argon.compiler.*
import dev.argon.compiler.tube.ArTubeC
import dev.argon.compiler.definitions.*
import dev.argon.io.ZipFileResource
import zio.*
import zio.stm.TMap

private[tube] object TubeWriterImplementation {
  def apply
  (ctx: Context { type Error >: TubeError })
  (tube: ArTubeC & HasContext[ctx.type] & HasImplementation[true])
  (entryQueue: Queue[Exit[Option[ctx.Error], ZipFileResource.Entry[ctx.Env, ctx.Error]]])
  : UIO[TubeWriterBase { val context: ctx.type; type IsImplementation = true }] =
    for
      classIdMap <- TMap.empty[ArClassC & HasContext[ctx.type], BigInt].commit
      traitIdMap <- TMap.empty[ArTraitC & HasContext[ctx.type], BigInt].commit
      functionIdMap <- TMap.empty[ArFuncC & HasContext[ctx.type], BigInt].commit
      methodIdMap <- TMap.empty[ArMethodC & HasContext[ctx.type], BigInt].commit
      classCtorIdMap <- TMap.empty[ClassConstructorC & HasContext[ctx.type], BigInt].commit
    yield
      new TubeWriterBase:
        override val context: ctx.type = ctx
        override type IsImplementation = true

        override protected def ifImplementation[A, B, C](value: A)(whenImplementation: A => C, whenInterface: => C): C =
          whenImplementation(value)

        override protected def dummyImplementationValue: Unit = ()

        override val currentTube: ArTube & HasImplementation[true] = tube

        override protected def pushEntry(entry: ZipFileResource.Entry[context.Env, context.Error]): UIO[Unit] =
          entryQueue.offer(Exit.succeed(entry)).unit

        override val classIds: TMap[ArClass, BigInt] = classIdMap
        override val traitIds: TMap[ArTrait, BigInt] = traitIdMap
        override val functionIds: TMap[ArFunc, BigInt] = functionIdMap
        override val methodIds: TMap[ArMethod, BigInt] = methodIdMap
        override val classCtorIds: TMap[ClassConstructor, BigInt] = classCtorIdMap
      end new
}
