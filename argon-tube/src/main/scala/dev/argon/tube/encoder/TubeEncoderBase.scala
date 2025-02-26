package dev.argon.tube.encoder

import dev.argon.compiler as c
import zio.*
import zio.stream.*
import zio.stm.*
import dev.argon.ast
import dev.argon.compiler.SignatureEraser
import dev.argon.compiler.HasContext
import dev.argon.tube.{ExternMap, SupportedPlatform}
import dev.argon.tube.encoder.TubeEncoderBase.EncodeContext
import dev.argon.tube.loader.TubeFormatException
import esexpr.ESExpr


trait TubeEncoderBase[Entry] {
  sealed trait EncodeState extends c.UsingContext {
    override val context: EncodeContext
    import context.DefaultExprContext.Expr as ArExpr

    val tube: ArTube

    protected[TubeEncoderBase] lazy val emitter: Emitter
    
    def emitEntryBuilder(entry: Comp[Entry]): UIO[Unit]
    def assignTubeId(tubeName: c.TubeName, id: Int): UIO[Unit]
    def assignModuleId(moduleName: c.ModuleName, id: Int): UIO[Unit]

    def getTubeId(tubeName: c.TubeName): UIO[BigInt]
    def getModuleId(moduleName: c.ModuleName): UIO[BigInt]
    def getFunctionId(func: ArFunc): UIO[BigInt]
    def getRecordId(rec: ArRecord): UIO[BigInt]
    def getRecordFieldId(rec: RecordField): UIO[BigInt]

    trait Emitter {
      def emitTube: Comp[Unit]

      def emitModuleReference(moduleName: c.ModuleName, id: BigInt): Comp[Entry]
      def emitFunction(func: ArFunc, id: BigInt): Comp[Entry]
      def emitRecord(rec: ArRecord, id: BigInt): Comp[Entry]
      def emitRecordFieldInfo(field: RecordField, id: BigInt): Comp[Entry]
    }
  }



  protected def createEmitter(state: EncodeState): state.Emitter

  final def encode(context: EncodeContext)(tube: c.ArTubeC & c.HasContext[context.type]): ZStream[context.Env, context.Error, Entry] =
    val ctx: context.type = context
    val tube2: tube.type = tube

    class IdManager[A](mapping: TMap[A, Int])(using CanEqual[A, A]) {
      def assignId(value: A, id: Int): UIO[Unit] =
        mapping.put(value, id).commit
      
      def getIdWith(value: A)(entryBuilderQueue: TEnqueue[context.Comp[Entry]])(emit: (A, BigInt) => context.Comp[Entry]): UIO[BigInt] =
        mapping.get(value).flatMap {
          case Some(id) => ZSTM.succeed(id : BigInt)
          case none =>
            for
              id <- mapping.size
              _ <- mapping.put(value, id)
              _ <- entryBuilderQueue.offer(emit(value, id))
            yield id : BigInt

        }.commit

      def getIdOrFail(value: A): UIO[BigInt] =
        mapping.get(value).map { id => id.get : BigInt }.commit

    }

    def createIdManager[A](using CanEqual[A, A]): UIO[IdManager[A]] =
      for
        mapping <- TMap.empty[A, Int].commit
      yield IdManager(mapping)


    ZStream.unwrap(
      for
        entryBuilders <- TQueue.unbounded[context.Comp[Entry]].commit

        tubeIds <- createIdManager[c.TubeName]
        moduleIds <- createIdManager[c.ModuleName]
        functionIds <- createIdManager[c.ArFuncC & c.HasContext[context.type]]
        recordIds <- createIdManager[c.ArRecordC & c.HasContext[context.type]]
        recordFieldIds <- createIdManager[c.RecordFieldC & c.HasContext[context.type]]

        encodeState = new EncodeState {

          override val context: ctx.type = ctx
          override val tube: ArTube = tube2

          override protected[TubeEncoderBase] lazy val emitter: Emitter = createEmitter(this)

          override def emitEntryBuilder(entry: context.Comp[Entry]): UIO[Unit] =
            entryBuilders.offer(entry).commit.unit

          override def assignTubeId(tubeName: c.TubeName, id: Int): UIO[Unit] =
            tubeIds.assignId(tubeName, id)

          override def assignModuleId(moduleName: c.ModuleName, id: Int): UIO[Unit] =
            moduleIds.assignId(moduleName, id)

          override def getTubeId(tubeName: c.TubeName): UIO[BigInt] =
            tubeIds.getIdOrFail(tubeName)

          override def getModuleId(moduleName: c.ModuleName): UIO[BigInt] =
            moduleIds.getIdWith(moduleName)(entryBuilders)(emitter.emitModuleReference)

          override def getFunctionId(func: ArFunc): UIO[BigInt] =
            functionIds.getIdWith(func)(entryBuilders)(emitter.emitFunction)

          override def getRecordId(rec: ArRecord): UIO[BigInt] =
            recordIds.getIdWith(rec)(entryBuilders)(emitter.emitRecord)

          override def getRecordFieldId(field: RecordField): UIO[BigInt] =
            recordFieldIds.getIdWith(field)(entryBuilders)(emitter.emitRecordFieldInfo)
        }

        _ <- encodeState.emitter.emitTube

      yield ZStream
        .repeatZIOChunkOption(
          entryBuilders.takeAll.commit
            .flatMap { chunk =>
              if chunk.isEmpty then
                ZIO.fail(None)
              else
                ZIO.succeed(chunk)
            }
        )
        .mapZIO(identity)
    )
  end encode

}

object TubeEncoderBase {
  type EncodeContext = c.Context {
    type Error >: TubeFormatException
    val implementations: c.Context.ImplementationExterns {
      type TubeMetadata = (Seq[SupportedPlatform], Map[String, ESExpr])
      type ExternFunction = ExternMap
    }
  }
}
