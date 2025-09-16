package dev.argon.tube.encoder

import dev.argon.compiler as c
import zio.*
import zio.stream.*
import zio.stm.*
import dev.argon.ast
import dev.argon.compiler.{HasContext, ImportSpecifier, MethodOwner, SignatureEraser}
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
    def assignTubeId(tubeName: c.TubeName, id: BigInt): UIO[Unit]
    def assignModuleId(moduleName: c.ModuleName, id: BigInt): UIO[Unit]

    def newSyntheticImport(parentImport: ImportSpecifier | SyntheticImport): UIO[SyntheticImport]

    def getTubeId(tubeName: c.TubeName): UIO[BigInt]
    def getModuleId(moduleName: c.ModuleName): UIO[BigInt]
    def getFunctionId(func: ArFunc): UIO[BigInt]
    def newSyntheticFunctionId: UIO[BigInt]
    def getRecordId(rec: ArRecord): UIO[BigInt]
    def getRecordFieldId(field: RecordField): UIO[BigInt]
    def getEnumId(e: ArEnum): UIO[BigInt]
    def getEnumVariantId(v: EnumVariant): UIO[BigInt]
    def getSyntheticEnumVariantImport(v: EnumVariant): Comp[SyntheticImport]
    def getTraitId(t: ArTrait): UIO[BigInt]
    def getMethodId(m: ArMethod): UIO[BigInt]
    def getSyntheticMethodImport(m: ArMethod): Comp[SyntheticImport]
    def newSyntheticMethodId: UIO[BigInt]

    trait Emitter {
      def emitTube: Comp[Unit]

      def emitModuleReference(moduleName: c.ModuleName, id: BigInt): Comp[Entry]
      def emitFunction(func: ArFunc, id: BigInt): Comp[Entry]
      def emitRecord(rec: ArRecord, id: BigInt): Comp[Entry]
      def emitRecordFieldInfo(field: RecordField, id: BigInt): Comp[Entry]
      def emitEnum(e: ArEnum, id: BigInt): Comp[Entry]
      def emitEnumVariantInfo(v: EnumVariant, id: BigInt): Comp[Entry]
      def emitTrait(t: ArTrait, id: BigInt): Comp[Entry]
      def emitMethod(m: ArMethod, id: BigInt): Comp[Entry]
    }
  }



  protected def createEmitter(state: EncodeState): state.Emitter

  final def encode(context: EncodeContext)(tube: c.ArTubeC & c.HasContext[context.type]): ZStream[context.Env, context.Error, Entry] =
    val ctx: context.type = context
    val tube2: tube.type = tube

    import context.DefaultExprContext.ExpressionOwner

    class IdManager[A](nextId: TRef[BigInt], mapping: TMap[A, BigInt])(using CanEqual[A, A]) {
      def assignId(value: A, id: BigInt): UIO[Unit] =
        (mapping.put(value, id) *> nextId.update(_.max(id + 1))).commit

      def getIdWith(value: A)(entryBuilderQueue: TEnqueue[context.Comp[Entry]])(emit: (A, BigInt) => context.Comp[Entry]): UIO[BigInt] =
        mapping.get(value).flatMap {
          case Some(id) => ZSTM.succeed(id: BigInt)
          case none =>
            for
              id <- claimId
              _ <- mapping.put(value, id)
              _ <- entryBuilderQueue.offer(emit(value, id))
            yield id
        }.commit

      def getIdOrFail(value: A): UIO[BigInt] =
        mapping.get(value).map { id => id.get }.commit

      def claimId: USTM[BigInt] =
        nextId.getAndUpdate(_ + 1)

    }

    def createIdManager[A](using CanEqual[A, A]): UIO[IdManager[A]] =
      (
        for
          nextId <- TRef.make[BigInt](0)
          mapping <- TMap.empty[A, BigInt]
        yield IdManager(nextId, mapping)
      ).commit


    ZStream.unwrap(
      for
        entryBuilders <- TQueue.unbounded[context.Comp[Entry]].commit

        syntheticIndexes <- TMap.empty[ImportSpecifier | SyntheticImport, BigInt].commit

        tubeIds <- createIdManager[c.TubeName]
        moduleIds <- createIdManager[c.ModuleName]
        functionIds <- createIdManager[c.ArFuncC & c.HasContext[context.type]]
        recordIds <- createIdManager[c.ArRecordC & c.HasContext[context.type]]
        recordFieldIds <- createIdManager[c.RecordFieldC & c.HasContext[context.type]]
        enumIds <- createIdManager[c.ArEnumC & c.HasContext[context.type]]
        enumVariantIds <- createIdManager[c.EnumVariantC & c.HasContext[context.type]]
        enumVariantImports <- TMap.empty[c.EnumVariantC & c.HasContext[context.type], SyntheticImport].commit
        traitIds <- createIdManager[c.ArTraitC & c.HasContext[context.type]]
        methodIds <- createIdManager[c.ArMethodC & c.HasContext[context.type]]
        methodImports <- TMap.empty[c.ArMethodC & c.HasContext[context.type], SyntheticImport].commit

        encodeState = new EncodeState {

          override val context: ctx.type = ctx
          override val tube: ArTube = tube2

          override protected[TubeEncoderBase] lazy val emitter: Emitter = createEmitter(this)

          override def emitEntryBuilder(entry: context.Comp[Entry]): UIO[Unit] =
            entryBuilders.offer(entry).commit.unit

          override def assignTubeId(tubeName: c.TubeName, id: BigInt): UIO[Unit] =
            tubeIds.assignId(tubeName, id)

          override def assignModuleId(moduleName: c.ModuleName, id: BigInt): UIO[Unit] =
            moduleIds.assignId(moduleName, id)


          private def getMemoSyntheticImport[A](a: A)(memo: TMap[A, SyntheticImport])(getParentImport: Comp[ImportSpecifier | SyntheticImport]): Comp[SyntheticImport] =
            memo.get(a).commit.flatMap {
              case Some(imp) => ZIO.succeed(imp)
              case None =>
                getParentImport
                  .flatMap(newSyntheticImport)
                  .tap { imp => memo.put(a, imp).commit }
            }

          private def getImportForOwner(owner: ExpressionOwner): Comp[ImportSpecifier | SyntheticImport] =
            owner match {
              case ExpressionOwner.Func(f) => f.importSpecifier
              case ExpressionOwner.Rec(r) => r.importSpecifier
              case ExpressionOwner.Enum(e) => e.importSpecifier
              case ExpressionOwner.Trait(t) => t.importSpecifier
              case ExpressionOwner.EnumVariant(v) => getSyntheticEnumVariantImport(v)
              case ExpressionOwner.Method(m) => getSyntheticMethodImport(m)
            }

          override def newSyntheticImport(parentImport: ImportSpecifier | SyntheticImport): UIO[SyntheticImport] =
            (
              for
                index <- syntheticIndexes.get(parentImport).map {
                  case Some(index) => index + 1
                  case None => 0 : BigInt
                }
                _ <- syntheticIndexes.put(parentImport, index)
              yield SyntheticImport(parentImport, index)
            ).commit


          override def getTubeId(tubeName: c.TubeName): UIO[BigInt] =
            tubeIds.getIdOrFail(tubeName)

          override def getModuleId(moduleName: c.ModuleName): UIO[BigInt] =
            moduleIds.getIdWith(moduleName)(entryBuilders)(emitter.emitModuleReference)

          override def getFunctionId(func: ArFunc): UIO[BigInt] =
            functionIds.getIdWith(func)(entryBuilders)(emitter.emitFunction)

          override def newSyntheticFunctionId: UIO[BigInt] =
            functionIds.claimId.commit

          override def getRecordId(rec: ArRecord): UIO[BigInt] =
            recordIds.getIdWith(rec)(entryBuilders)(emitter.emitRecord)

          override def getRecordFieldId(field: RecordField): UIO[BigInt] =
            recordFieldIds.getIdWith(field)(entryBuilders)(emitter.emitRecordFieldInfo)

          override def getEnumId(e: ArEnum): UIO[BigInt] =
            enumIds.getIdWith(e)(entryBuilders)(emitter.emitEnum)

          override def getEnumVariantId(v: EnumVariant): UIO[BigInt] =
            enumVariantIds.getIdWith(v)(entryBuilders)(emitter.emitEnumVariantInfo)

          override def getSyntheticEnumVariantImport(v: EnumVariant): Comp[SyntheticImport] =
            getMemoSyntheticImport(v)(enumVariantImports)(
              v.owningEnum.importSpecifier
            )

          override def getTraitId(t: ArTrait): UIO[BigInt] =
            traitIds.getIdWith(t)(entryBuilders)(emitter.emitTrait)

          override def getMethodId(m: ArMethod): UIO[BigInt] =
            methodIds.getIdWith(m)(entryBuilders)(emitter.emitMethod)

          override def getSyntheticMethodImport(m: ArMethod): Comp[SyntheticImport] =
            getMemoSyntheticImport(m)(methodImports)(m.owner match {
              case MethodOwner.ByTrait(t) => t.importSpecifier
            })


          override def newSyntheticMethodId: UIO[BigInt] =
            methodIds.claimId.commit
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
      type ExternMethod = ExternMap
    }
  }
}
