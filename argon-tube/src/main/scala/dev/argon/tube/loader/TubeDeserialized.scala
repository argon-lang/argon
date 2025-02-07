package dev.argon.tube.loader

import dev.argon.tube as t
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*
import zio.stream.*
import zio.stm.*
import dev.argon.ast.IdentifierExpr

private[loader] object TubeDeserialized {
  def apply(ctx: Context, entries: ZStream[ctx.Env, ctx.Error, t.TubeFileEntry])(using tubeImporter: TubeImporter & HasContext[ctx.type]): ZIO[ctx.Env, ctx.Error, ArTubeC & HasContext[ctx.type]] =
    import ctx.Comp

    final case class ElementState(
      functions: TMap[BigInt, t.FunctionDefinition | t.ImportSpecifier],
      records: TMap[BigInt, t.RecordDefinition | t.ImportSpecifier],
      moduleReferences: TMap[BigInt, t.TubeFileEntry.ModuleReference],
      recordFieldReferences: TMap[BigInt, t.TubeFileEntry.RecordFieldReference],
    )

    def createElementLoader(metadata: t.TubeMetadata, state: ElementState): Comp[ElementLoader & HasContext[ctx.type]] =
      for
        tubeCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArTubeC & HasContext[ctx.type]]
        moduleCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArModuleC & HasContext[ctx.type]]
        
        funcCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArFuncC & HasContext[ctx.type]]
        recordCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArRecordC & HasContext[ctx.type]]
        recordFieldCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, RecordFieldC & HasContext[ctx.type]]
      yield new ElementLoader with LoaderUtils {
        override val context: ctx.type = ctx
        override protected def elementLoader: ElementLoader & HasContext[context.type] = this

        override def getTube(id: BigInt): Comp[ArTube] =
          tubeCache.usingCreate(id) { id =>
            
            val tubeName =
              if id == 0 then
                metadata.name
              else if id > 0 && id <= metadata.referencedTubes.size then
                metadata.referencedTubes(id.toInt - 1)
              else
                ???

            tubeImporter.getTube(decodeTubeName(tubeName))
          }

        override def getModule(id: BigInt): Comp[ArModule] =
          moduleCache.usingCreate(id) { id =>
            state.moduleReferences.get(id)
              .commit
              .map { _.get }
              .flatMap { modRef =>
                for
                  refedTube <- getTube(modRef.tubeId)
                yield refedTube.modules(decodeModulePath(modRef.path))
              }
          }
          

        override def getFunction(id: BigInt): Comp[ArFunc] =
          funcCache.usingCreate(id) { id =>
            state.functions.get(id)
              .commit
              .map { _.get }
              .flatMap {
                case importSpec: t.ImportSpecifier =>
                  getImport(importSpec) {
                    case ModuleExportC.Function(f) => f
                  }

                case funcDef: t.FunctionDefinition =>
                  TubeFunction(ctx, this, funcDef)
              }
          }

        override def getRecord(id: BigInt): Comp[ArRecord] =
          recordCache.usingCreate(id) { id =>
            state.records.get(id)
              .commit
              .map { _.get }
              .flatMap {
                case importSpec: t.ImportSpecifier =>
                  getImport(importSpec) {
                    case ModuleExportC.Record(r) => r
                  }

                case recDef: t.RecordDefinition =>
                  TubeRecord(ctx, this, recDef)
              }
          }

        override def getRecordField(id: BigInt): Comp[RecordField] =
          recordFieldCache.usingCreate(id) { id =>
            for
              fieldRef <- state.recordFieldReferences.get(id).commit.map { _.get }
              fieldName = decodeIdentifier(fieldRef.name)
              r <- getRecord(fieldRef.recordId)
              fields <- r.fields
            yield fields.find(_.name == fieldName).get
          }

        private def getImport[A <: DeclarationBase & HasContext[ctx.type]](importSpec: t.ImportSpecifier)(get: PartialFunction[ModuleExport, A]): Comp[A] =
          for
            importSpec2 <- decodeImportSpecifier(importSpec)
            mod <- getModule(importSpec.moduleId)
            exps <- mod.getExports(Set.empty)(importSpec.name.map(decodeIdentifier))
            exp <- ZStream.fromIterable(exps.toList.flatten)
              .map(getExportFrom(get))
              .collectSome
              .filterZIO { exp =>
                exp.importSpecifier.map { _ == importSpec2 }
              }
              .runHead
          yield exp.get

        private def getExportFrom[A](get: PartialFunction[ModuleExport, A])(exp: ModuleExport): Option[A] =
          exp match {
            case ModuleExportC.Exported(exp) => getExportFrom(get)(exp)
            case _ => get.lift(exp)
          }

      }

    def createTube(metadata: t.TubeMetadata, state: ElementState): Comp[ArTubeC & HasContext[ctx.type]] =
      for
        elemLoader <- createElementLoader(metadata, state)

        mods <- ZIO.foreach(metadata.modules)(TubeModule(ctx, elemLoader, metadata, _) : Comp[ArModuleC & HasContext[ctx.type]])
      yield new ArTubeC with LoaderUtils {
        override val context: ctx.type = ctx
        override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

        override val name: TubeName =
          decodeTubeName(metadata.name)
        
        override def modules: Map[ModulePath, ArModule] = ???

        override def referencedTubes: Set[TubeName] =
          metadata.referencedTubes
            .view
            .map(decodeTubeName)
            .toSet
      }

    def loadElements(metadata: t.TubeMetadata): ZChannel[ctx.Env, Nothing, t.TubeFileEntry, Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =

      def createState: USTM[ElementState] =
        for
          functions <- TMap.empty[BigInt, t.FunctionDefinition | t.ImportSpecifier]
          records <- TMap.empty[BigInt, t.RecordDefinition | t.ImportSpecifier]
          moduleReferences <- TMap.empty[BigInt, t.TubeFileEntry.ModuleReference]
          recordFieldReferences <- TMap.empty[BigInt, t.TubeFileEntry.RecordFieldReference]
        yield ElementState(
          functions = functions,
          records = records,
          moduleReferences = moduleReferences,
          recordFieldReferences = recordFieldReferences,
        )

      def iter(state: ElementState): ZChannel[ctx.Env, Nothing, t.TubeFileEntry, Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =
        ZChannelUtils.peel(
          process = {
            case t.TubeFileEntry.Header(_) => ???
            case t.TubeFileEntry.Metadata(_) => ???
            
            case moduleRef: t.TubeFileEntry.ModuleReference =>
              ZChannel.fromZIO(state.moduleReferences.put(moduleRef.moduleId, moduleRef).commit) *> iter(state)
            
            case t.TubeFileEntry.FunctionDefinition(funcDef) =>
              ZChannel.fromZIO(state.functions.put(funcDef.functionId, funcDef).commit) *> iter(state)
            
            case t.TubeFileEntry.FunctionReference(functionId, importSpec) =>
              ZChannel.fromZIO(state.functions.put(functionId, importSpec).commit) *> iter(state)
            
            case t.TubeFileEntry.RecordDefinition(recDef) =>
              ZChannel.fromZIO(state.records.put(recDef.recordId, recDef).commit) *> iter(state)
            
            case t.TubeFileEntry.RecordReference(recordId, importSpec) =>
              ZChannel.fromZIO(state.records.put(recordId, importSpec).commit) *> iter(state)
            
            case recordFieldRef: t.TubeFileEntry.RecordFieldReference =>
              ZChannel.fromZIO(state.recordFieldReferences.put(recordFieldRef.recordFieldId, recordFieldRef).commit) *> iter(state)

          },
          empty = ZChannel.fromZIO(createTube(metadata, state)),
        )

      ZChannel.fromZIO(createState.commit)
        .flatMap(iter)
    end loadElements
      

    def loadTubeFromMetadata: ZChannel[ctx.Env, Nothing, t.TubeFileEntry, Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =
      ZChannelUtils.peel(
        process = {
          case t.TubeFileEntry.Metadata(metadata) =>
            loadElements(metadata)

          case _ => ???
        },
        empty = ???,
      )

    def loadTubeFromVersion: ZChannel[ctx.Env, Nothing, t.TubeFileEntry, Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =
      ZChannelUtils.peel(
        process = {
          case t.TubeFileEntry.Header(header) =>
            loadTubeFromVersion

          case _ => ???
        },
        empty = ???,
      )

    def loadTube: ZChannel[ctx.Env, Nothing, Chunk[t.TubeFileEntry], Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =
      ZChannelUtils.unchunkInput[ctx.Env, t.TubeFileEntry, Any] >>> loadTubeFromVersion

    entries.toChannel.pipeToOrFail(loadTube).run
  end apply
}
