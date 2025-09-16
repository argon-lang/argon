package dev.argon.tube.loader

import dev.argon.tube as t
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*
import zio.stream.*
import zio.stm.*
import dev.argon.ast.IdentifierExpr
import dev.argon.tube.SupportedPlatform
import esexpr.ESExpr

private[loader] object TubeDeserialized {
  def apply(ctx: TubeLoader.TubeLoadContext, entries: ZStream[ctx.Env, ctx.Error, t.TubeFileEntry])(using tubeImporter: TubeImporter & HasContext[ctx.type]): ZIO[ctx.Env, ctx.Error, ArTubeC & HasContext[ctx.type]] =
    import ctx.Comp

    final case class ElementState(
      functions: TMap[BigInt, t.FunctionDefinition | t.ImportSpecifier],
      records: TMap[BigInt, t.RecordDefinition | t.ImportSpecifier],
      moduleReferences: TMap[BigInt, t.TubeFileEntry.ModuleReference],
      recordFieldReferences: TMap[BigInt, t.TubeFileEntry.RecordFieldReference | t.TubeFileEntry.EnumVariantRecordFieldReference],
      enums: TMap[BigInt, t.EnumDefinition | t.ImportSpecifier],
      enumVariantReferences: TMap[BigInt, t.TubeFileEntry.EnumVariantReference],
      traits: TMap[BigInt, t.TraitDefinition | t.ImportSpecifier],
      methods: TMap[BigInt, t.TubeFileEntry.TraitMethodReference],
    )

    def createElementLoader(metadata: t.TubeMetadata, state: ElementState): Comp[ElementLoader & HasContext[ctx.type]] =
      for
        tubeCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArTubeC & HasContext[ctx.type]]
        moduleCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArModuleC & HasContext[ctx.type]]
        
        funcCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArFuncC & HasContext[ctx.type]]

        recordCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArRecordC & HasContext[ctx.type]]
        recordFieldCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, RecordFieldC & HasContext[ctx.type]]

        enumCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArEnumC & HasContext[ctx.type]]
        enumVariantCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, EnumVariantC & HasContext[ctx.type]]

        traitCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArTraitC & HasContext[ctx.type]]
        methodCache <- MemoCacheStore.make[ctx.Env, ctx.Error, BigInt, ArMethodC & HasContext[ctx.type]]
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
            if id < metadata.modules.size then
              getTube(0).map { tube =>
                val module = metadata.modules(id.toInt)
                tube.modules(decodeModulePath(module.path))
              }
            else
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
              .flatMap {
                case Some(item) => ZIO.succeed(item)
                case None => ZIO.fail(TubeFormatException("Invalid function id " + id))
              }
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
              .flatMap {
                case Some(item) => ZIO.succeed(item)
                case None => ZIO.fail(TubeFormatException("Invalid record id " + id))
              }
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
            state.recordFieldReferences.get(id).commit.map { _.get }
              .flatMap {
                case fieldRef: t.TubeFileEntry.RecordFieldReference =>
                  val fieldName = decodeIdentifier(fieldRef.name)
                  for
                    r <- getRecord(fieldRef.recordId)
                    fields <- r.fields
                  yield fields.find(_.name == fieldName).get
                  
                case fieldRef: t.TubeFileEntry.EnumVariantRecordFieldReference =>
                  val fieldName = decodeIdentifier(fieldRef.name)
                  for
                    r <- getEnumVariant(fieldRef.variantId)
                    fields <- r.fields
                  yield fields.find(_.name == fieldName).get
              }
          }

        override def getEnum(id: BigInt): Comp[ArEnum] =
          enumCache.usingCreate(id) { id =>
            state.enums.get(id)
              .commit
              .flatMap {
                case Some(item) => ZIO.succeed(item)
                case None => ZIO.fail(TubeFormatException("Invalid enum id " + id))
              }
              .flatMap {
                case importSpec: t.ImportSpecifier =>
                  getImport(importSpec) {
                    case ModuleExportC.Enum(e) => e
                  }

                case enumDef: t.EnumDefinition =>
                  TubeEnum(ctx, this, enumDef)
              }
          }

        override def getEnumVariant(id: BigInt): Comp[EnumVariant] =
          enumVariantCache.usingCreate(id) { id =>
            for
              enumVariant <- state.enumVariantReferences.get(id).commit.map { _.get }
              fieldName = decodeIdentifier(enumVariant.name)
              e <- getEnum(enumVariant.enumId)
              variants <- e.variants
            yield variants.find(_.name == fieldName).get
          }

        override def getTrait(id: BigInt): Comp[ArTrait] =
          traitCache.usingCreate(id) { id =>
            state.traits.get(id)
              .commit
              .flatMap {
                case Some(item) => ZIO.succeed(item)
                case None => ZIO.fail(TubeFormatException("Invalid trait id " + id))
              }
              .flatMap {
                case importSpec: t.ImportSpecifier =>
                  getImport(importSpec) {
                    case ModuleExportC.Trait(t) => t
                  }

                case traitDef: t.TraitDefinition =>
                  TubeTrait(ctx, this, traitDef)
              }
          }

        override def getMethod(id: BigInt): Comp[ArMethod] =
          methodCache.usingCreate(id) { id =>
            state.methods.get(id)
              .commit
              .flatMap {
                case Some(item) => ZIO.succeed(item)
                case None => ZIO.fail(TubeFormatException("Invalid method id " + id))
              }
              .flatMap {
                case methodRef: t.TubeFileEntry.TraitMethodReference =>
                  val name = decodeIdentifier(methodRef.name)

                  decodeErasedSignature(methodRef.signature)
                    .flatMap { erasedSig =>
                      ZStream.fromZIO(getTrait(methodRef.traitId))
                        .mapZIO(_.methods)
                        .flatMap(ZStream.fromIterable(_))
                        .filter { m => m.name == name }
                        .filterZIO { m =>
                          for
                            sig <- m.signature
                            sig <- SignatureEraser(context).eraseSignature(sig)
                          yield sig == erasedSig
                        }
                        .runHead
                    }
                    .flatMap {
                      case Some(item) => ZIO.succeed(item)
                      case None => ZIO.fail(TubeFormatException("Could not find method " + methodRef))
                    }
              }
          }


        private def getImport[A <: DeclarationBase & HasContext[ctx.type]](importSpec: t.ImportSpecifier)(get: PartialFunction[ModuleExport, A]): Comp[A] =
          importSpec match {
            case importSpec: t.ImportSpecifier.Global =>
              for
                importSpec2 <- decodeImportSpecifier(importSpec)
                mod <- getModule(importSpec.moduleId)
                exps <- mod.getExports(Set.empty)(decodeIdentifier(importSpec.name))
                exp <- ZStream.fromIterable(exps.toList.flatten)
                  .map(getExportFrom(get))
                  .collectSome
                  .filterZIO { exp =>
                    exp.importSpecifier.map { _ == importSpec2 }
                  }
                  .runHead
              yield exp.get
          }

        private def getExportFrom[A](get: PartialFunction[ModuleExport, A])(exp: ModuleExport): Option[A] =
          exp match {
            case ModuleExportC.Exported(exp) => getExportFrom(get)(exp)
            case _ => get.lift(exp)
          }

      }

    def createTube(tubeMetadata: t.TubeMetadata, state: ElementState): Comp[ArTubeC & HasContext[ctx.type]] =
      for
        elemLoader <- createElementLoader(tubeMetadata, state)

        mods <- ZIO.foreach(tubeMetadata.modules)(TubeModule(ctx, elemLoader, tubeMetadata, _) : Comp[ArModuleC & HasContext[ctx.type]])
      yield new ArTubeC with LoaderUtils {
        override val context: ctx.type = ctx
        override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

        override val name: TubeName =
          decodeTubeName(tubeMetadata.name)

        override def metadata: context.implementations.TubeMetadata =
          (tubeMetadata.platforms, tubeMetadata.platformMetadata.dict)

        override val modules: Map[ModulePath, ArModule] =
          mods.iterator
            .map { mod => mod.path -> mod }
            .toMap

        override def referencedTubes: Set[TubeName] =
          tubeMetadata.referencedTubes
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
          recordFieldReferences <- TMap.empty[BigInt, t.TubeFileEntry.RecordFieldReference | t.TubeFileEntry.EnumVariantRecordFieldReference]
          enums <- TMap.empty[BigInt, t.EnumDefinition | t.ImportSpecifier]
          enumVariantReferences <- TMap.empty[BigInt, t.TubeFileEntry.EnumVariantReference]
          traits <- TMap.empty[BigInt, t.TraitDefinition | t.ImportSpecifier]
          methods <- TMap.empty[BigInt, t.TubeFileEntry.TraitMethodReference]
        yield ElementState(
          functions = functions,
          records = records,
          moduleReferences = moduleReferences,
          recordFieldReferences = recordFieldReferences,
          enums = enums,
          enumVariantReferences = enumVariantReferences,
          traits = traits,
          methods = methods,
        )

      def iter(state: ElementState): ZChannel[ctx.Env, Nothing, t.TubeFileEntry, Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =
        ZChannelUtils.peel(
          process = {
            case t.TubeFileEntry.Header(_) => ZChannel.fail(TubeFormatException("Unexpected tube header"))
            case t.TubeFileEntry.Metadata(_) => ZChannel.fail(TubeFormatException("Unexpected tube metadata"))
            
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

            case t.TubeFileEntry.EnumDefinition(enumDef) =>
              ZChannel.fromZIO(state.enums.put(enumDef.enumId, enumDef).commit) *> iter(state)

            case t.TubeFileEntry.EnumReference(enumId, importSpec) =>
              ZChannel.fromZIO(state.enums.put(enumId, importSpec).commit) *> iter(state)

            case enumVariantRef: t.TubeFileEntry.EnumVariantReference =>
              ZChannel.fromZIO(
                state.enumVariantReferences.put(enumVariantRef.variantId, enumVariantRef).commit
              ) *> iter(state)

            case recordFieldRef: t.TubeFileEntry.EnumVariantRecordFieldReference =>
              ZChannel.fromZIO(state.recordFieldReferences.put(recordFieldRef.recordFieldId, recordFieldRef).commit) *> iter(state)

            case t.TubeFileEntry.TraitDefinition(traitDef) =>
              ZChannel.fromZIO(state.traits.put(traitDef.traitId, traitDef).commit) *> iter(state)

            case t.TubeFileEntry.TraitReference(traitId, importSpec) =>
              ZChannel.fromZIO(state.traits.put(traitId, importSpec).commit) *> iter(state)

            case methodRef: t.TubeFileEntry.TraitMethodReference =>
              ZChannel.fromZIO(state.methods.put(methodRef.methodId, methodRef).commit) *> iter(state)
              
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

          case entry => ZChannel.fail(TubeFormatException(s"Unexpected entry (${entry.getClass}). Second entry must be metadata" ))
        },
        empty = ZChannel.fail(TubeFormatException(s"Unexpected end of file. Second entry must be metadata" ))
      )

    def loadTubeFromVersion: ZChannel[ctx.Env, Nothing, t.TubeFileEntry, Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =
      ZChannelUtils.peel(
        process = {
          case t.TubeFileEntry.Header(header) =>
            loadTubeFromMetadata

          case entry => ZChannel.fail(TubeFormatException(s"Unexpected entry (${entry.getClass}). Second entry must be header" ))
        },
        empty = ZChannel.fail(TubeFormatException(s"Unexpected end of file. Second entry must be metadata" ))
      )

    def loadTube: ZChannel[ctx.Env, Nothing, Chunk[t.TubeFileEntry], Any, ctx.Error, Nothing, ArTubeC & HasContext[ctx.type]] =
      ZChannelUtils.unchunkInput[ctx.Env, t.TubeFileEntry, Any] >>> loadTubeFromVersion

    entries.toChannel.pipeToOrFail(loadTube).run
  end apply
}
