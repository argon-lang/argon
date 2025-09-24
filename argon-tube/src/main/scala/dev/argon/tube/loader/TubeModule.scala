package dev.argon.tube.loader

import dev.argon.tube as t
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*
import dev.argon.ast.IdentifierExpr

private[loader] object TubeModule {
  def apply(ctx: Context, elemLoader: ElementLoader & HasContext[ctx.type], tubeMetadata: t.TubeMetadata, mod: t.Module): ctx.Comp[ArModuleC & HasContext[ctx.type]] =
    for
      groups <- MemoCacheStore.make[ctx.Env, ctx.Error, IdentifierExpr, Seq[ModuleExportC[ctx.type]]]
    yield new ArModuleC with LoaderUtils {
      override val context: ctx.type = ctx
      override val elementLoader: elemLoader.type = elemLoader

      override def tubeName: TubeName =
        decodeTubeName(tubeMetadata.name)

      override def path: ModulePath =
        decodeModulePath(mod.path)

      override def allExports(reexportingModules: Set[ModuleName]): Comp[Map[IdentifierExpr, Seq[ModuleExport]]] =
        ZIO.foreach(mod.groups) { group =>
          val name = decodeIdentifier(group.name)
          for
            items <- getExportsImpl(name, group.exports)
          yield name -> items.toList
        }.map(_.toMap)

      override def getExports(reexportingModules: Set[ModuleName])(id: IdentifierExpr): Comp[Option[Seq[ModuleExport]]] =
        ZIO.foreach(mod.groups.find(group => decodeIdentifier(group.name) == id)) { group =>
          getExportsImpl(id, group.exports)
        }

      private def getExportsImpl(id: IdentifierExpr, groupExports: Seq[t.ModuleExport]): Comp[Seq[ModuleExport]] =
        groups.usingCreate(id) { _ =>

          def loadExport(exp: t.ModuleExport): Comp[ModuleExport] =
            exp match {
              case t.ModuleExport.Function(functionId, _) =>
                for
                  f <- elemLoader.getFunction(functionId)
                yield ModuleExportC.Function(f)

              case t.ModuleExport.Record(recordId, _) =>
                for
                  rec <- elemLoader.getRecord(recordId)
                yield ModuleExportC.Record(rec)

              case t.ModuleExport.Enum(enumId, _) =>
                for
                  e <- elemLoader.getEnum(enumId)
                yield ModuleExportC.Enum(e)

              case t.ModuleExport.Trait(traitId, _) =>
                for
                  e <- elemLoader.getTrait(traitId)
                yield ModuleExportC.Trait(e)

              case t.ModuleExport.Instance(instanceId, _) =>
                for
                  i <- elemLoader.getInstance(instanceId)
                yield ModuleExportC.Instance(i)

              case t.ModuleExport.Exported(inner) =>
                for
                  inner <- loadExport(inner)
                yield ModuleExportC.Exported(inner)
            }

          ZIO.foreach(groupExports)(loadExport)
        }

    }

}
