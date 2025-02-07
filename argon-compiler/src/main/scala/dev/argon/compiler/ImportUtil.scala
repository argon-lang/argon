package dev.argon.compiler

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import zio.*
import zio.stm.*
import cats.*
import cats.implicits.given
import zio.interop.catz.core.given
import dev.argon.util.{*, given}

object ImportUtil {
  def getModuleExports
  (context: Context)
  (reexportingModules: Set[ModuleName])
  (currentTube: TubeName, currentModule: ModulePath)
  (imp: WithSource[ast.ImportStmt])
  (using tubeImporter: TubeImporter & HasContext[context.type])
  : context.Comp[Map[IdentifierExpr, Seq[ModuleExportC[context.type]]]] =
    import context.Comp
    def importName(tubeName: TubeName, path: ModulePath, name: WithSource[IdentifierExpr]): Comp[Seq[ModuleExportC[context.type]]] =
      tubeImporter.getTube(tubeName).flatMap { tube =>
        tube.modules.get(path) match {
          case Some(module) =>
            module.getExports(reexportingModules)(Some(name.value))
              .map(_.getOrElse(???))

          case None =>
            ErrorLog.logError(CompilerError.UnknownModule(tubeName, path, name.location))
              .as(Seq.empty)
        }
      }


    def importWildcard(tubeName: TubeName, path: ModulePath, location: Location[FilePosition], excludedNames: Set[IdentifierExpr]): Comp[Map[IdentifierExpr, Seq[ModuleExportC[context.type]]]] =
      tubeImporter.getTube(tubeName).flatMap { tube =>
        tube.modules.get(path) match {
          case Some(module) =>
            module.allExports(reexportingModules)
              .map { exportMap =>
                exportMap.collect {
                  case (Some(k), v) => k -> v
                }
              }

          case None =>
            ErrorLog.logError(CompilerError.UnknownModule(tubeName, path, location))
              .as(Map.empty)
        }
      }

    def importFromStmt(tubeName: TubeName, path: ModulePath, seg: ast.ImportPathSegment, excludedNames: TSet[IdentifierExpr]): Comp[Map[IdentifierExpr, Seq[ModuleExportC[context.type]]]] =
      seg match {
        case ast.ImportPathSegment.Cons(id, subPath) =>
          importFromStmtNoExcludes(tubeName, ModulePath(path.parts :+ id), subPath)

        case ast.ImportPathSegment.Many(segs) =>
          Foldable[Seq].collectFold(segs)(importFromStmt(tubeName, path, _, excludedNames))

        case ast.ImportPathSegment.Renaming(WithLocation(importing, _), WithLocation(None, _)) =>
          excludedNames.put(importing).commit
            .as(Map.empty)

        case ast.ImportPathSegment.Renaming(importing, WithLocation(Some(viewedName), _)) =>
          excludedNames.put(importing.value).commit *>
            importName(tubeName, path, importing)
              .map { elements => Map(viewedName -> elements) }

        case ast.ImportPathSegment.Imported(id) =>
          excludedNames.put(id.value).commit *>
            importName(tubeName, path, id)
              .map { elements => Map(id.value -> elements) }

        case ast.ImportPathSegment.Wildcard(location) =>
          excludedNames.toSet.commit.flatMap { excl =>
            importWildcard(tubeName, path, location, excl)
          }
      }

    def importFromStmtNoExcludes(tubeName: TubeName, path: ModulePath, seg: ast.ImportPathSegment): Comp[Map[IdentifierExpr, Seq[ModuleExportC[context.type]]]] =
      TSet.empty[IdentifierExpr].commit.flatMap { excludedNames =>
        importFromStmt(tubeName, path, seg, excludedNames)
      }

    imp.value match {
      case ast.ImportStmt.Absolute(path) =>
        importFromStmtNoExcludes(currentTube, ModulePath(Seq.empty), path)

      case ast.ImportStmt.Relative(upCount, path) =>
        def basePath(upCount: Int, path: ModulePath): ModulePath =
          if upCount == 0 then
            path
          else
            assert(upCount > 0)
            path.parts match {
              case init :+ _ =>
                basePath(upCount - 1, ModulePath(init))

              case _ => ???
            }
          end if

        importFromStmtNoExcludes(currentTube, basePath(upCount, currentModule), path)

      case ast.ImportStmt.Tube(tubeName, path) =>
        importFromStmtNoExcludes(TubeName(tubeName), ModulePath(Seq.empty), path)

      case ast.ImportStmt.Member(memberPath) => ???
    }
  end getModuleExports
}
