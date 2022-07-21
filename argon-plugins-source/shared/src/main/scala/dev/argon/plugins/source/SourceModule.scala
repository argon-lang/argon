package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.module.*
import zio.*
import dev.argon.parser.*
import dev.argon.util.given

object SourceModule {

  def make
    (
      context: Context,
      currentTube: ArTubeC with HasContext[context.type],
      moduleName: ModuleName,
      importer: ImporterC with HasContext[context.type],
      moduleFile: ArgonSourceCodeResource[context.Env, context.Error],
    )
    : UIO[ArModuleC with HasContext[context.type]] =
    val context2: context.type = context
    val moduleName2 = moduleName
    for {
      exportEntries <-
        moduleFile.parsed
          .mapAccumZIO[context.Env, context.Error, context.Comp[Imports[context.type]], Option[ModuleElementC[context.type]]](ZIO.succeed(Map.empty))(loadElement(context, currentTube, moduleName, importer))
          .collect {
            case Some(entry) => entry
          }
          .runCollect
          .memoize

      exportMap <-
        exportEntries.map { exp =>
          exp.groupBy { entry => entry.name }
        }
          .memoize

    } yield new ArModuleC {
      override val context: context2.type = context2
      override val moduleName: ModuleName = moduleName2

      override def allExports(): Comp[Seq[ModuleElement]] = exportEntries
      override def exports(name: IdentifierExpr): Comp[Seq[ModuleElement]] =
        exportMap.map { exp =>
          exp.get(Some(name)).toList.flatten
        }

    }
  end make

  private def loadElement
    (
      context: Context,
      currentTube: ArTubeC with HasContext[context.type],
      moduleName: ModuleName,
      importer: ImporterC with HasContext[context.type],
    )
    (
      imports: context.Comp[Imports[context.type]],
      stmt: Stmt,
    )
    : context.Comp[(context.Comp[Imports[context.type]], Option[ModuleElementC[context.type]])] =
    import context.Comp

    def flattenImportPaths(pathSegment: ImportPathSegment, modulePath: Seq[String])
      : List[(ModulePath, ImportPathSegment.End)] =
      pathSegment match {
        case ImportPathSegment.Cons(id, rest) => flattenImportPaths(rest, modulePath :+ id)
        case ImportPathSegment.Many(segments) =>
          segments.toList.flatMap(flattenImportPaths(_, modulePath))

        case pathSegment: ImportPathSegment.End => List((ModulePath(modulePath), pathSegment))
      }

    type ImportsUngrouped = Seq[(IdentifierExpr, ModuleElementC[context.type])]
    def loadModuleImports
      (
        module: ArModuleC with HasContext[context.type],
        imports: List[ImportPathSegment.End],
        importedIds: Set[IdentifierExpr],
        acc: ImportsUngrouped,
      )
      : Comp[ImportsUngrouped] =
      imports match {
        case ImportPathSegment.Imported(id) :: tail if importedIds.contains(id) =>
          ???

        case ImportPathSegment.Imported(id) :: tail =>
          module.exports(id).flatMap { newImports =>
            loadModuleImports(module, tail, importedIds + id, acc ++ newImports.map((id, _)))
          }

        case ImportPathSegment.Renaming(id, _) :: tail if importedIds.contains(id) =>
          ???

        case ImportPathSegment.Renaming(id, None) :: tail =>
          module.exports(id).flatMap { _ =>
            loadModuleImports(module, tail, importedIds + id, acc)
          }

        case ImportPathSegment.Renaming(id, Some(viewedName)) :: tail =>
          module.exports(id).flatMap { newImports =>
            loadModuleImports(module, tail, importedIds + id, acc ++ newImports.map((viewedName, _)))
          }

        case ImportPathSegment.Wildcard :: _ :: _ =>
          ???

        case ImportPathSegment.Wildcard :: Nil =>
          module.allExports().map { newImports =>
            newImports
              .flatMap { element =>
                element.name match {
                  case Some(name) if importedIds.contains(name) => Seq.empty
                  case Some(name) => Seq((name, element))
                  case None => Seq.empty
                }
              }

          }

        case Nil => ZIO.succeed(acc)
      }

    def loadTubeImports(tube: ArTubeC with HasContext[context.type], pathSegment: ImportPathSegment)
      : Comp[ImportsUngrouped] =
      ZIO.foreach(
        flattenImportPaths(pathSegment, Seq.empty)
          .groupMap(_._1)(_._2)
          .toSeq
      ) { case (modulePath, imports) =>
        tube.module(modulePath).flatMap { module =>
          loadModuleImports(module, imports, Set.empty, Seq.empty)
        }
      }
        .map { _.flatten }

    def loadImport(stmt: ImportStmt)(imports: Imports[context.type]): context.Comp[Imports[context.type]] =
      (stmt match {
        case ImportStmt.Absolute(pathSegment) => loadTubeImports(currentTube, pathSegment)
        case ImportStmt.Relative(upCount, path) =>
          def appendRelative(currentPath: Seq[String], path: ImportPathSegment): ImportPathSegment =
            currentPath match {
              case front :+ last => appendRelative(front, ImportPathSegment.Cons(last, path))
              case _ => path
            }

          def resolveRelative(currentPath: Seq[String], upCount: Int): ImportPathSegment =
            if upCount > 0 then
              currentPath match {
                case front :+ _ => resolveRelative(front, upCount - 1)
                case _ => ???
              }
            else
              appendRelative(currentPath, path)

          loadTubeImports(currentTube, resolveRelative(moduleName.path.ids, upCount))

        case ImportStmt.Tube(tubeName, path) =>
          context.getTube(TubeName(tubeName)).flatMap { tube =>
            loadTubeImports(tube, path)
          }

        case ImportStmt.Member(_) => ???

      }).map { newImports =>
        (imports.toSeq.flatMap { case (id, elements) => elements.map { (id, _) } } ++ newImports)
          .groupMap(_._1)(_._2)
      }

    def loadTrait
      (
        stmt: TraitDeclarationStmt
      )
      : context.Comp[ModuleElementC[context.type]] = ???

    def loadDataConstructor
      (
        stmt: DataConstructorDeclarationStmt
      )
      : context.Comp[ModuleElementC[context.type]] = ???

    def loadClass
      (
        stmt: ClassDeclarationStmt
      )
      : context.Comp[ModuleElementC[context.type]] = ???

    def loadFunction
      (
        stmt: FunctionDeclarationStmt
      )
      : context.Comp[ModuleElementC[context.type]] = ???

    stmt match {
      case stmt: ImportStmt => imports.flatMap(loadImport(stmt)).memoize.map((_, None))
      case stmt: TraitDeclarationStmt => loadTrait(stmt).map { entry => (imports, Some(entry)) }
      case stmt: DataConstructorDeclarationStmt => loadDataConstructor(stmt).map { entry => (imports, Some(entry)) }
      case stmt: ClassDeclarationStmt => loadClass(stmt).map { entry => (imports, Some(entry)) }
      case stmt: FunctionDeclarationStmt => loadFunction(stmt).map { entry => (imports, Some(entry)) }
      case _ => ZIO.fail(DiagnosticError.InvalidTopLevelStatement(stmt))
    }

  end loadElement

}
