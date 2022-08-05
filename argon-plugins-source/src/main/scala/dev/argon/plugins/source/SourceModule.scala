package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.module.*
import dev.argon.compiler.expr.*
import dev.argon.plugin.ImporterC
import zio.*
import dev.argon.parser.*
import dev.argon.util.{*, given}

object SourceModule {

  def make
    (
      context: Context,
      currentTube: ArTubeC with HasContext[context.type],
      moduleName: ModuleName,
      moduleFile: ArgonSourceCodeResource[context.Env, context.Error],
    )
    : UIO[ArModuleC with HasContext[context.type] with HasDeclaration[true]] =
    val context2: context.type = context
    val moduleName2 = moduleName
    for {
      exportEntriesCell <- MemoCell.make[context.Env, context.Error, Seq[ModuleElementC[context.type, true]]]
      exportMapCell <- MemoCell.make[context.Env, context.Error, Map[IdentifierExpr, Seq[ModuleElementC[context.type, true]]]]
      exprConverter <- ExpressionConverter.make(context)
    } yield new ArModuleC {
      override type IsDeclaration = true

      override val context: context2.type = context2
      override val tube: ArTube = currentTube
      override val moduleName: ModuleName = moduleName2

      def currentModule: this.type = this

      override def allExports(exportingModules: Set[ArModule]): Comp[Seq[ModuleElement[true]]] =
        exportEntriesCell.get(
          moduleFile.parsed
            .mapAccumZIO[context.Env, context.Error, context.Comp[Imports[context.type]], Seq[ModuleElement[true]]](ZIO.succeed(Map.empty))(loadElement(exportingModules))
            .flattenIterables
            .runCollect
        )


      private def exportsAsImports(exportingModules: Set[ArModule]): Comp[Map[IdentifierExpr, Seq[ModuleElement[true]]]] =
        exportMapCell.get(
          allExports(exportingModules).map { exp =>
            exp
              .map { entry => entry.name -> entry }
              .collect {
                case (Some(name), entry) => name -> entry
              }
              .groupMap(_._1)(_._2)
          }
        )

      override def exports(exportingModules: Set[ArModule])(name: IdentifierExpr): Comp[Seq[ModuleElement[true]]] =
        exportsAsImports(exportingModules).map { exp =>
          exp.get(name).toList.flatten
        }



      def loadElement
      (exportingModules: Set[ArModule])
      (
        imports: context.Comp[Imports[context.type]],
        stmt: Stmt,
      )
      : context.Comp[(context.Comp[Imports[context.type]], Seq[ModuleElement[true]])] =
        def env: exprConverter.Env =
          exprConverter.Env(
            scope = exprConverter.Scope.fromImports(
              exportsAsImports(exportingModules),
              exprConverter.Scope.fromImports(
                imports,
                exprConverter.Scope.empty
              )
            ),
            model = Map.empty
          )

        stmt match {
          case stmt: ImportStmt => imports.flatMap(loadImport(Set.empty)(stmt)).memoize.map((_, Seq()))

          case stmt: ExportStmt =>
            loadImport(exportingModules)(stmt.fromImport)(Map.empty).map { exportedImports =>
              (imports, exportedImports.values.flatten.map(ModuleElementC.ExportedElement.apply).toSeq)
            }
          
          case stmt: TraitDeclarationStmt =>
            for
              modifier <- AccessUtil.parseGlobal(stmt.modifiers)
              owner = OwnedByModuleC[context.type](currentModule, stmt.name, modifier)
              arTrait <- SourceTrait.make(context)(exprConverter)(env)(owner)(stmt)
            yield (imports, Seq(ModuleElementC.TraitElement(arTrait)))

          case stmt: ClassDeclarationStmt =>
            for
              modifier <- AccessUtil.parseGlobal(stmt.modifiers)
              owner = OwnedByModuleC[context.type](currentModule, stmt.name.value, modifier)
              arClass <- SourceClass.make(context)(exprConverter)(env)(owner)(stmt)
            yield (imports, Seq(ModuleElementC.ClassElement(arClass)))

          case stmt: FunctionDeclarationStmt =>
            for
              modifier <- AccessUtil.parseGlobal(stmt.modifiers)
              owner = OwnedByModuleC[context.type](currentModule, stmt.name, modifier)
              func <- SourceFunction.make(context)(exprConverter)(env)(owner)(stmt)
            yield (imports, Seq(ModuleElementC.FunctionElement(func)))

          case _ => ZIO.fail(DiagnosticError.InvalidTopLevelStatement(stmt))
        }
      end loadElement

      private def flattenImportPaths(pathSegment: ImportPathSegment, modulePath: Seq[String])
      : List[(ModulePath, ImportPathSegment.End)] =
        pathSegment match {
          case ImportPathSegment.Cons(id, rest) => flattenImportPaths(rest, modulePath :+ id)
          case ImportPathSegment.Many(segments) =>
            segments.toList.flatMap(flattenImportPaths(_, modulePath))

          case pathSegment: ImportPathSegment.End => List((ModulePath(modulePath), pathSegment))
        }

      private type ImportsUngrouped = Seq[(IdentifierExpr, ModuleElement[?])]
      private def loadModuleImports
      (exportingModules: Set[ArModule])
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
            module.exports(Set.empty)(id).flatMap { newImports =>
              loadModuleImports(exportingModules)(module, tail, importedIds + id, acc ++ newImports.map((id, _)))
            }

          case ImportPathSegment.Renaming(id, _) :: tail if importedIds.contains(id) =>
            ???

          case ImportPathSegment.Renaming(id, None) :: tail =>
            loadModuleImports(exportingModules)(module, tail, importedIds + id, acc)

          case ImportPathSegment.Renaming(id, Some(viewedName)) :: tail =>
            module.exports(Set.empty)(id).flatMap { newImports =>
              loadModuleImports(exportingModules)(module, tail, importedIds + id, acc ++ newImports.map((viewedName, _)))
            }

          case ImportPathSegment.Wildcard :: _ :: _ =>
            ???

          case ImportPathSegment.Wildcard :: Nil =>
            module.allExports(exportingModules).map { newImports =>
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


      private def loadTubeImports(exportingModules: Set[ArModule])(tube: ArTubeC with HasContext[context.type], pathSegment: ImportPathSegment)
      : Comp[ImportsUngrouped] =
        ZIO.foreach(
          flattenImportPaths(pathSegment, Seq.empty)
            .groupMap(_._1)(_._2)
            .toSeq
        ) { case (modulePath, imports) =>
          tube.module(modulePath).flatMap { module =>
            loadModuleImports(exportingModules)(module, imports, Set.empty, Seq.empty)
          }
        }
          .map { _.flatten }

      private def loadImport(exportingModules: Set[ArModule])(stmt: ImportStmt)(imports: Imports[context.type]): context.Comp[Imports[context.type]] =
        (stmt match {
          case ImportStmt.Absolute(pathSegment) => loadTubeImports(exportingModules)(currentTube, pathSegment)
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

            loadTubeImports(exportingModules)(currentTube, resolveRelative(moduleName.path.ids, upCount + (if moduleName.path.ids.nonEmpty then 1 else 0)))

          case ImportStmt.Tube(tubeName, path) =>
            context.getTube(TubeName(tubeName)).flatMap { tube =>
              loadTubeImports(exportingModules)(tube, path)
            }

          case ImportStmt.Member(_) => ???

        }).map { newImports =>
          (imports.toSeq.flatMap { case (id, elements) => elements.map { (id, _) } } ++ newImports)
            .groupMap(_._1)(_._2)
        }


    }
  end make


}
