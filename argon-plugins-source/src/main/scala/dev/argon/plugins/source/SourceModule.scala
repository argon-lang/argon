package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.module.*
import dev.argon.compiler.expr.*
import dev.argon.compiler.vtable.VTableBuilder
import zio.*
import dev.argon.parser.*
import dev.argon.util.{*, given}

object SourceModule {

  def make
    (
      context: Context,
      tubeImporter: TubeImporter & HasContext[context.type],
      currentTube: ArTubeC & HasContext[context.type] & HasImplementation[true],
      moduleName: ModuleName,
      moduleFile: ArgonSourceCodeResource[context.Env, context.Error],
      tubeOptions: context.Options,
    )
    : UIO[ArModuleC & HasContext[context.type] & HasImplementation[true]] =
    val context2: context.type = context
    val moduleName2 = moduleName
    for {
      exportEntriesCell <- MemoCell.make[context.Env, context.Error, Seq[ModuleElementC[context.type, true]]]
      exportMapCell <- MemoCell.make[context.Env, context.Error, Map[Option[IdentifierExpr], Seq[ModuleElementC[context.type, true]]]]
      exprConverter <- ExpressionConverter.make(context, tubeImporter)
      vtableBuilder <- VTableBuilder(context, tubeImporter)
    } yield new ArModuleC {
      override type IsImplementation = true

      override val context: context2.type = context2
      override val tube: ArTube & HasImplementation[true] = currentTube
      override val moduleName: ModuleName = moduleName2

      def currentModule: this.type = this

      override def allExports(exportingModules: Set[ArModule]): Comp[Seq[ModuleElement[true]]] =
        exportEntriesCell.get(
          moduleFile.parsed
            .mapAccumZIO[context.Env, context.Error, context.Comp[Imports[context.type]], Seq[ModuleElement[true]]](ZIO.succeed(Map.empty))(loadElement(exportingModules))
            .flattenIterables
            .runCollect
        )


      private def exportsAsImports(exportingModules: Set[ArModule]): Comp[Map[Option[IdentifierExpr], Seq[ModuleElement[true]]]] =
        exportMapCell.get(
          allExports(exportingModules).map { exp =>
            exp
              .map { entry => entry.name -> entry }
              .groupMap(_._1)(_._2)
          }
        )

      override def exports(exportingModules: Set[ArModule])(name: Option[IdentifierExpr]): Comp[Seq[ModuleElement[true]]] =
        exportsAsImports(exportingModules).map { exp =>
          exp.get(name).toList.flatten
        }


      private def importsDropModule(imports: ImportsWithModule[context.type]): Imports[context.type] =
        imports.view.mapValues(_.map(_._2)).toMap

      def loadElement
      (exportingModules: Set[ArModule])
      (
        imports: context.Comp[Imports[context.type]],
        stmt: Stmt,
      )
      : context.Comp[(context.Comp[Imports[context.type]], Seq[ModuleElement[true]])] =
        val selfImports = exportsAsImports(exportingModules)

        def env: exprConverter.Env =
          exprConverter.Env(
            scope = exprConverter.Scope.fromImports(
              selfImports,
              exprConverter.Scope.fromImports(
                imports,
                exprConverter.Scope.empty
              )
            ),
            model = Map.empty,
            knownVarValues = Map.empty,
            implicitSource = exprConverter.ImplicitSource.fromImports(
              selfImports,
              exprConverter.ImplicitSource.fromImports(
                imports,
                exprConverter.ImplicitSource.empty
              )
            ),
          )

        stmt match {
          case stmt: ImportStmt =>
            (
              for
                imports <- imports
                newImports <- loadImport(Set.empty)(stmt)
              yield mergeImports(imports, importsDropModule(newImports))
            )
              .memoize
              .map((_, Seq()))

          case stmt: ExportStmt =>
            loadImport(exportingModules)(stmt.fromImport).map { exportedImports =>
              def exported: Iterator[ModuleElementC.ExportedElement[context.type, true]] =
                for
                  (name, elements) <- exportedImports.iterator
                  (module, element) <- elements
                yield ModuleElementC.ExportedElement(module, name, element)

              (imports, exported.toSeq)
            }
          
          case stmt: TraitDeclarationStmt =>
            for
              modifier <- AccessUtil.parseGlobal(stmt.modifiers)
              owner = OwnedByModuleC[context.type, true](currentModule, stmt.name.value, modifier)
              arTrait <- SourceTrait.make(context)(tubeOptions)(exprConverter)(vtableBuilder)(env)(owner)(stmt)
            yield (imports, Seq(ModuleElementC.TraitElement(arTrait)))

          case stmt: ClassDeclarationStmt =>
            for
              modifier <- AccessUtil.parseGlobal(stmt.modifiers)
              owner = OwnedByModuleC[context.type, true](currentModule, stmt.name.value, modifier)
              arClass <- SourceClass.make(context)(tubeOptions)(exprConverter)(vtableBuilder)(env)(owner)(stmt)
            yield (imports, Seq(ModuleElementC.ClassElement(arClass)))

          case stmt: FunctionDeclarationStmt =>
            for
              modifier <- AccessUtil.parseGlobal(stmt.modifiers)
              owner = OwnedByModuleC[context.type, true](currentModule, stmt.name.value, modifier)
              func <- SourceFunction.make(context)(tubeOptions)(exprConverter)(env)(owner)(stmt)
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

      private type ImportsUngrouped = Seq[(Option[IdentifierExpr], ArModule, ModuleElement[?])]
      private def loadModuleImports
      (exportingModules: Set[ArModule])
      (
        module: ArModuleC & HasContext[context.type],
        imports: List[ImportPathSegment.End],
        importedIds: Set[IdentifierExpr],
        acc: ImportsUngrouped,
      )
      : Comp[ImportsUngrouped] =
        imports match {
          case ImportPathSegment.Imported(id) :: tail if importedIds.contains(id) =>
            ???

          case ImportPathSegment.Imported(id) :: tail =>
            module.namedExports(Set.empty)(id).flatMap { newImports =>
              loadModuleImports(exportingModules)(module, tail, importedIds + id, acc ++ newImports.map((Some(id), module, _)))
            }

          case ImportPathSegment.Renaming(id, _) :: tail if importedIds.contains(id) =>
            ???

          case ImportPathSegment.Renaming(id, None) :: tail =>
            loadModuleImports(exportingModules)(module, tail, importedIds + id, acc)

          case ImportPathSegment.Renaming(id, Some(viewedName)) :: tail =>
            module.namedExports(Set.empty)(id).flatMap { newImports =>
              loadModuleImports(exportingModules)(module, tail, importedIds + id, acc ++ newImports.map((Some(viewedName), module, _)))
            }

          case ImportPathSegment.Wildcard :: _ :: _ =>
            ???

          case ImportPathSegment.Wildcard :: Nil =>
            module.allExports(exportingModules).map { newImports =>
              newImports
                .flatMap { element =>
                  element.name match {
                    case Some(name) if importedIds.contains(name) => Seq.empty
                    case name => Seq((name, module, element))
                  }
                }

            }

          case Nil => ZIO.succeed(acc)
        }


      private def loadTubeImports(exportingModules: Set[ArModule])(tube: ArTubeC & HasContext[context.type], pathSegment: ImportPathSegment)
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

      private def loadImport(exportingModules: Set[ArModule])(stmt: ImportStmt): context.Comp[ImportsWithModule[context.type]] =
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
            tubeImporter.getTube(TubeName(tubeName)).flatMap { tube =>
              loadTubeImports(exportingModules)(tube, path)
            }

          case ImportStmt.Member(_) => ???

        }).map { newImports =>
          newImports.groupMap(_._1) { (_, module, element) => (module, element) }
        }

      private def mergeImports(a: Imports[context.type], b: Imports[context.type]): Imports[context.type] =
        def flattenImports(a: Imports[context.type]): Seq[(Option[IdentifierExpr], ModuleElement[?])] =
          a.toSeq.flatMap { case (id, elements) => elements.map { (id, _) } }

        (flattenImports(a) ++ flattenImports(b))
          .groupMap(_._1)(_._2)
      end mergeImports


    }
  end make


}
