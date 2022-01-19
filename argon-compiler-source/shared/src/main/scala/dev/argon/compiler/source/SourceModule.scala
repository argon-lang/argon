package dev.argon.compiler.source

import dev.argon.compiler.*
import dev.argon.compiler.tube.*
import dev.argon.compiler.module.*
import zio.*
import dev.argon.parser.*

object SourceModule {
  def make(
    context: Context,
    moduleName: ModuleName,
    importer: ImporterC with HasContext[context.type],
    moduleFile: SourceCodeResource,
  ): UIO[ArModuleC with HasContext[context.type]] =
    val context2: context.type = context
    val moduleName2 = moduleName
    for {
      exportEntries <-
        moduleFile.parsed
          .mapAccumZIO(IO.succeed(Map.empty))(loadElement(context, importer))
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

  private def loadElement(
    context: Context,
    importer: ImporterC with HasContext[context.type]
  )(
    imports: context.Comp[Imports[context.type]],
    stmt: Stmt
  ): context.Comp[(context.Comp[Imports[context.type]], Option[ModuleElementC[context.type]])] =
    stmt match {
      case stmt: ImportStmt => IO.succeed(imports.flatMap(loadImport(context, importer, _, stmt)), None)
      case stmt: TraitDeclarationStmt => loadTrait(context, importer, imports, stmt).map { entry => (imports, Some(entry)) }
      case stmt: DataConstructorDeclarationStmt => loadDataConstructor(context, importer, imports, stmt).map { entry => (imports, Some(entry)) }
      case stmt: ClassDeclarationStmt => loadClass(context, importer, imports, stmt).map { entry => (imports, Some(entry)) }
      case stmt: FunctionDeclarationStmt => loadFunction(context, importer, imports, stmt).map { entry => (imports, Some(entry)) }
      case _ => IO.fail(DiagnosticError.InvalidTopLevelStatement(stmt))
    }

  private def loadImport(
    context: Context,
    importer: ImporterC with HasContext[context.type],
    imports: Imports[context.type],
    stmt: ImportStmt
  ): context.Comp[Imports[context.type]] = ???

  private def loadTrait(
    context: Context,
    importer: ImporterC with HasContext[context.type],
    imports: context.Comp[Imports[context.type]],
    stmt: TraitDeclarationStmt
  ): context.Comp[ModuleElementC[context.type]] = ???

  private def loadDataConstructor(
    context: Context,
    importer: ImporterC with HasContext[context.type],
    imports: context.Comp[Imports[context.type]],
    stmt: DataConstructorDeclarationStmt
  ): context.Comp[ModuleElementC[context.type]] = ???

  private def loadClass(
    context: Context,
    importer: ImporterC with HasContext[context.type],
    imports: context.Comp[Imports[context.type]],
    stmt: ClassDeclarationStmt
  ): context.Comp[ModuleElementC[context.type]] = ???

  private def loadFunction(
    context: Context,
    importer: ImporterC with HasContext[context.type],
    imports: context.Comp[Imports[context.type]],
    stmt: FunctionDeclarationStmt
  ): context.Comp[ModuleElementC[context.type]] = ???


}
